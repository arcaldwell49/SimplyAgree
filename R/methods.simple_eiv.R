#' Methods for simple_eiv objects
#'
#' Methods defined for objects returned from error-in-variables models (e.g., dem_reg, pb_reg).
#'
#' @param object,x object of class \code{simple_eiv} from dem_reg or pb_reg function.
#' @param ... further arguments passed through.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the EIV regression model.}
#'   \item{\code{summary}}{Prints detailed summary.}
#'   \item{\code{plot}}{Returns a plot of the regression line and data.}
#'   \item{\code{check}}{Returns plots of residuals.}
#'   \item{\code{plot_joint}}{Returns plot of joint confidence region (Deming only).}
#'   \item{\code{predict}}{Predicts Y values for new X values.}
#'   \item{\code{fitted}}{Extracts fitted values.}
#'   \item{\code{residuals}}{Extracts residuals.}
#'   \item{\code{coef}}{Extracts model coefficients.}
#'   \item{\code{vcov}}{Extracts variance-covariance matrix.}
#'   \item{\code{formula}}{Extracts model formula.}
#'   \item{\code{model.frame}}{Extracts the model frame.}
#' }
#'
#' @name simple_eiv-methods

#' @rdname simple_eiv-methods
#' @method print simple_eiv
#' @export

print.simple_eiv <- function(x, ...) {
  # Determine method type
  is_passing_bablok <- !is.null(x$method) && grepl("Passing-Bablok", x$method)

  if (is_passing_bablok) {
    header <- paste0(x$method, " with ", x$conf.level * 100, "% C.I.")
  } else if (!is.null(x$weighted) && x$weighted == TRUE) {
    header <- paste0("Weighted Deming Regression with ", x$conf.level * 100, "% C.I.")
  } else {
    header <- paste0("Deming Regression with ", x$conf.level * 100, "% C.I.")
  }

  cat(header, "\n\n")
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients, digits = 4)
  cat("\n")

  # Passing-Bablok specific tests
  if (is_passing_bablok) {
    if (!is.null(x$kendall_test)) {
      cat("Kendall's Tau Test (H0: tau = 0):\n")
      cat(sprintf("  Tau:       %.4f\n", x$kendall_test$tau))
      cat(sprintf("  p-value:   %.4f\n", x$kendall_test$p_value))
      cat("\n")
    }

    if (!is.null(x$cusum_test)) {
      cat("CUSUM Linearity Test:\n")
      cat(sprintf("  Test stat: %.4f\n", x$cusum_test$test_statistic))
      cat(sprintf("  Linear:    %s\n", ifelse(x$cusum_test$linear, "Yes", "No")))
      cat("\n")
    }
  }

  # Deming joint region test
  if (!is_passing_bablok && !is.null(x$joint_test)) {
    cat("Joint Confidence Region Test (H0: slope=1, intercept=0):\n")
    cat(sprintf("  Identity enclosed: %s (p = %.4f)\n",
                ifelse(x$joint_test$is_enclosed, "Yes", "No"),
                x$joint_test$p_value))
  }

  invisible(x)
}

#' @rdname simple_eiv-methods
#' @method summary simple_eiv
#' @export

summary.simple_eiv <- function(object, ...) {
  # Determine method type
  is_passing_bablok <- !is.null(object$method) && grepl("Passing-Bablok", object$method)

  if (is_passing_bablok) {
    header <- paste0(object$method, " with ", object$conf.level * 100, "% C.I.")
  } else if (!is.null(object$weighted) && object$weighted == TRUE) {
    header <- paste0("Weighted Deming Regression with ", object$conf.level * 100, "% C.I.")
  } else {
    header <- paste0("Deming Regression with ", object$conf.level * 100, "% C.I.")
  }

  cat(header, "\n\n")
  cat("Call:\n")
  print(object$call)
  cat("\n")

  cat("Residuals:\n")
  print(summary(object$residuals))
  cat("\n")

  cat("Coefficients:\n")
  print(object$model_table, digits = 4)
  cat("\n")

  cat(sprintf("Residual standard error: %.4f on %d degrees of freedom\n",
              sd(object$residuals), object$df.residual))

  # Error ratio output
  if (!is.null(object$error.ratio)) {
    cat(sprintf("Error variance ratio (lambda): %.4f\n", object$error.ratio))
  }

  # Passing-Bablok specific tests
  if (is_passing_bablok) {
    cat("\n")
    if (!is.null(object$kendall_test)) {
      cat("Kendall's Tau Test (H0: tau = 0):\n")
      cat(sprintf("  Tau: %.4f (95%% CI: [%.4f, %.4f])\n",
                  object$kendall_test$tau,
                  object$kendall_test$lower,
                  object$kendall_test$upper))
      cat(sprintf("  Z:   %.4f, p = %.4f\n",
                  object$kendall_test$z_statistic,
                  object$kendall_test$p_value))
    }

    if (!is.null(object$cusum_test)) {
      cat("\n")
      cat("CUSUM Linearity Test:\n")
      cat(sprintf("  Test stat: %.4f, p ≈ %.3f\n",
                  object$cusum_test$test_statistic,
                  object$cusum_test$p_value))
      cat(sprintf("  Linear:    %s\n", ifelse(object$cusum_test$linear, "Yes", "No")))
    }

    # Bootstrap info
    if (!is.null(object$nboot) && object$nboot > 0) {
      cat("\n")
      cat(sprintf("Bootstrap CIs based on %d resamples\n", object$nboot))
    }
  }

  # Deming joint test
  if (!is_passing_bablok && !is.null(object$joint_test)) {
    cat("\n")
    cat("Joint Confidence Region Test (H0: slope=1, intercept=0):\n")
    cat(sprintf("  Mahalanobis distance: %.4f\n", object$joint_test$mahalanobis_distance))
    cat(sprintf("  Chi-square critical:  %.4f\n", object$joint_test$chi2_critical))
    cat(sprintf("  Identity enclosed:    %s\n",
                ifelse(object$joint_test$is_enclosed, "Yes", "No")))
    cat(sprintf("  p-value:             %.4f\n", object$joint_test$p_value))
  }

  invisible(object)
}

#' @rdname simple_eiv-methods
#' @param x_name Name for x-axis label (optional).
#' @param y_name Name for y-axis label (optional).
#' @param interval Type of interval to display. Options are "none" (default), "confidence", or "prediction".
#' @param level Confidence level for intervals (default uses the model's conf.level).
#' @param n_points Number of points to use for drawing smooth interval bands (default = 100).
#' @method plot simple_eiv
#' @importFrom patchwork plot_annotation
#' @export

plot.simple_eiv <- function(x,
                            x_name = NULL,
                            y_name = NULL,
                            interval = c("none", "confidence"),
                            level = NULL,
                            n_points = 100,
                            ...) {

  interval <- match.arg(interval)

  if (is.null(level)) {
    level <- x$conf.level
  }

  if (is.null(x_name)) {
    x_name <- names(x$model)[2]
  }
  if (is.null(y_name)) {
    y_name <- names(x$model)[1]
  }

  df <- data.frame(x = x$x_vals, y = x$y_vals)
  scalemin <- min(c(df$x, df$y), na.rm = TRUE)
  scalemax <- max(c(df$x, df$y), na.rm = TRUE)

  slp <- x$coefficients[2]
  int <- x$coefficients[1]
  tmp.lm <- data.frame(the_int = int, the_slope = slp)

  # Base plot
  p1 <- ggplot(df, aes(x = x, y = y)) +
    geom_point(na.rm = TRUE) +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "solid",
                color = "black") +
    geom_abline(
      data = tmp.lm,
      aes(intercept = the_int, slope = the_slope),
      linetype = "dashed",
      color = "red"
    )

  # Add confidence or prediction bands if requested
  if (interval != "none") {
    # Check if method supports intervals
    is_passing_bablok <- !is.null(x$method) && grepl("Passing-Bablok", x$method)

    if (is_passing_bablok) {
      warning("Confidence/prediction intervals are not available for Passing-Bablok regression. Showing point estimates only.")
    } else {
      # Create sequence of x values for smooth bands
      x_seq <- seq(scalemin, scalemax, length.out = n_points)
      newdata <- data.frame(x = x_seq)
      names(newdata) <- x_name

      # Compute intervals
      pred_result <- predict(x, newdata = newdata, interval = interval, level = level)

      # Create data frame for ribbon
      band_df <- data.frame(
        x = x_seq,
        fit = pred_result$fit,
        lwr = pred_result$lwr,
        upr = pred_result$upr
      )

      # Add ribbon to plot
      interval_label <- ifelse(interval == "confidence", "Confidence", "Prediction")
      p1 <- p1 +
        geom_ribbon(data = band_df,
                    aes(x = x, ymin = lwr, ymax = upr),
                    fill = "red",
                    alpha = 0.2,
                    inherit.aes = FALSE) +
        labs(caption = sprintf("%.0f%% %s interval shown",
                               level * 100,
                               interval_label))

      scalemin = min(band_df$lwr, na.rm = TRUE)
      scalemax = max(band_df$upr, na.rm = TRUE)
    }
  }

  # Finalize plot
  p1 <- p1 +
    xlab(paste0("Method: ", x_name)) +
    xlim(scalemin, scalemax) +
    ylim(scalemin, scalemax) +
    ylab(paste0("Method: ", y_name)) +
    coord_fixed(ratio = 1 / 1) +
    theme_bw()

  return(p1)
}

#' @rdname simple_eiv-methods
#' @method check simple_eiv
#' @export

check.simple_eiv <- function(x) {

  df = model.frame(x)
  colnames(df) = c("y", "x")
  b0 = x$model_table$coef[1]
  b1 = x$model_table$coef[2]
  w_i = x$weights
  error.ratio = x$error.ratio
  d_i = df$y - (b0+b1*df$x)
  x_hat = df$x + (error.ratio*b1*d_i)/(1+error.ratio*b1^2)
  y_hat = df$y - (d_i)/(1+error.ratio*b1^2)
  res_x = df$x - x_hat
  res_y = df$y - y_hat
  d_sign = ifelse(d_i >= 0, 1, -1)
  opt_res = d_sign * sqrt(w_i*res_x^2 + w_i * error.ratio * res_y^2)
  avg_both = ((x_hat + y_hat )/ 2)

  mod <- lm(opt_res/d_sign ~ avg_both)

  SS <- anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS / 2
  ### Breusch-Pagan Test
  p_val_het <- pchisq(Chisq, df = 1, lower.tail = FALSE)
  df1 = data.frame(x = avg_both,
                   y = opt_res/d_sign)
  p1 = ggplot(df1,
              aes(x=x,
                  y=y)) +
    geom_point() +
    geom_smooth(se = TRUE,
                method = "loess",
                linewidth = .8,
                color ="#3aaf85",
                formula = y~x) +
    labs(y = "|Optimized Residuals|",
         x = "Average of Both Estimated Values",
         title = "Homogeneity of Residuals",
         subtitle = "Reference line should be flat and horizontal",
         caption = paste0("Heteroskedasticity", " \n",
                          "Breusch-Pagan Test: p = ",
                          signif(p_val_het,4))) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )

  dat_norm <- na.omit(data.frame(y = opt_res))
  norm_test = shapiro.test(opt_res)
  norm_text = "Shapiro-Wilk Test"
  p2 = plot_qq(x = dat_norm) +
    labs(caption = paste0("Normality", " \n",
                          norm_text, ": p = ",
                          signif(norm_test$p.value,4)))+
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
  wrap_plots(p2, p1, ncol = 2) & plot_annotation(
    theme = theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    ))
}

#' @rdname simple_eiv-methods
#' @export

plot_joint <- function(x, ...) {
  UseMethod("plot_joint")
}

#' @rdname simple_eiv-methods
#' @param object object of class \code{simple_eiv} for plot_joint method
#' @method plot_joint simple_eiv
#' @param ideal_slope The hypothesized slope value to test against (default = 1)
#' @param ideal_intercept The hypothesized intercept value to test against (default = 0)
#' @param show_intervals Logical. If TRUE, shows individual confidence intervals as well.
#' @export
plot_joint.simple_eiv <- function(object,
                                  ideal_slope = 1,
                                  ideal_intercept = 0,
                                  show_intervals = TRUE,
                                  ...) {

  if (is.null(object$joint_region)) {
    stop("Joint confidence region not computed. Re-run dem_reg() with compute_joint = TRUE")
  }

  # Get estimates
  est_slope <- object$coefficients[2]
  est_intercept <- object$coefficients[1]

  # Get confidence intervals
  ci_slope <- c(object$model_table$lower.ci[2], object$model_table$upper.ci[2])
  ci_intercept <- c(object$model_table$lower.ci[1], object$model_table$upper.ci[1])

  # Test if ideal point enclosed
  ideal_test <- .test_joint_enclosure(
    intercept = est_intercept,
    slope = est_slope,
    vcov = object$vcov,
    ideal_intercept = ideal_intercept,
    ideal_slope = ideal_slope,
    conf.level = object$conf.level
  )

  # Create base plot
  p <- ggplot() +
    # Joint confidence region (ellipse)
    geom_path(data = object$joint_region,
              aes(x = slope, y = intercept),
              color = "red",
              linewidth = 1.2) +
    # Estimated point
    geom_point(aes(x = est_slope, y = est_intercept),
               size = 3,
               color = "black") +
    # Ideal point
    geom_point(aes(x = ideal_slope, y = ideal_intercept),
               size = 4,
               shape = 4,
               stroke = 1.5,
               color = ifelse(ideal_test$is_enclosed, "darkgreen", "darkred")) +
    labs(
      title = "Joint Confidence Region",
      subtitle = sprintf("%.0f%% confidence level | Identity %s by region",
                         object$conf.level * 100,
                         ifelse(ideal_test$is_enclosed, "ENCLOSED", "NOT enclosed")),
      x = "Slope",
      y = "Intercept",
      caption = sprintf("Mahalanobis distance = %.3f | chi-squared critical = %.3f | p = %.4f",
                        ideal_test$mahalanobis_distance,
                        ideal_test$chi2_critical,
                        ideal_test$p_value)
    ) +
    theme_bw() +
    theme(plot.caption = element_text(hjust = 0))

  # Add confidence interval rectangle if requested
  if (show_intervals) {
    rect_df <- data.frame(
      xmin = ci_slope[1],
      xmax = ci_slope[2],
      ymin = ci_intercept[1],
      ymax = ci_intercept[2]
    )

    p <- p +
      geom_rect(data = rect_df,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = "blue",
                alpha = 0.1,
                color = "blue",
                linetype = "dashed")
  }

  return(p)
}


#' @rdname simple_eiv-methods
#' @method vcov simple_eiv
#' @export

vcov.simple_eiv <- function(object, ...) {
  if (is.null(object$vcov)) {
    warning("Variance-covariance matrix not available for this model.")
    return(NULL)
  }
  return(object$vcov)
}

#' @rdname simple_eiv-methods
#' @method coef simple_eiv
#' @export

coef.simple_eiv <- function(object, ...) {
  return(object$coefficients)
}

#' @rdname simple_eiv-methods
#' @method fitted simple_eiv
#' @param type Type of fitted values to return. Options are "y" (default, estimated true Y values),
#'   "x" (estimated true X values), or "both" (returns a data frame with both).
#' @export

fitted.simple_eiv <- function(object, type = c("y", "x", "both"), ...) {
  type <- match.arg(type)

  # Check if Passing-Bablok
  is_passing_bablok <- !is.null(object$method) && grepl("Passing-Bablok", object$method)

  if (is_passing_bablok && type %in% c("x", "both")) {
    stop("Estimated true X values (x_hat) are not available for Passing-Bablok regression. Use type = 'y' for fitted Y values.")
  }

  switch(type,
         y = object$fitted.values,
         x = object$x_hat,
         both = data.frame(x_hat = object$x_hat, y_hat = object$y_hat)
  )
}

#' @rdname simple_eiv-methods
#' @method residuals simple_eiv
#' @param type Type of residuals to return. Options are "optimized" (default), "x", "y", or "raw_y".
#' @export

residuals.simple_eiv <- function(object, type = c("optimized", "x", "y", "raw_y"), ...) {
  type <- match.arg(type)

  # Check if Passing-Bablok
  is_passing_bablok <- !is.null(object$method) && grepl("Passing-Bablok", object$method)

  if (is_passing_bablok && type == "optimized") {
    # Return raw_y residuals for PB
    type <- "raw_y"
  }

  if (is_passing_bablok && type %in% c("x", "y")) {
    stop("X and Y residuals are not available for Passing-Bablok regression (no x_hat/y_hat). Use type = 'raw_y'.")
  }

  b0 <- object$coefficients[1]
  b1 <- object$coefficients[2]

  switch(type,
         optimized = object$residuals,
         x = object$x_vals - object$x_hat,
         y = object$y_vals - object$y_hat,
         raw_y = object$y_vals - (b0 + b1 * object$x_vals)
  )
}

#' @rdname simple_eiv-methods
#' @method formula simple_eiv
#' @export

formula.simple_eiv <- function(x, ...) {
  formula(x$terms)
}

#' @rdname simple_eiv-methods
#' @method model.frame simple_eiv
#' @export

model.frame.simple_eiv <- function(formula, ...) {
  formula$model
}

#' @rdname simple_eiv-methods
#' @method predict simple_eiv
#' @param newdata An optional data frame containing values of X at which to predict.
#'   If omitted, the fitted values are returned.
#' @param interval Type of interval calculation. Can be "none" (default), or "confidence"
#' @param level Confidence level for intervals (default uses the model's conf.level).
#' @param se.fit Logical. If TRUE, standard errors of predictions are returned.
#' @export

predict.simple_eiv <- function(object,
                               newdata = NULL,
                               interval = c("none", "confidence"),
                               level = NULL,
                               se.fit = FALSE,
                               ...) {

  # Check if Passing-Bablok
  is_passing_bablok <- !is.null(object$method) && grepl("Passing-Bablok", object$method)

  if (is_passing_bablok && (interval != "none" || se.fit)) {
    stop("Confidence intervals and standard errors are not available for Passing-Bablok regression. Only point predictions are supported.")
  }

  interval <- match.arg(interval)

  if (is.null(level)) {
    level <- object$conf.level
  }

  # Get coefficients
  b0 <- object$coefficients[1]
  b1 <- object$coefficients[2]

  # Determine X values for prediction
  if (is.null(newdata)) {
    # Return fitted values for original data
    x_pred <- object$x_vals
  } else {
    # Extract X from newdata
    x_name <- names(object$model)[2]
    if (is.data.frame(newdata)) {
      if (!x_name %in% names(newdata)) {
        stop(paste0("Variable '", x_name, "' not found in newdata"))
      }
      x_pred <- newdata[[x_name]]
    } else {
      x_pred <- newdata
    }
  }

  # Predicted values
  y_pred <- b0 + b1 * x_pred

  # If no intervals or standard errors requested, return predictions
  if (interval == "none" && !se.fit) {
    return(y_pred)
  }

  # For Passing-Bablok, we already returned above
  # Below is Deming regression only

  # Calculate standard errors using jackknife
  n <- length(object$x_vals)

  # Function to compute prediction for a single X value using jackknife
  compute_pred_se <- function(x_new) {
    pred_jack <- numeric(n)

    for (i in 1:n) {
      # Fit model without observation i
      x_sub <- object$x_vals[-i]
      y_sub <- object$y_vals[-i]
      w_sub <- object$weights[-i]

      res_sub <- jack_dem(x_sub, y_sub,
                          w_i = w_sub,
                          error.ratio = object$error.ratio)

      b0_i <- res_sub$df$coef[1]
      b1_i <- res_sub$df$coef[2]

      pred_jack[i] <- b0_i + b1_i * x_new
    }

    # Full prediction
    pred_full <- b0 + b1 * x_new

    # Jackknife pseudovariates
    pred_pseudo <- n * pred_full - (n - 1) * pred_jack

    # Jackknife SE
    pred_se <- sqrt(sum((pred_pseudo - mean(pred_pseudo))^2) / (n * (n - 1)))

    return(list(pred = pred_full, se = pred_se))
  }

  # Compute for all prediction points
  pred_results <- lapply(x_pred, compute_pred_se)
  y_pred <- sapply(pred_results, function(x) x$pred)
  se_pred <- sapply(pred_results, function(x) x$se)

  # Prepare output based on options
  if (interval == "none") {
    if (se.fit) {
      return(list(fit = y_pred, se.fit = se_pred, df = object$df.residual))
    } else {
      return(y_pred)
    }
  }

  # Compute intervals
  alpha <- 1 - level
  t_crit <- qt(1 - alpha / 2, df = object$df.residual)

  if (interval == "confidence") {
    # Confidence interval for mean response
    lwr <- y_pred - t_crit * se_pred
    upr <- y_pred + t_crit * se_pred
  }

  # Return results
  result <- data.frame(fit = y_pred, lwr = lwr, upr = upr)

  if (se.fit) {
    return(list(fit = result, se.fit = se_pred, df = object$df.residual))
  } else {
    return(result)
  }
}

# internal -----
#
## Deming ----
calc_dem = function(X,Y, w_i, error.ratio){
  x_w = sum(w_i*X)/sum(w_i)
  y_w = sum(w_i*Y)/sum(w_i)
  p_w = sum(w_i * (X - x_w)*(Y - y_w))
  u_w = sum(w_i * (X - x_w)^2)
  q_w = sum(w_i * (Y - y_w)^2)
  b1_w <- ((error.ratio * q_w - u_w) + sqrt((u_w - error.ratio * q_w)^2 +
                                              4 * error.ratio * p_w^2))/(2 * error.ratio * p_w)
  b0_w <- y_w- b1_w * x_w
  return(list(b0 = b0_w, b1 = b1_w))
}
jack_dem = function(X, Y, w_i, error.ratio) {
  len <- length(X)
  u <- list()
  for (i in 1:len) {
    u <- append(u, list(calc_dem(X[-i], Y[-i], w_i[-i], error.ratio)))
  }
  b0 = rep(0, length(u))
  b1 = rep(0, length(u))
  for (j in 1:length(u)) {
    b0[j] = u[[j]]$b0
    b1[j] = u[[j]]$b1
  }

  theta_b0 <- calc_dem(X, Y, w_i, error.ratio)$b0
  theta_b1 <- calc_dem(X, Y, w_i, error.ratio)$b1

  b0_bias <- (len - 1) * (mean(b0) - theta_b0)
  b1_bias <- (len - 1) * (mean(b1) - theta_b1)

  b0_se <- sqrt(((len - 1)/len) * sum((b0 - mean(b0))^2))
  b1_se <- sqrt(((len - 1)/len) * sum((b1 - mean(b1))^2))

  # Compute variance-covariance matrix
  # The jackknife vcov should match the SE values:
  # vcov[i,i] = SE[i]^2
  # vcov[i,j] = cor(b0, b1) * SE[i] * SE[j]
  jack_cor <- cor(b0, b1)

  vcov_matrix <- matrix(
    c(b0_se^2, b0_se * b1_se * jack_cor,
      b0_se * b1_se * jack_cor, b1_se^2),
    nrow = 2, ncol = 2,
    dimnames = list(c("Intercept", "Slope"),
                    c("Intercept", "Slope"))
  )

  res = data.frame(
    row.names = c("Intercept", "Slope"),
    coef = c(theta_b0, theta_b1),
    bias = c(b0_bias, b1_bias),
    se = c(b0_se, b1_se)
  )

  return(list(
    df = res,
    jacks = list(b0 = b0, b1 = b1),
    vcov = vcov_matrix
  ))
}
