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
  # if (!is_passing_bablok && !is.null(x$joint_test)) {
  #  cat("Joint Confidence Region Test (H0: slope=1, intercept=0):\n")
  #  cat(sprintf("  Identity enclosed: %s (p = %.4f)\n",
  #              ifelse(x$joint_test$is_enclosed, "Yes", "No"),
  #              x$joint_test$p_value))
  #}

  invisible(x)
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

model.frame.simple_eiv <- function(formula, na.action = na.pass, ...) {

  # Reconstruct from call if model not stored
  call <- formula$call
  fcall <- formula$terms
  data <- eval(call$data, parent.frame())

  model.frame(fcall, data = data, na.action = na.action)
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

  #cat("Residuals:\n")
  #print(summary(object$residuals))
  #cat("\n")

  cat("Coefficients:\n")
  print(object$model_table, digits = 4)
  cat("\n")

  cat(sprintf("%d degrees of freedom\n",
              object$df.residual))

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
  # # Drop
  #if (!is_passing_bablok && !is.null(object$joint_test)) {
  #  cat("\n")
  #  cat("Joint Confidence Region Test (H0: slope=1, intercept=0):\n")
  #  cat(sprintf("  Mahalanobis distance: %.4f\n", object$joint_test$mahalanobis_distance))
  #  cat(sprintf("  Chi-square critical:  %.4f\n", object$joint_test$chi2_critical))
  #  cat(sprintf("  Identity enclosed:    %s\n",
  #              ifelse(object$joint_test$is_enclosed, "Yes", "No")))
  #  cat(sprintf("  p-value:             %.4f\n", object$joint_test$p_value))
  #}

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

  if (is.null(x_name)) {
    x_name <- attr(x$terms, "term.labels")[1]
  }
  if (is.null(y_name)) {
    resp_var <- attr(x$terms, "variables")[[2]]
    y_name <- deparse(resp_var)
  }
  if (is.null(level)) {
    level <- x$conf.level
  }



  df <- .get_simple_eiv_data(x)
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
  is_passing_bablok <- !is.null(x$method) && grepl("Passing-Bablok", x$method)

  if (is_passing_bablok) {
    stop("check visuals not available for Passing-Bablok.")
  }
  df = .get_simple_eiv_data(x)
  #colnames(df) = c("y", "x")
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
#' @param n_points Number of points to use for drawing the joint confidence region (default = 100).
#' @export
plot_joint.simple_eiv <- function(object,
                                  ideal_slope = 1,
                                  ideal_intercept = 0,
                                  show_intervals = TRUE,
                                  n_points = 100,
                                  ...) {



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

  joint_region <- .compute_joint_region(
    intercept = est_intercept,
    slope = est_slope,
    vcov = vcov(object),
    conf.level = conf.level,
    n_points = n_points
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
      caption = sprintf("chi-squared statistic = %.3f | chi-squared critical = %.3f | p = %.4f",
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
  df3 = .get_simple_eiv_data(object)

  # Compute fitted values and residuals
  b0 <- object$coefficients[1]
  b1 <- object$coefficients[2]

  error.ratio = object$error.ratio
  # Compute estimated true values (from NCSS documentation page 5)
  x_hat <- df3$x + (error.ratio * b1 * d_i) / (1 + error.ratio * b1^2)
  y_hat <- df3$y - d_i / (1 + error.ratio * b1^2)

  # Check if Passing-Bablok
  is_passing_bablok <- !is.null(object$method) && grepl("Passing-Bablok", object$method)

  if (is_passing_bablok && type %in% c("x", "both")) {
    stop("Estimated true X values (x_hat) are not available for Passing-Bablok regression. Use type = 'y' for fitted Y values.")
  }

  switch(type,
         y = y_hat,
         x = x_hat,
         both = data.frame(x_hat = x_hat, y_hat = y_hat)
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
  df3 = .get_simple_eiv_data(object)

  # Compute fitted values and residuals
  b0 <- object$coefficients[1]
  b1 <- object$coefficients[2]

  # Compute d_i (raw y residuals)
  d_i <- df3$y - (b0 + b1 * df3$x)
  error.ratio = object$error.ratio
  # Compute estimated true values (from NCSS documentation page 5)
  x_hat <- df3$x + (error.ratio * b1 * d_i) / (1 + error.ratio * b1^2)
  y_hat <- df3$y - d_i / (1 + error.ratio * b1^2)

  # Compute residuals (from NCSS documentation page 10)
  res_x <- df3$x - x_hat
  res_y <- df3$y - y_hat
  d_sign <- ifelse(d_i >= 0, 1, -1)
  opt_res <- d_sign * sqrt(w_i * res_x^2 + w_i * error.ratio * res_y^2)

  if (is_passing_bablok && type == "optimized") {
    # Return raw_y residuals for PB
    type <- "raw_y"
  }

  if (is_passing_bablok && type %in% c("x", "y")) {
    stop("X and Y residuals are not available for Passing-Bablok regression (no x_hat/y_hat). Use type = 'raw_y'.")
  }



  switch(type,
         optimized = opt_res,
         x = res_x,
         y = res_y,
         raw_y = d_i
  )
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

  x_name <- attr(object$terms, "term.labels")[1]

  interval <- match.arg(interval)

  if (is.null(level)) {
    level <- object$conf.level
  }

  # Check if vcov is available
  has_vcov <- !is.null(object$vcov) && all(!is.na(object$vcov))

  # Check if intervals requested but vcov unavailable
  if (!has_vcov && (interval != "none" || se.fit)) {
    stop("Variance-covariance matrix not available. Cannot compute standard errors or confidence intervals.")
  }

  # Get coefficients
  b0 <- object$coefficients[1]
  b1 <- object$coefficients[2]

  # Always get the original data
  df3 <- .get_simple_eiv_data(object)

  # Determine X values for prediction
  if (is.null(newdata)) {
    x_pred <- df3$x
  } else {
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

  # Compute standard errors using vcov
  # SE of prediction at x is: sqrt(Var(b0) + x^2*Var(b1) + 2*x*Cov(b0,b1))
  var_b0 <- object$vcov[1, 1]
  var_b1 <- object$vcov[2, 2]
  cov_b0_b1 <- object$vcov[1, 2]

  se_pred <- sqrt(var_b0 + x_pred^2 * var_b1 + 2 * x_pred * cov_b0_b1)

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


#' Joint Confidence Region Test for Method Agreement
#'
#' Tests whether the estimated intercept and slope jointly fall within a
#' confidence region around specified ideal values (typically intercept=0
#' and slope=1 for method comparison studies).
#'
#' @param object A `simple_eiv` object from `dem_reg()` or `pb_reg()`.
#' @param ideal_intercept The hypothesized intercept value (default: 0).
#' @param ideal_slope The hypothesized slope value (default: 1).
#' @param conf.level Confidence level for the test (default: 0.95).
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class `htest` containing:
#' \describe{
#'   \item{statistic}{The Mahalanobis distance (chi-squared distributed with df=2).}
#'   \item{parameter}{Degrees of freedom (always 2).}
#'   \item{p.value}{The p-value for the test.}
#'   \item{conf.int}{The confidence level used.}
#'   \item{estimate}{Named vector of estimated intercept and slope.}
#'   \item{null.value}{Named vector of hypothesized intercept and slope.}
#'   \item{alternative}{Description of the alternative hypothesis.}
#'   \item{method}{Description of the test.}
#'   \item{data.name}{Name of the input object.}
#' }
#'
#' @details
#' The test computes the Mahalanobis distance between the estimated coefficients
#' and the hypothesized values using the variance-covariance matrix of the
#' estimates. Under the null hypothesis, this distance follows a chi-squared
#' distribution with 2 degrees of freedom.
#'
#' For Deming regression, the variance-covariance matrix is computed via

#' jackknife. For Passing-Bablok regression, bootstrap resampling must have
#' been performed (i.e., `boot_ci = TRUE` in the original call).
#'
#' @export
joint_test <- function(object, ...) {

  UseMethod("joint_test")
}

#' @rdname joint_test
#' @export
joint_test.simple_eiv <- function(object,
                                  ideal_intercept = 0,
                                  ideal_slope = 1,
                                  conf.level = 0.95,
                                  ...) {

  # Validate conf.level

  if (!is.numeric(conf.level) || length(conf.level) != 1 ||
      conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be a single number between 0 and 1")
  }

  # Extract variance-covariance matrix
  vcov_mat <- vcov(object)

  if (is.null(vcov_mat)) {
    stop("Variance-covariance matrix not available. ",
         "For Passing-Bablok regression, refit with 'boot_ci = TRUE'.")
  }

  # Check for invalid vcov matrix

  if (any(!is.finite(vcov_mat))) {
    stop("Cannot perform joint test: variance-covariance matrix contains non-finite values")
  }

  # Extract coefficients
  coefs <- coef(object)
  intercept <- coefs[1]
  slope <- coefs[2]

  # Compute test using internal function
  test_result <- .test_joint_enclosure(
    intercept = intercept,
    slope = slope,
    vcov = vcov_mat,
    ideal_intercept = ideal_intercept,
    ideal_slope = ideal_slope,
    conf.level = conf.level
  )

  # Handle failure from internal function

  if (is.null(test_result)) {
    stop("Joint test computation failed. Check variance-covariance matrix.")
  }

  # Build htest object
  dname <- deparse(formula(object$terms))

  # Format null hypothesis string for method description
  method_string <- sprintf(
    "Joint Confidence Region Test (H0: intercept = %g, slope = %g)",
    ideal_intercept, ideal_slope
  )

  statistic <- test_result$mahalanobis_distance
  names(statistic) <- "X-squared"


  parameter <- 2L
  names(parameter) <- "df"

  estimate <- c(intercept, slope)
  names(estimate) <- c("intercept", "slope")

  null.value <- c(ideal_intercept, ideal_slope)
  names(null.value) <- c("intercept", "slope")

  structure(
    list(
      statistic = statistic,
      parameter = parameter,
      p.value = test_result$p_value,
      conf.int = NULL,
      estimate = estimate,
      null.value = null.value,
      alternative = "true intercept and slope are not equal to the null values",
      method = method_string,
      data.name = dname
    ),
    class = "htest"
  )
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
