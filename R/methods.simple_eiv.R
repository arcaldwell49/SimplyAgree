#' Methods for simple_eiv objects
#'
#' Methods defined for objects returned from error-in-variables models (e.g., `dem_reg`, `pb_reg`).
#'
#' @param object,x object of class \code{simple_eiv} from dem_reg or pb_reg function.
#' @param data Optional data frame. Required for methods like `plot()`, `fitted()`, `residuals()`,
#'   and `predict()` if the model was fitted with `model = FALSE`. If `model = TRUE` (default),
#'   the data is stored in the object and this argument is not needed.
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
  } else if (!is.null(x$call$weighted) && x$call$weighted == TRUE) {
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
      cat("Kendall's Tau (H0: tau <= 0):\n")
      cat(sprintf("  tau:       %.4f\n", x$kendall_test$estimate))
      cat(sprintf("  p-value:   %.4f\n", x$kendall_test$p.value))
      cat("\n")
    }

    if (!is.null(x$cusum_test)) {
      cat("CUSUM Linearity Test:\n")
      cat(sprintf("  Test stat: %.4f\n", x$cusum_test$statistic))
      cat(sprintf("  p-value:   %.4f\n", x$cusum_test$p.value))
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
#' @param formula A simple_eiv object (for model.frame method)
#' @param data Optional data frame. Required if the model was fitted with `model = FALSE`.
#' @param na.action Function for handling NA values (default: na.pass)
#' @export

model.frame.simple_eiv <- function(formula, data = NULL, na.action = na.pass, ...) {
  # If model frame is stored, return it directly
  if (!is.null(formula$model)) {
    return(formula$model)
  }

  # Model frame not stored (model = FALSE), need data argument

  if (is.null(data)) {
    # Try to get data from call as last resort
    call <- formula$call
    data <- tryCatch(
      eval(call$data, parent.frame()),
      error = function(e) NULL
    )

    if (is.null(data)) {
      stop("Model frame not stored (fitted with model = FALSE). ",
           "Please provide 'data' argument or refit with model = TRUE.")
    }
  }

  # Reconstruct model frame from terms and data

  fcall <- formula$terms
  mf <- model.frame(fcall, data = data, na.action = na.action)

  # Process to df3 format (same as in dem_reg/pb_reg)
  y_vals <- model.response(mf, "numeric")
  x_vals <- mf[[2]]

  if (ncol(mf) == 3) {
    # Has id column
    id_vals <- mf[[3]]
    df <- data.frame(id = id_vals, x = x_vals, y = y_vals)
    df <- df[complete.cases(df), ]

    df3 <- df %>%
      group_by(id) %>%
      summarize(x = mean(x, na.rm = TRUE),
                y = mean(y, na.rm = TRUE),
                .groups = 'drop') %>%
      drop_na()
  } else {
    df3 <- data.frame(x = x_vals, y = y_vals)
    df3 <- df3[complete.cases(df3), ]
  }

  return(df3)
}

#' @rdname simple_eiv-methods
#' @method summary simple_eiv
#' @export

summary.simple_eiv <- function(object, ...) {
  # Determine method type
  is_passing_bablok <- !is.null(object$method) && grepl("Passing-Bablok", object$method)

  if (is_passing_bablok) {
    header <- paste0(object$method, " with ", object$conf.level * 100, "% C.I.")
  } else if (!is.null(object$call$weighted) && object$call$weighted == TRUE) {
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
    # if (!is.null(object$kendall_test)) {
    #  cat("Kendall's Tau Test (H0: tau = 0):\n")
    #  cat(sprintf("  Tau: %.4f \n",
    #              object$kendall_test$estimate))
    #  cat(sprintf("  Z:   %.4f, p = %.4f\n",
    #              object$kendall_test$statistic,
    #              object$kendall_test$p.value))
    #}

    #if (!is.null(object$cusum_test)) {
    #  cat("\n")
    #  cat("CUSUM Linearity Test:\n")
    #  cat(sprintf("  Test stat: %.4f, p ~= %.3f\n",
    #              object$cusum_test$statistic,
    #              object$cusum_test$p.value))
    #  cat(sprintf("  Linear:    %s\n", ifelse(object$cusum_test$linear, "Yes", "No")))
    #}

    # Bootstrap info
    if (!is.null(object$replicates) && object$replicates > 0) {
      cat("\n")
      cat(sprintf("Bootstrap CIs based on %d resamples\n", object$replicates))
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
#' @param parm A specification of which parameters are to be given confidence intervals,
#'   either a vector of numbers or a vector of names. If missing, all parameters are considered.
#' @param level The confidence level required, but only the model's conf.level will be accepted currently.
#' @method confint simple_eiv
#' @export

confint.simple_eiv <- function(object, parm, level = NULL, ...) {


  # Use model's confidence level if not specified
  if (is.null(level)) {
    level <- object$conf.level
  }

  # Check if requested level matches the stored level
  if (!is.null(object$conf.level) && abs(level - object$conf.level) > 1e-10) {
    message(sprintf(
      "Requested level (%.1f%%) differs from model's stored level (%.1f%%). Using stored CIs.",
      level * 100, object$conf.level * 100
    ))
  }

  # Extract confidence intervals from model_table
  cf <- object$coefficients
  pnames <- names(cf)

  # Build CI matrix from model_table
  ci <- cbind(object$model_table$lower.ci, object$model_table$upper.ci)
  rownames(ci) <- pnames

  # Column names based on level
  a <- (1 - level) / 2
  pct <- paste(format(100 * c(a, 1 - a), trim = TRUE, scientific = FALSE, digits = 3), "%")
  colnames(ci) <- pct


  # Subset if parm is specified
  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }

  ci[parm, , drop = FALSE]
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
                            data = NULL,
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



  df <- .get_simple_eiv_data(x, data = data)
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

check.simple_eiv <- function(x, data = NULL) {
  is_passing_bablok <- !is.null(x$method) && grepl("Passing-Bablok", x$method)

  if (is_passing_bablok) {
    # Get the data from the model
    df <- .get_simple_eiv_data(x, data = data)
    n <- nrow(df)

    x_name <- attr(x$terms, "term.labels")[1]


    resp_var <- attr(x$terms, "variables")[[2]]
    y_name <- deparse(resp_var)

    # Extract parameters
    b0 <- x$model_table$coef[1]
    b1 <- x$model_table$coef[2]
    ci_lower <- x$model_table$lower.ci[2]
    ci_upper <- x$model_table$upper.ci[2]

    # Get slopes, cusum, and Di from model object
    # These should be stored during pb_reg calculation
    slopes <- x$slopes
    cusum_vals <- x$cusum_test$cumsum
    Di <- x$cusum_test$Di

    # Calculate residuals
    residuals <- residuals(x, type = "raw_y")
    # ===== Plot 1: Monotonic Relationship =====
    rank_x <- rank(df$x)
    rank_y <- rank(df$y)
    kendall_result <- x$kendall_test
    kendall_tau <- kendall_result$estimate
    kendall_p <- kendall_result$p.value

    interpretation1 <- paste0("Positive correlation should be present")

    df_ranks <- data.frame(rank_x = rank_x, rank_y = rank_y)

    p1 <- ggplot(df_ranks, aes(x = rank_x, y = rank_y)) +
      geom_point(color = "#2200aa", alpha = 0.6, size = 2) +
      geom_smooth(method = "lm", se = TRUE,
                  formula = 'y ~ x',
                  color = "blue",
                  fill = "#ccaaff50", linewidth = 1) +
      labs(
        x = "Rank(Method 1)",
        y = "Rank(Method 2)",
        title = "Monotonic Relationship",
        subtitle = interpretation1,
        caption = paste0("Kendall's tau = ", signif(kendall_tau, 3),
                         ", p = ", signif(kendall_p, 4))
      ) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0)
      )

    # ===== Plot 2: Ranked Residuals =====
    # Order residuals by Di
    order_Di <- order(Di)
    ranked_residuals <- residuals[order_Di]

    df_res <- data.frame(
      index = 1:length(ranked_residuals),
      residual = ranked_residuals
    )



    interpretation2 <- "Residuals should have random scatter around zero"

    p2 <- ggplot(df_res, aes(x = index, y = residual)) +
      geom_point(color = "#2200aa", alpha = 0.6, size = 2) +
      geom_hline(yintercept = 0, color = "#99999955", linewidth = 1.5) +
      labs(
        x = "Observation (ranked by distance)",
        y = "Residuals",
        title = "Ranked Residuals Plot",
        subtitle = interpretation2
      ) +
      ylim(c(-max(abs(ranked_residuals)), max(abs(ranked_residuals)))) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank()
      )

    # ===== Plot 3: Cusum Test =====
    df_cusum <- data.frame(
      index = 1:length(cusum_vals),
      cusum = cusum_vals
    )

    max_cusum <- max(abs(cusum_vals), na.rm = TRUE)

    # Critical value from Kolmogorov-Smirnov at 5% level
    critical_val <- 1.36  # For 5% significance level

    interpretation3 <- paste0("Trend line should be within the dashed lines")

    p3 <- ggplot(df_cusum, aes(x = index, y = cusum)) +
      geom_hline(yintercept = x$cusum_test$cusum_limit,
                 linetype = "dashed", alpha = 0.5) +
      geom_hline(yintercept = -1*x$cusum_test$cusum_limit,
                 linetype = "dashed", alpha = 0.5) +
      geom_line(linewidth = 1.2, color = "blue") +
      geom_hline(yintercept = 0, color = "#99999955", linewidth = 1.5) +
      labs(
        x = "Observation (ranked by distance)",
        y = "CUSUM",
        title = "CUSUM Linearity Test",
        subtitle = interpretation3,
        caption = paste0("H = ", signif(x$cusum_test$statistic, 4),
                         ", p = ", signif(x$cusum_test$p.value, 4))
      ) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0)
      )

    # ===== Plot 4: Slopes Histogram =====
    # Filter slopes within 5 x IQR for better visibility
    slopes_clean <- slopes[!is.na(slopes) & is.finite(slopes)]
    iqr_slopes <- IQR(slopes_clean, na.rm = TRUE)
    slopes_filtered <- slopes_clean[
      slopes_clean > (b1 - 2.5*iqr_slopes) &
        slopes_clean < (b1 + 2.5*iqr_slopes)
    ]

    df_slopes <- data.frame(slopes = slopes_filtered)

    p4 <- ggplot(df_slopes, aes(x = slopes)) +
      geom_histogram(aes(y = after_stat(density)),
                     fill = "gray", color = "black", bins = 30) +
      geom_density(color = "#8866aa", linewidth = 1.5, alpha = 0.6) +
      geom_vline(aes(xintercept = b1, linetype = "Median"),
                 color = "#1222bb", linewidth = 1.5) +
      geom_vline(aes(xintercept = ci_lower, linetype = "CI"),
                 color = "#bb2212", linewidth = 1) +
      geom_vline(aes(xintercept = ci_upper, linetype = "CI"),
                 color = "#bb2212", linewidth = 1) +
      scale_linetype_manual(
        name = "",
        values = c("Median" = "solid", "CI" = "dashed"),
        guide = guide_legend(override.aes = list(
          color = c("#bb2212","#1222bb" )
        ))
      ) +
      labs(
        x = "Individual Slopes (range: 5 * IQR)",
        y = "Density",
        title = "Distribution of Pairwise Slopes"
      ) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        legend.position = "right"
      )

    # Combine all 4 plots
    wrap_plots(p1, p2, p3, p4, ncol = 2) &
      plot_annotation(
        #title = "Passing-Bablok Regression Diagnostics",
        theme = theme(
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          #plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
        )
      )

  } else {
    # Original Deming regression code (unchanged)
    df = .get_simple_eiv_data(x, data = data)
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
    p_val_het <- pchisq(Chisq, df = 1, lower.tail = FALSE)

    df1 = data.frame(x = avg_both, y = opt_res/d_sign)

    p1 = ggplot(df1, aes(x=x, y=y)) +
      geom_point() +
      geom_smooth(se = TRUE, method = "loess", linewidth = .8,
                  color ="#3aaf85", formula = y~x) +
      labs(y = "|Optimized Residuals|",
           x = "Average of Both Estimated Values",
           title = "Homogeneity of Residuals",
           subtitle = "Reference line should be flat and horizontal",
           caption = paste0("Heteroskedasticity", " \n",
                            "Breusch-Pagan Test: p = ",
                            signif(p_val_het,4))) +
      theme_bw() +
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
      )

    dat_norm <- na.omit(data.frame(y = opt_res))
    norm_test = shapiro.test(opt_res)
    norm_text = "Shapiro-Wilk Test"

    p2 = plot_qq(x = dat_norm) +
      labs(caption = paste0("Normality", " \n",
                            norm_text, ": p = ",
                            signif(norm_test$p.value,4)))+
      theme(
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent')
      )

    wrap_plots(p2, p1, ncol = 2) &
      plot_annotation(
        theme = theme(
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent')
        )
      )
  }
}

#' @rdname simple_eiv-methods
#' @export

plot_joint <- function(x, ...) {
  UseMethod("plot_joint")
}

#' @rdname simple_eiv-methods
#' @method plot_joint simple_eiv
#' @param ideal_slope The hypothesized slope value to test against (default = 1)
#' @param ideal_intercept The hypothesized intercept value to test against (default = 0)
#' @param show_intervals Logical. If TRUE, shows individual confidence intervals as well.
#' @param n_points Number of points to use for drawing the joint confidence region (default = 100).
#' @export
plot_joint.simple_eiv <- function(x,
                                  ideal_slope = 1,
                                  ideal_intercept = 0,
                                  show_intervals = TRUE,
                                  n_points = 100,
                                  ...) {
  object <- x

  # Get estimates
  est_slope <- object$coefficients[2]
  est_intercept <- object$coefficients[1]
  conf.level <- object$conf.level
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
    geom_path(data = joint_region,
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

fitted.simple_eiv <- function(object, type = c("y", "x", "both"), data = NULL, ...) {
  type <- match.arg(type)
  df3 = .get_simple_eiv_data(object, data = data)

  # Compute fitted values and residuals
  b0 <- object$coefficients[1]
  b1 <- object$coefficients[2]

  error.ratio = object$error.ratio

  # Compute d_i (raw y residuals) - needed for true value estimation
  d_i <- df3$y - (b0 + b1 * df3$x)

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

residuals.simple_eiv <- function(object, type = c("optimized", "x", "y", "raw_y"), data = NULL, ...) {
  type <- match.arg(type)

  # Check if Passing-Bablok
  is_passing_bablok <- !is.null(object$method) && grepl("Passing-Bablok", object$method)
  df3 = .get_simple_eiv_data(object, data = data)

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
  if (!is_passing_bablok){
    w_i = object$weights
    opt_res <- d_sign * sqrt(w_i * res_x^2 + w_i * error.ratio * res_y^2)
  }


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
#'   Note: For Passing-Bablok regression without bootstrap, standard errors are not
#'   available and this argument is ignored with a warning.
#' @param data Optional data frame. Required if model was fitted with `model = FALSE`
#'   and `newdata` is NULL.
#' @export
predict.simple_eiv <- function(object,
                               newdata = NULL,
                               interval = c("none", "confidence"),
                               level = NULL,
                               se.fit = FALSE,
                               data = NULL,
                               ...) {

  x_name <- attr(object$terms, "term.labels")[1]

  interval <- match.arg(interval)

  if (is.null(level)) {
    level <- object$conf.level
  }

  # Check if this is a Passing-Bablok model
  is_passing_bablok <- !is.null(object$method) && grepl("Passing-Bablok", object$method)

  # Check if vcov is available (from bootstrap)
  has_vcov <- !is.null(object$vcov) && all(is.finite(object$vcov))

  # Check if analytical PB CI data is available (stored CI slopes)
  has_pb_ci_data <- is_passing_bablok &&
    !is.null(object$ci_slopes) &&
    length(object$ci_slopes) > 0

  # Get coefficients
  b0 <- object$coefficients[1]
  b1 <- object$coefficients[2]

  # Get original data (only needed if newdata is NULL)
  df3 <- .get_simple_eiv_data(object, data = data)

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

  # Determine which CI method to use
  if (has_vcov) {
    # Use vcov-based method (works for both Deming and bootstrapped PB)
    result <- .predict_vcov_method(object, x_pred, y_pred, interval, level, se.fit)
  } else if (has_pb_ci_data) {
    # Use MethComp-style analytical method for Passing-Bablok
    if (se.fit) {
      warning("Standard errors not available for Passing-Bablok regression without bootstrap. ",
              "Ignoring se.fit = TRUE.")
    }
    result <- .predict_pb_analytical(object, x_pred, y_pred, interval, level, df3)
  } else {
    # No method available
    stop("Cannot compute confidence intervals. ",
         "For Passing-Bablok regression, either use bootstrap (replicates > 0) ",
         "or ensure analytical CI data is available.")
  }

  return(result)
}


#' Prediction using variance-covariance matrix
#' @keywords internal
#' @noRd
.predict_vcov_method <- function(object, x_pred, y_pred, interval, level, se.fit) {

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

  lwr <- y_pred - t_crit * se_pred
  upr <- y_pred + t_crit * se_pred

  # Return results
  result <- data.frame(fit = y_pred, lwr = lwr, upr = upr)

  if (se.fit) {
    return(list(fit = result, se.fit = se_pred, df = object$df.residual))
  } else {
    return(result)
  }
}


#' Prediction using MethComp-style analytical method for Passing-Bablok
#'
#' This method computes confidence intervals by enumerating all regression lines
#' within the joint confidence region for the slope, following the approach used
#' in the MethComp package (predict.PBreg).
#'
#' For each slope within the CI bounds, the corresponding intercept is computed
#' as median(y - slope * x). The prediction CI at each new x value is then the
#' envelope (min, max) of all possible predicted values across these slope-intercept
#' pairs.
#'
#' @keywords internal
#' @noRd
.predict_pb_analytical <- function(object, x_pred, y_pred, interval, level, df3) {

  if (interval == "none") {
    return(y_pred)
  }

  # Extract the slopes within the CI bounds
  ci_slopes <- object$ci_slopes

  # Get original data for computing intercepts
  x_orig <- df3$x
  y_orig <- df3$y

  # For each slope in the CI range, compute the corresponding intercept
  # Intercept = median(y - slope * x) for each slope
  intercepts <- vapply(ci_slopes, function(s) {
    median(y_orig - s * x_orig)
  }, numeric(1))

  # Initialize output
  n_pred <- length(x_pred)
  result <- data.frame(
    fit = numeric(n_pred),
    lwr = numeric(n_pred),
    upr = numeric(n_pred)
  )

  # For each prediction point, compute the envelope of all possible y values
  for (i in seq_len(n_pred)) {
    # Compute y = intercept + slope * x for all slope-intercept pairs
    y_envelope <- intercepts + ci_slopes * x_pred[i]

    # Following MethComp: fit is median, lwr is min, upr is max
    result$fit[i] <- median(y_envelope, na.rm = TRUE)
    result$lwr[i] <- min(y_envelope, na.rm = TRUE)
    result$upr[i] <- max(y_envelope, na.rm = TRUE)
  }

  return(result)
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

#' Get data from simple_eiv object
#'
#' Internal helper function to retrieve the model data frame from a simple_eiv object.
#' If the model frame is stored (model = TRUE), it returns it directly.
#' If not stored (model = FALSE), requires data argument or attempts to retrieve from call.
#'
#' @param object A simple_eiv object
#' @param data Optional data frame. Required if model was fitted with model = FALSE.
#' @return A data frame with columns x and y
#' @keywords internal
#' @noRd
.get_simple_eiv_data <- function(object, data = NULL) {
  # If model frame is stored, return it directly
  if (!is.null(object$model)) {
    return(object$model)
  }

  # Model frame not stored, need to reconstruct
  if (is.null(data)) {
    # Try to get data from call as fallback
    call <- object$call
    data <- tryCatch(
      eval(call$data, parent.frame(2)),
      error = function(e) NULL
    )

    if (is.null(data)) {
      stop("Model frame not stored (fitted with model = FALSE). ",
           "Please provide 'data' argument or refit with model = TRUE.")
    }
  }

  # Use model.frame method with data argument
  model.frame(object, data = data)
}

