#' Power Analysis for Deming Regression
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' Functions for conducting power analysis and sample size determination for Deming regression
#' in method comparison studies. These functions help determine the sample size needed to
#' detect specified biases (proportional and/or constant) between two measurement methods.
#'
#' @name deming_power
#' @rdname deming_power


#' @title Simulate Deming Regression Power
#'
#' @description
#' Estimates statistical power to detect deviations from the line of identity using
#' simulated data with known properties.
#'
#' @param n_sims Number of simulation iterations. Default is 1000.
#' @param sample_size Sample size (number of paired observations) per simulation.
#' @param x_range Numeric vector of length 2 specifying min and max of X values (e.g., c(10, 100)).
#' @param x_dist Character specifying distribution of X values: "uniform", "central", or "right_skewed".
#' @param actual_slope Actual slope used to generate data (e.g., 1.05 for 5% proportional bias).
#' @param actual_intercept Actual intercept used to generate data.
#' @param ideal_slope Hypothesized slope to test against (typically 1 for identity line).
#' @param ideal_intercept Hypothesized intercept to test against (typically 0 for identity line).
#' @param y_var_params List with Y variance parameters: beta1, beta2, J, type.
#'   Type can be "constant", "proportional", or "power".
#'   For power function: sigma^2 = (beta1 + beta2*U)^J
#' @param x_var_params List with X variance parameters (same structure as y_var_params).
#' @param weighted Logical. Use weighted Deming regression? Default is FALSE.
#' @param conf.level Confidence level for tests. Default is 0.95.
#'
#' @return A list of class "deming_power" containing:
#'   \item{power_ci_slope}{Power based on slope confidence interval}
#'   \item{power_ci_intercept}{Power based on intercept confidence interval}
#'   \item{power_either_ci}{Power when either CI detects difference}
#'   \item{power_joint}{Power based on joint confidence region}
#'   \item{settings}{List of simulation settings}
#'   \item{advantage}{Difference in power between joint region and CIs}
#'
#' @details
#' This function generates simulated datasets with specified error characteristics and
#' tests whether confidence intervals and joint confidence regions detect deviations
#' from hypothesized values. The joint confidence region typically provides higher
#' statistical power, especially when the X-range is narrow (high slope-intercept correlation).
#'
#' The variance functions allow flexible modeling of heteroscedastic errors:
#' - constant: sigma^2 = beta1
#' - proportional: sigma^2 = beta1 * U^2 (constant CV%)
#' - power: sigma^2 = (beta1 + beta2*U)^J
#'
#' @references
#' Sadler, W.A. (2010). Joint parameter confidence regions improve the power of parametric
#' regression in method-comparison studies. Accreditation and Quality Assurance, 15, 547-554.
#'
#' @examples
#' \dontrun{
#' # Simple example: detect 5% proportional bias with constant variance
#' power_result <- deming_power_sim(
#'   n_sims = 500,
#'   sample_size = 50,
#'   x_range = c(10, 100),
#'   actual_slope = 1.05,
#'   ideal_slope = 1.0,
#'   y_var_params = list(beta1 = 25, beta2 = 0, J = 1, type = "constant"),
#'   x_var_params = list(beta1 = 20, beta2 = 0, J = 1, type = "constant")
#' )
#' print(power_result)
#'
#' # More complex: heteroscedastic errors
#' power_result2 <- deming_power_sim(
#'   n_sims = 500,
#'   sample_size = 75,
#'   x_range = c(1, 100),
#'   actual_slope = 1.03,
#'   y_var_params = list(beta1 = 0.5, beta2 = 0.05, J = 2, type = "power"),
#'   x_var_params = list(beta1 = 0.4, beta2 = 0.04, J = 2, type = "power"),
#'   weighted = TRUE
#' )
#' }
#' @importFrom scales percent_format
#' @importFrom stats rbeta
#' @export

deming_power_sim <- function(n_sims = 1000,
                             sample_size = 50,
                             x_range = c(10, 100),
                             x_dist = "uniform",
                             actual_slope = 1.05,
                             actual_intercept = 0,
                             ideal_slope = 1,
                             ideal_intercept = 0,
                             y_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
                             x_var_params = list(beta1 = 1, beta2 = 0, J = 1, type = "constant"),
                             weighted = FALSE,
                             conf.level = 0.95) {

  # Input validation
  if (sample_size < 3) stop("sample_size must be at least 3")
  if (n_sims < 100) message("n_sims < 100 may produce unstable power estimates")
  if (length(x_range) != 2) stop("x_range must be vector of length 2")
  if (x_range[2] <= x_range[1]) stop("x_range[2] must be greater than x_range[1]")

  # Storage for results
  slope_detected <- logical(n_sims)
  intercept_detected <- logical(n_sims)
  joint_detected <- logical(n_sims)

  # Run simulations
  for (i in 1:n_sims) {

    # Generate simulated data
    sim_data <- .generate_deming_data(
      n = sample_size,
      x_range = x_range,
      x_dist = x_dist,
      true_slope = actual_slope,
      true_intercept = actual_intercept,
      y_var_params = y_var_params,
      x_var_params = x_var_params
    )

    # Calculate error ratio for Deming regression
    error.ratio <- .estimate_error_ratio(
      x_var_params = x_var_params,
      y_var_params = y_var_params,
      x_mean = mean(sim_data$x)
    )

    # Fit Deming regression
    tryCatch({
      fit <- dem_reg(
        x = "x",
        y = "y",
        data = sim_data,
        weighted = weighted,
        error.ratio = error.ratio,
        conf.level = conf.level,
        keep_data = FALSE,
        compute_joint = TRUE
      )

      # Test with confidence intervals
      slope_ci <- c(fit$model$lower.ci[2], fit$model$upper.ci[2])
      int_ci <- c(fit$model$lower.ci[1], fit$model$upper.ci[1])

      slope_detected[i] <- !(ideal_slope >= slope_ci[1] && ideal_slope <= slope_ci[2])
      intercept_detected[i] <- !(ideal_intercept >= int_ci[1] && ideal_intercept <= int_ci[2])

      # Test with joint region
      joint_detected[i] <- !fit$joint_test$is_enclosed

    }, error = function(e) {
      # If fit fails, count as non-detection
      slope_detected[i] <<- FALSE
      intercept_detected[i] <<- FALSE
      joint_detected[i] <<- FALSE
    })
  }

  # Calculate power estimates
  power_ci_slope <- mean(slope_detected, na.rm = TRUE)
  power_ci_intercept <- mean(intercept_detected, na.rm = TRUE)
  power_either_ci <- mean(slope_detected | intercept_detected, na.rm = TRUE)
  power_joint <- mean(joint_detected, na.rm = TRUE)

  # Create results object
  result <- list(
    power_ci_slope = power_ci_slope,
    power_ci_intercept = power_ci_intercept,
    power_either_ci = power_either_ci,
    power_joint = power_joint,
    advantage = power_joint - power_either_ci,
    settings = list(
      n_sims = n_sims,
      sample_size = sample_size,
      x_range = x_range,
      actual_slope = actual_slope,
      actual_intercept = actual_intercept,
      ideal_slope = ideal_slope,
      ideal_intercept = ideal_intercept,
      weighted = weighted,
      conf.level = conf.level
    )
  )

  class(result) <- "deming_power"
  return(result)
}


#' @title Determine Required Sample Size for Deming Regression
#'
#' @description
#'
#' #' `r lifecycle::badge('experimental')`
#'
#' Automatically determines the minimum sample size needed to achieve target statistical
#' power for detecting specified bias in method comparison studies using Deming regression.
#'
#' @param target_power Desired statistical power (e.g., 0.80 or 0.90). Default is 0.90.
#' @param initial_n Starting sample size for search. Default is 20.
#' @param max_n Maximum sample size to try. Default is 500.
#' @param n_sims Number of simulations per sample size tested. Default is 500.
#' @param use_joint Logical. If TRUE, optimizes for joint region power; if FALSE, for CI power.
#' @param step_size Step size for sample size increments. Default is 5.
#' @param ... Additional arguments passed to deming_power_sim()
#'
#' @return A list of class "deming_sample_size" containing:
#'   \item{n_required_ci}{Required N for confidence intervals}
#'   \item{n_required_joint}{Required N for joint confidence region}
#'   \item{target_power}{Target power level}
#'   \item{power_curve}{Data frame with N and power for both methods}
#'   \item{reduction_n}{Sample size reduction using joint method}
#'   \item{reduction_pct}{Percentage reduction}
#'
#' @details
#' This function performs a grid search over sample sizes to find the minimum N
#' needed to achieve the target power. It tests both confidence interval and
#' joint confidence region approaches, allowing comparison of required sample sizes.
#'
#' Using joint confidence regions typically requires 20-50% fewer samples than
#' confidence intervals when the measurement range is narrow (max:min ratio < 10:1).
#'
#' @examples
#' \dontrun{
#' # Determine N needed for 90% power to detect 5% bias
#' sample_size_result <- deming_sample_size(
#'   target_power = 0.90,
#'   initial_n = 30,
#'   max_n = 200,
#'   n_sims = 500,
#'   x_range = c(20, 200),
#'   actual_slope = 1.05,
#'   ideal_slope = 1.0,
#'   y_var_params = list(beta1 = 1, beta2 = 0.05, J = 2, type = "power"),
#'   x_var_params = list(beta1 = 0.8, beta2 = 0.04, J = 2, type = "power")
#' )
#'
#' print(sample_size_result)
#' plot(sample_size_result)
#' }
#'
#' @export

deming_sample_size <- function(target_power = 0.90,
                                initial_n = 20,
                                max_n = 500,
                                n_sims = 500,
                                use_joint = TRUE,
                                step_size = 5,
                                ...) {

  # Input validation
  if (target_power <= 0 || target_power >= 1) {
    stop("target_power must be between 0 and 1")
  }
  if (initial_n < 3) stop("initial_n must be at least 3")
  if (max_n <= initial_n) stop("max_n must be greater than initial_n")

  # Grid of sample sizes to test
  sample_sizes <- seq(initial_n, max_n, by = step_size)

  # Storage for results
  results_list <- list()
  n_required_ci <- NA
  n_required_joint <- NA

  cat(sprintf("Searching for N to achieve %.0f%% power...\n", target_power * 100))

  for (n in sample_sizes) {
    cat(sprintf("  Testing N = %d...", n))

    # Run power simulation
    power_res <- deming_power_sim(
      n_sims = n_sims,
      sample_size = n,
      ...
    )

    results_list[[length(results_list) + 1]] <- list(
      n = n,
      power_ci = power_res$power_either_ci,
      power_joint = power_res$power_joint
    )

    cat(sprintf(" CI: %.1f%%, Joint: %.1f%%\n",
                power_res$power_either_ci * 100,
                power_res$power_joint * 100))

    # Check if targets achieved
    if (is.na(n_required_ci) && power_res$power_either_ci >= target_power) {
      n_required_ci <- n
    }
    if (is.na(n_required_joint) && power_res$power_joint >= target_power) {
      n_required_joint <- n
    }

    # Stop if both targets achieved
    if (!is.na(n_required_ci) && !is.na(n_required_joint)) {
      cat(sprintf("\nTarget power achieved!\n"))
      break
    }
  }

  # Compile results
  power_curve <- do.call(rbind, lapply(results_list, function(x) {
    data.frame(n = x$n, power_ci = x$power_ci, power_joint = x$power_joint)
  }))

  # Calculate reduction
  if (!is.na(n_required_ci) && !is.na(n_required_joint)) {
    reduction_n <- n_required_ci - n_required_joint
    reduction_pct <- (reduction_n / n_required_ci) * 100
  } else {
    reduction_n <- NA
    reduction_pct <- NA
    warning("Target power not achieved within max_n. Consider increasing max_n.")
  }

  result <- list(
    n_required_ci = n_required_ci,
    n_required_joint = n_required_joint,
    target_power = target_power,
    power_curve = power_curve,
    reduction_n = reduction_n,
    reduction_pct = reduction_pct,
    settings = list(...)
  )

  class(result) <- "deming_sample_size"
  return(result)
}



# Helper Functions (Internal) ---------



.generate_deming_data <- function(n, x_range, x_dist, true_slope, true_intercept,
                                   y_var_params, x_var_params) {

  # Generate X values
  x_true <- .generate_x_values(n, x_range, x_dist)

  # Generate true Y values
  y_true <- true_intercept + true_slope * x_true

  # Add error to X
  x_var <- sapply(x_true, function(u) {
    .calculate_variance(u, x_var_params$beta1, x_var_params$beta2,
                       x_var_params$J, x_var_params$type)
  })
  x_obs <- x_true + rnorm(n, 0, sqrt(x_var))

  # Add error to Y
  y_var <- sapply(y_true, function(u) {
    .calculate_variance(u, y_var_params$beta1, y_var_params$beta2,
                       y_var_params$J, y_var_params$type)
  })
  y_obs <- y_true + rnorm(n, 0, sqrt(y_var))

  data.frame(x = x_obs, y = y_obs, x_true = x_true, y_true = y_true)
}


.generate_x_values <- function(n, range, distribution) {
  min_x <- range[1]
  max_x <- range[2]

  if (distribution == "uniform") {
    x <- runif(n, min_x, max_x)
  } else if (distribution == "central") {
    # Beta distribution centered
    mid <- (min_x + max_x) / 2
    width <- (max_x - min_x) / 2
    x <- mid + width * (rbeta(n, 2, 2) - 0.5) * 2
  } else if (distribution == "right_skewed") {
    # Beta distribution skewed to lower end
    x <- min_x + (max_x - min_x) * rbeta(n, 0.5, 2)
  } else {
    stop("distribution must be 'uniform', 'central', or 'right_skewed'")
  }

  return(x)
}



.calculate_variance <- function(u, beta1, beta2, J, variance_type) {
  if (variance_type == "constant") {
    return(beta1)
  } else if (variance_type == "proportional") {
    return(beta1 * u^2)
  } else if (variance_type == "power") {
    return((beta1 + beta2 * abs(u))^J)
  } else {
    stop("variance_type must be 'constant', 'proportional', or 'power'")
  }
}



.estimate_error_ratio <- function(x_var_params, y_var_params, x_mean) {
  # Calculate at mean X value
  var_x <- .calculate_variance(x_mean, x_var_params$beta1, x_var_params$beta2,
                               x_var_params$J, x_var_params$type)
  var_y <- .calculate_variance(x_mean, y_var_params$beta1, y_var_params$beta2,
                               y_var_params$J, y_var_params$type)
  return(var_x / var_y)
}



# S3 Methods for deming_power and deming_sample_size objects -------------

#' @export
print.deming_power <- function(x, ...) {
  cat("\n=== Deming Regression Power Analysis ===\n\n")
  cat(sprintf("Sample Size: N = %d\n", x$settings$sample_size))
  cat(sprintf("Simulations: %d\n", x$settings$n_sims))
  cat(sprintf("Confidence Level: %.0f%%\n", x$settings$conf.level * 100))
  cat(sprintf("Testing: slope = %.3f, intercept = %.3f\n",
              x$settings$actual_slope, x$settings$actual_intercept))
  cat(sprintf("Against: slope = %.3f, intercept = %.3f\n\n",
              x$settings$ideal_slope, x$settings$ideal_intercept))

  cat("Statistical Power:\n")
  cat(sprintf("  Slope CI:          %.1f%%\n", x$power_ci_slope * 100))
  cat(sprintf("  Intercept CI:      %.1f%%\n", x$power_ci_intercept * 100))
  cat(sprintf("  Either CI:         %.1f%%\n", x$power_either_ci * 100))
  cat(sprintf("  Joint Region:      %.1f%%\n", x$power_joint * 100))
  cat(sprintf("\nJoint Region Advantage: +%.1f percentage points\n",
              x$advantage * 100))

  invisible(x)
}


#' @export
print.deming_sample_size <- function(x, ...) {
  cat("\n=== Deming Regression Sample Size Determination ===\n\n")
  cat(sprintf("Target Power: %.0f%%\n\n", x$target_power * 100))

  cat("Required Sample Sizes:\n")
  if (!is.na(x$n_required_ci)) {
    cat(sprintf("  Confidence Intervals:  N = %d\n", x$n_required_ci))
  } else {
    cat("  Confidence Intervals:  Target not achieved\n")
  }

  if (!is.na(x$n_required_joint)) {
    cat(sprintf("  Joint Region:          N = %d\n", x$n_required_joint))
  } else {
    cat("  Joint Region:          Target not achieved\n")
  }

  if (!is.na(x$reduction_n)) {
    cat(sprintf("\nSample Size Reduction:  %d (%.0f%%)\n",
                x$reduction_n, x$reduction_pct))
    cat(sprintf("You can save %d participants using joint confidence regions!\n",
                x$reduction_n))
  }

  invisible(x)
}


#' @export
plot.deming_sample_size <- function(x, ...) {

  if (nrow(x$power_curve) == 0) {
    stop("No power curve data available")
  }

  # Reshape for plotting
  df_long <- tidyr::pivot_longer(
    x$power_curve,
    cols = c("power_ci", "power_joint"),
    names_to = "method",
    values_to = "power"
  )

  df_long$method <- factor(
    df_long$method,
    levels = c("power_ci", "power_joint"),
    labels = c("Confidence Intervals", "Joint Confidence Region")
  )

  p <- ggplot(df_long, aes(x = n, y = power, color = method)) +
    geom_line(linewidth = 1) +
    geom_point() +
    geom_hline(yintercept = x$target_power,
               linetype = "dashed",
               color = "gray50") +
    scale_color_manual(
      values = c("Confidence Intervals" = "blue",
                 "Joint Confidence Region" = "red")
    ) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Statistical Power by Sample Size",
      subtitle = sprintf("Target Power: %.0f%%", x$target_power * 100),
      x = "Sample Size (N)",
      y = "Statistical Power",
      color = "Method"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")

  # Add vertical lines for required N if available
  if (!is.na(x$n_required_ci)) {
    p <- p + geom_vline(xintercept = x$n_required_ci,
                        linetype = "dotted",
                        color = "blue",
                        alpha = 0.7)
  }

  if (!is.na(x$n_required_joint)) {
    p <- p + geom_vline(xintercept = x$n_required_joint,
                        linetype = "dotted",
                        color = "red",
                        alpha = 0.7)
  }

  return(p)
}
