# Data generators for the dem_reg() / pb_reg() simulations. Written
# independently of the package helpers (.generate_deming_data() etc.) so the
# power-function check compares against separate code.
#
# Error ratio convention as in the package: lambda = var(e_x) / var(e_y).

# Part A (type I error): H0 true, y_true = x_true.
#   range:    c(lo, hi) for x_true ~ Uniform(lo, hi)
#   err:      SD(e_y) / SD(x_true) (on average, for var_type = "prop")
#   var_type: "const" (constant error SD) or "prop" (constant CV; the
#             variance structure weighted Deming assumes)
gen_h0 <- function(n, range, lambda, err, var_type = "const") {
  x_true <- runif(n, range[1], range[2])
  sd_true <- diff(range) / sqrt(12)
  if (var_type == "const") {
    sd_y <- rep(err * sd_true, n)
  } else {
    cv <- err * sd_true / mean(range)
    sd_y <- cv * x_true
  }
  sd_x <- sqrt(lambda) * sd_y
  data.frame(x = x_true + rnorm(n, 0, sd_x),
             y = x_true + rnorm(n, 0, sd_y))
}

# Part A replicate block: k replicates per subject on each method, long
# format with subject id; constant error variance.
gen_h0_reps <- function(n, range, lambda, err, k = 2) {
  x_true <- runif(n, range[1], range[2])
  sd_y <- err * diff(range) / sqrt(12)
  sd_x <- sqrt(lambda) * sd_y
  tr <- rep(x_true, each = k)
  data.frame(id = rep(seq_len(n), each = k),
             x = tr + rnorm(n * k, 0, sd_x),
             y = tr + rnorm(n * k, 0, sd_y))
}

# Part B (power): the data-generating model deming_power_sim() documents.
# Variance of each method at true value u:
#   constant: beta1; proportional: beta1 * u^2; power: (beta1 + beta2 |u|)^J
var_fun <- function(u, p) {
  switch(p$type,
         constant = rep(p$beta1, length(u)),
         proportional = p$beta1 * u^2,
         power = (p$beta1 + p$beta2 * abs(u))^p$J)
}

gen_power <- function(n, sc) {
  x_true <- runif(n, sc$x_range[1], sc$x_range[2])
  y_true <- sc$actual_intercept + sc$actual_slope * x_true
  data.frame(x = x_true + rnorm(n, 0, sqrt(var_fun(x_true, sc$x_var_params))),
             y = y_true + rnorm(n, 0, sqrt(var_fun(y_true, sc$y_var_params))))
}

# Error ratio the way deming_power_sim() sets it: both variance functions
# evaluated at the mean observed x.
power_ratio <- function(d, sc) {
  m <- mean(d$x)
  var_fun(m, sc$x_var_params) / var_fun(m, sc$y_var_params)
}

# Part B scenarios (deming_power_sim() argument names)
power_scenarios <- list(
  P1 = list(label = "const var, wide range, slope 1.05",
            x_range = c(10, 100), actual_slope = 1.05, actual_intercept = 0,
            y_var_params = list(beta1 = 9, beta2 = 0, J = 1, type = "constant"),
            x_var_params = list(beta1 = 9, beta2 = 0, J = 1, type = "constant"),
            weighted = FALSE),
  P2 = list(label = "const var, narrow range, slope 1.05, lambda 0.5",
            x_range = c(50, 100), actual_slope = 1.05, actual_intercept = 0,
            y_var_params = list(beta1 = 4, beta2 = 0, J = 1, type = "constant"),
            x_var_params = list(beta1 = 2, beta2 = 0, J = 1, type = "constant"),
            weighted = FALSE),
  P3 = list(label = "power var, weighted, slope 1.05",
            x_range = c(1, 100), actual_slope = 1.05, actual_intercept = 0,
            y_var_params = list(beta1 = 0.5, beta2 = 0.05, J = 2, type = "power"),
            x_var_params = list(beta1 = 0.4, beta2 = 0.04, J = 2, type = "power"),
            weighted = TRUE),
  P4 = list(label = "const var, wide range, intercept 3",
            x_range = c(10, 100), actual_slope = 1, actual_intercept = 3,
            y_var_params = list(beta1 = 9, beta2 = 0, J = 1, type = "constant"),
            x_var_params = list(beta1 = 9, beta2 = 0, J = 1, type = "constant"),
            weighted = FALSE),
  P5 = list(label = "H0 true (P1 with slope 1)",
            x_range = c(10, 100), actual_slope = 1, actual_intercept = 0,
            y_var_params = list(beta1 = 9, beta2 = 0, J = 1, type = "constant"),
            x_var_params = list(beta1 = 9, beta2 = 0, J = 1, type = "constant"),
            weighted = FALSE)
)
