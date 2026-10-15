# Internals for method_convert() ----

# cluster-robust covariance ----
# CR1 sandwich covariance of the coefficients of an lm, clustered by `cl`:
# (X'X)^-1 (sum_g X_g' e_g e_g' X_g) (X'X)^-1 * G/(G-1) * (N-1)/(N-p)
mc_vcov_cluster = function(fit,
                           cl) {
  X = stats::model.matrix(fit)
  e = stats::residuals(fit)
  N = nrow(X)
  p = ncol(X)
  cl = droplevels(as.factor(cl))
  G = nlevels(cl)
  bread = solve(crossprod(X))
  scores = rowsum(X * e, cl)
  meat = crossprod(scores)
  V = bread %*% meat %*% bread * (G / (G - 1)) * ((N - 1) / (N - p))
  dimnames(V) = list(colnames(X), colnames(X))
  V
}

# differences on averages (DA) regression for one pair ----
# Carstensen (2010), section 4.7, and MethComp::DA.reg. With D = y1 - y2 and
# A = (y1 + y2) / 2 (transformed scale):
#   D = a + b A + e,  SD(e | A) = s               (variance = "constant")
#                     SD(e | A) = c' + d' A       (variance = "linear";
#                     c' = sqrt(pi/2) c, d' = sqrt(pi/2) d from regressing
#                     |residuals| on A)
# When items contribute several pairs (replicates), the covariance of (a, b)
# is cluster-robust by item, with G - 1 df.
mc_da_fit = function(pp,
                     variance = "constant") {
  if (nrow(pp) < 4) {
    stop("Fewer than 4 paired measurements; the conversion cannot be ",
         "estimated.", call. = FALSE)
  }
  fit = stats::lm(d ~ avg, data = pp)
  cf = stats::coef(fit)
  n_per_id = table(droplevels(as.factor(pp$id)))
  clustered = any(n_per_id > 1)
  if (clustered) {
    V = mc_vcov_cluster(fit, pp$id)
    df = length(n_per_id) - 1
  } else {
    V = stats::vcov(fit)
    df = stats::df.residual(fit)
  }
  s = stats::sigma(fit)

  sd_fit = NULL
  c_sd = s
  d_sd = 0
  if (variance == "linear") {
    absr = abs(stats::residuals(fit))
    sd_fit = stats::lm(absr ~ pp$avg)
    c_sd = sqrt(pi / 2) * unname(stats::coef(sd_fit)[1])
    d_sd = sqrt(pi / 2) * unname(stats::coef(sd_fit)[2])
  }
  list(a = unname(cf[1]),
       b = unname(cf[2]),
       vcov = unname(V),
       df = df,
       s = s,
       c_sd = c_sd,
       d_sd = d_sd,
       clustered = clustered,
       n_pairs = nrow(pp),
       n_items = length(n_per_id),
       fit = fit,
       sd_fit = sd_fit)
}

# conversion parameters from a DA fit ----
# Carstensen (2010) eq. 4.11: y1 = a/(1 - b/2) + (1 + b/2)/(1 - b/2) y2, and
# y2 = -a/(1 + b/2) + (1 - b/2)/(1 + b/2) y1. Delta-method SEs from the
# covariance of (a, b).
mc_da_conversion = function(da,
                            m1,
                            m2,
                            conf_level) {
  a = da$a
  b = da$b
  if (abs(b) >= 2) {
    stop("The slope of the differences on the averages for ", m1, " and ",
         m2, " is ", signif(b, 3), "; it must be between -2 and 2 for ",
         "the methods to be related by a line with positive slope. The ",
         "methods may not measure the same quantity.", call. = FALSE)
  }
  V = da$vcov
  qt_c = qt((1 + conf_level) / 2, da$df)
  # gradients with respect to (a, b)
  dirs = list(
    # to = m1, from = m2
    list(to = m1, from = m2,
         alpha = a / (1 - b / 2),
         beta = (1 + b / 2) / (1 - b / 2),
         g_alpha = c(1 / (1 - b / 2), a / 2 / (1 - b / 2)^2),
         g_beta = c(0, 1 / (1 - b / 2)^2),
         sd_pred = if (da$d_sd == 0) da$s / (1 - b / 2) else NA_real_),
    # to = m2, from = m1
    list(to = m2, from = m1,
         alpha = -a / (1 + b / 2),
         beta = (1 - b / 2) / (1 + b / 2),
         g_alpha = c(-1 / (1 + b / 2), a / 2 / (1 + b / 2)^2),
         g_beta = c(0, -1 / (1 + b / 2)^2),
         sd_pred = if (da$d_sd == 0) da$s / (1 + b / 2) else NA_real_)
  )
  rows = lapply(dirs, function(d) {
    se_a = sqrt(drop(t(d$g_alpha) %*% V %*% d$g_alpha))
    se_b = sqrt(drop(t(d$g_beta) %*% V %*% d$g_beta))
    dplyr::tibble(to = d$to, from = d$from,
                  alpha = d$alpha, beta = d$beta, sd_pred = d$sd_pred,
                  alpha_se = se_a, beta_se = se_b, df = da$df,
                  alpha_lower = d$alpha - qt_c * se_a,
                  alpha_upper = d$alpha + qt_c * se_a,
                  beta_lower = d$beta - qt_c * se_b,
                  beta_upper = d$beta + qt_c * se_b)
  })
  dplyr::bind_rows(rows)
}

# prediction lines ----
# Parameters to evaluate the conversion and its prediction limits for one
# ordered pair (to, from) at any level, on the transformed scale.
# - "constant": y_to = alpha + y_from, SD sd (from a method_vc() fit)
# - "da": DA model for the stored pair (m1 - m2) with a, b, c_sd, d_sd; the
#   limits are the lines D = (a +/- z c_sd) + (b +/- z d_sd) A transformed to
#   the y scale (eq. 4.11), inverted when to = m2.
mc_limits_par = function(type,
                         to,
                         from,
                         ...) {
  c(list(type = type, to = to, from = from), list(...))
}

# lines (intercept, slope) for the fit and both limits at quantile z
mc_conv_lines = function(par,
                         z) {
  if (par$type == "constant") {
    return(list(fit = c(par$alpha, 1),
                lim1 = c(par$alpha - z * par$sd, 1),
                lim2 = c(par$alpha + z * par$sd, 1)))
  }
  da_to_y = function(a, b) {
    if (abs(b) >= 2) {
      stop("The prediction limits are not defined at this level: the ",
           "slope of a limit line for the differences (", signif(b, 3),
           ") is outside (-2, 2). Use a lower level or ",
           "variance = 'constant'.", call. = FALSE)
    }
    c(a / (1 - b / 2), (1 + b / 2) / (1 - b / 2))
  }
  lines = list(fit = da_to_y(par$a, par$b),
               lim1 = da_to_y(par$a - z * par$c_sd, par$b - z * par$d_sd),
               lim2 = da_to_y(par$a + z * par$c_sd, par$b + z * par$d_sd))
  if (par$to == par$m2) {
    # invert y1 = int + slope * y2 to y2 = (y1 - int) / slope
    lines = lapply(lines, function(l) c(-l[1] / l[2], 1 / l[2]))
  }
  lines
}

# conversion grid ----
# For values of the `from` method on the original scale: transform, compute
# the converted value and prediction limits on the transformed scale, and
# back-transform all three (Carstensen 2010, ch. 8).
mc_conv_grid = function(x,
                        from,
                        to,
                        values,
                        level = NULL) {
  if (is.null(level)) {
    level = x$fit_info$pred_level
  }
  key = paste0(to, "|", from)
  par = x$limits_par[[key]]
  if (is.null(par)) {
    stop("No conversion from '", from, "' to '", to, "'. Methods are: ",
         paste(levels(x$data$method), collapse = ", "), ".", call. = FALSE)
  }
  tf = x$transform
  z = qnorm((1 + level) / 2)
  in_dom = vapply(values, function(v) {
    !is.na(v) && isTRUE(tf$domain_check(v))
  }, logical(1))
  t_val = rep(NA_real_, length(values))
  t_val[in_dom] = tf$fun(values[in_dom])

  if (from == to) {
    fit = t_val
    sd_m = x$conversion$sd_pred[x$conversion$to == to &
                                  x$conversion$from == from]
    lo = t_val - z * sd_m
    hi = t_val + z * sd_m
  } else {
    ln = mc_conv_lines(par, z)
    fit = ln$fit[1] + ln$fit[2] * t_val
    l1 = ln$lim1[1] + ln$lim1[2] * t_val
    l2 = ln$lim2[1] + ln$lim2[2] * t_val
    lo = pmin(l1, l2)
    hi = pmax(l1, l2)
  }
  dplyr::tibble(from_value = values,
                fit = tf$inv(fit),
                lower = tf$inv(lo),
                upper = tf$inv(hi))
}
