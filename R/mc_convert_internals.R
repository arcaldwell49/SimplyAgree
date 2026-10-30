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
  if (par$type == "scaled") {
    return(list(fit = c(par$alpha, par$beta),
                lim1 = c(par$alpha - z * par$sd, par$beta),
                lim2 = c(par$alpha + z * par$sd, par$beta)))
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

# scaled model: alternating regression ----
# Model SC (Carstensen 2010, eq. 7.1; MethComp::AltReg):
#   y_mir = alpha_m + beta_m (mu_i + a_ir + c_mi) + e_mir
# Iterates between
#   1. for each method, a regression of y on the current "true values"
#      zeta_mir = mu_i + a_ir + c_mi (with a random item intercept), giving
#      alpha_m and beta_m;
#   2. the variance component model (no method effects) fit to
#      (y - alpha_m) / beta_m, giving mu_i, the variance components on the
#      mu scale, and predicted a_ir and c_mi, which update zeta.
# As in MethComp, the regressor in step 1 includes the predicted random
# effects (regressing on mu_i alone did not reproduce AltReg), and the
# method-by-item SD on the y scale is multiplied by I / (I - 2).
# Convergence: largest relative change in the conversion intercepts (at the
# grand mean), slopes, and the y-scale SDs (SDs below sd_lim ignored).
mc_altreg = function(df,
                     linked,
                     max_iter = 100,
                     tol = 1e-6,
                     sd_lim = 0.01,
                     control = nlme::lmeControl(returnObject = TRUE)) {
  y = df$y_t
  meth = df$method
  item = df$id
  repl = factor(df$repl)
  lv = levels(meth)
  M = length(lv)
  I = nlevels(item)
  mi = as.integer(meth)
  GM = mean(y)

  # starting values for the true values: item (linked: item-by-replicate)
  # means after removing the method means, common to all methods. MethComp
  # starts from the method-specific fitted values, which makes the first
  # slopes close to 1 and convergence slow when the slopes differ.
  y_c = y - stats::ave(y, meth)
  zeta = if (linked) {
    stats::fitted(stats::lm(y_c ~ item * repl))
  } else {
    stats::fitted(stats::lm(y_c ~ item))
  }
  rnd = if (linked) {
    list(id = nlme::pdIdent(~ method - 1), repl = ~ 1)
  } else {
    list(id = nlme::pdIdent(~ method - 1))
  }
  vf = nlme::varIdent(form = ~ 1 | method)

  cr_old = NULL
  crit = Inf
  iter = 0
  trace = list()
  while (crit > tol && iter < max_iter) {
    iter = iter + 1

    ## step 1: regress each method on zeta ----
    a = b = numeric(M)
    V = vector("list", M)
    dfx = numeric(M)
    for (m in seq_len(M)) {
      s = mi == m
      d1 = data.frame(yy = y[s], xx = zeta[s], ii = factor(item[s]))
      f1 = nlme::lme(yy ~ xx, random = ~ 1 | ii, data = d1,
                     control = control)
      cf = nlme::fixef(f1)
      a[m] = cf[[1]]
      b[m] = cf[[2]]
      V[[m]] = unname(as.matrix(stats::vcov(f1)))
      dfx[m] = f1$fixDF$X[["xx"]]
    }
    names(a) = names(b) = lv

    ## step 2: variance components on the mu scale ----
    d2 = data.frame(wk = (y - a[mi]) / b[mi], method = meth, id = item,
                    repl = repl)
    vc = nlme::lme(wk ~ -1 + id, random = rnd, weights = vf, data = d2,
                   control = control)
    re = as.matrix(vc$modelStruct$reStruct)
    sig = vc$sigma
    ratio = stats::coef(vc$modelStruct$varStruct, unconstrained = FALSE,
                        allCoef = TRUE)[lv]
    tau_z = sqrt(re$id[1, 1]) * sig
    om_z = if (linked) sqrt(re$repl[1, 1]) * sig else 0
    sig_z = unname(sig * ratio)

    r = stats::residuals(vc, level = 0:(if (linked) 2 else 1))
    cmi = r[, "fixed"] - r[, "id"]
    air = if (linked) r[, "id"] - r[, "repl"] else 0
    mu = nlme::fixef(vc)
    mu = stats::setNames(unname(mu), sub("^id", "", names(mu)))
    zeta = unname(mu[as.character(item)]) + air + cmi

    ## y-scale components and convergence ----
    tau_y = tau_z * b * I / (I - 2)
    om_y = om_z * b
    sig_y = sig_z * b
    slp = outer(b, b, "/")
    int = outer(a, rep(1, M)) - outer(rep(1, M), a) * slp + slp * GM
    cr = c(int, slp, om_y, tau_y, sig_y)
    if (!is.null(cr_old)) {
      chg = abs((cr - cr_old) / cr)
      sd_part = (2 * M * M + 1):length(cr)
      chg[sd_part] = chg[sd_part] * (cr_old[sd_part] > sd_lim)
      crit = max(chg, na.rm = TRUE)
    }
    cr_old = cr
    trace[[iter]] = data.frame(iteration = iter, method = lv,
                               alpha = unname(a - b * a[1] / b[1]),
                               beta = unname(b / b[1]),
                               criterion = if (iter == 1) NA_real_ else crit)
  }
  converged = crit <= tol

  list(a = a, b = b, vcov = V, df_x = dfx,
       mu = mu, tau_z = tau_z * I / (I - 2), omega_z = om_z, sigma_z = sig_z,
       tau_y = unname(tau_y), omega_y = unname(om_y), sigma_y = unname(sig_y),
       iterations = iter, converged = converged, criterion = crit,
       trace = do.call(rbind, trace), model = vc, lv = lv, linked = linked)
}

# conversion table from an mc_altreg() fit ----
# alpha_{k|m} = a_k - a_m b_k / b_m, beta_{k|m} = b_k / b_m,
# sd_{k|m} = sqrt(tau_k^2 + sigma_k^2 + beta_{k|m}^2 (tau_m^2 + sigma_m^2))
# (y scale; omega cancels), and sqrt(2) sigma_m for a method to itself.
# Delta-method SEs use the covariance of (a, b) from the step 1 regressions
# of the final iteration, which condition on the predicted true values.
#
# These intervals are much too narrow (they ignore the uncertainty in the
# predicted true values; 53-73% coverage of 95% intervals in
# data-raw/sims/sim_method_convert_coverage.R). The parametric bootstrap in
# mc_scaled_boot() has not been checked by simulation. Intervals with close
# to the stated coverage come from the maximum likelihood fit
# (estimator = "ml", R/mc_scaled_ml.R; 92-97% coverage in
# data-raw/sims/sim_method_convert_lavaan.R), which replaced the earlier plan
# for a Bayesian (MCMC) fit (see
# data-raw/sims/HANDOFF_method_convert_scaled_ci.md).
mc_scaled_conversion = function(alt,
                                conf_level) {
  lv = alt$lv
  a = alt$a
  b = alt$b
  v_y = alt$tau_y^2 + alt$sigma_y^2
  rows = list()
  for (k in seq_along(lv)) {
    for (m in seq_along(lv)) {
      if (k == m) {
        rows[[length(rows) + 1]] = dplyr::tibble(
          to = lv[k], from = lv[m], alpha = 0, beta = 1,
          sd_pred = sqrt(2) * alt$sigma_y[m],
          alpha_se = NA_real_, beta_se = NA_real_, df = NA_real_)
        next
      }
      beta = b[[k]] / b[[m]]
      alpha = a[[k]] - a[[m]] * beta
      # gradients with respect to (a_m, b_m, a_k, b_k)
      g_alpha = c(-beta, a[[m]] * b[[k]] / b[[m]]^2, 1, -a[[m]] / b[[m]])
      g_beta = c(0, -b[[k]] / b[[m]]^2, 0, 1 / b[[m]])
      Vmk = matrix(0, 4, 4)
      Vmk[1:2, 1:2] = alt$vcov[[m]]
      Vmk[3:4, 3:4] = alt$vcov[[k]]
      rows[[length(rows) + 1]] = dplyr::tibble(
        to = lv[k], from = lv[m], alpha = alpha, beta = beta,
        sd_pred = sqrt(v_y[k] + beta^2 * v_y[m]),
        alpha_se = sqrt(drop(t(g_alpha) %*% Vmk %*% g_alpha)),
        beta_se = sqrt(drop(t(g_beta) %*% Vmk %*% g_beta)),
        df = min(alt$df_x[c(m, k)]))
    }
  }
  out = dplyr::bind_rows(rows)
  q = qt((1 + conf_level) / 2, out$df)
  out$alpha_lower = out$alpha - q * out$alpha_se
  out$alpha_upper = out$alpha + q * out$alpha_se
  out$beta_lower = out$beta - q * out$beta_se
  out$beta_upper = out$beta + q * out$beta_se
  out
}

# y-scale variance components of an mc_altreg() fit
mc_scaled_varcomp = function(alt) {
  lv = alt$lv
  out = dplyr::tibble(component = rep(c("tau", "omega", "sigma"),
                                      each = length(lv)),
                      method = rep(lv, 3),
                      sd = c(alt$tau_y, alt$omega_y, alt$sigma_y))
  if (!alt$linked) {
    out = out[out$component != "omega", ]
  }
  out
}

# parametric bootstrap ----
# Data are simulated from the fitted model with the observed design (the
# same items, methods, and replicates), using the fitted item values, and the
# model is refit to each. Percentile intervals for the conversion
# parameters, prediction SDs, and variance components.
mc_scaled_boot = function(alt,
                          df,
                          boot_reps,
                          conf_level,
                          max_iter,
                          tol) {
  lv = alt$lv
  M = length(lv)
  linked = alt$linked
  # observed design, with replicate numbers 1..n
  des = data.frame(id = as.character(df$id),
                   method = as.character(df$method))
  des$repl = if (linked) {
    match(df$repl, sort(unique(df$repl)))
  } else {
    stats::ave(seq_len(nrow(df)), des$id, des$method, FUN = seq_along)
  }
  n_reps = max(des$repl)
  items = names(alt$mu)
  key_obs = paste(des$id, des$method, des$repl)
  rep_type = if (linked) "linked" else "exchangeable"

  one = function() {
    sim = sim_methods(n_items = length(items), n_reps = n_reps,
                      methods = lv, alpha = unname(alt$a),
                      beta = unname(alt$b), tau = alt$tau_z,
                      omega = alt$omega_z, sigma = alt$sigma_y,
                      replicates = rep_type,
                      mu_values = unname(alt$mu))
    sim$id = items[sim$id]
    sim = sim[paste(sim$id, sim$method, sim$repl) %in% key_obs, ]
    sim$id = factor(sim$id, levels = items)
    d_b = suppressMessages(
      mc_data_org(as.data.frame(sim), y = "y", method = "method",
                  id = "id", replicate = "repl", replicates = rep_type)
    )
    fit_b = mc_altreg(d_b, linked = linked, max_iter = max_iter, tol = tol)
    if (!fit_b$converged) {
      return(NULL)
    }
    cv = mc_scaled_conversion(fit_b, conf_level = conf_level)
    c(cv$alpha, cv$beta, cv$sd_pred, mc_scaled_varcomp(fit_b)$sd)
  }

  res = vector("list", boot_reps)
  for (r in seq_len(boot_reps)) {
    res[[r]] = tryCatch(suppressWarnings(one()), error = function(e) NULL)
  }
  ok = !vapply(res, is.null, logical(1))
  n_cv = M * M
  n_par = 3 * n_cv + (if (linked) 3 else 2) * M
  probs = c((1 - conf_level) / 2, (1 + conf_level) / 2)
  if (sum(ok) > 1) {
    mat = do.call(rbind, res[ok])
    ci = t(apply(mat, 2, stats::quantile, probs = probs, na.rm = TRUE))
  } else {
    mat = NULL
    ci = matrix(NA_real_, n_par, 2)
  }
  list(alpha = ci[seq_len(n_cv), , drop = FALSE],
       beta = ci[n_cv + seq_len(n_cv), , drop = FALSE],
       sd_pred = ci[2 * n_cv + seq_len(n_cv), , drop = FALSE],
       varcomp = ci[-seq_len(3 * n_cv), , drop = FALSE],
       n_fail = sum(!ok),
       reps = mat)
}
