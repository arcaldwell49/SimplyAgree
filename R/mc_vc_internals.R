# Internals for method_vc() ----

# design ----
# Decide which model can be fit (Carstensen 2010, ch. 5; identifiability
# rules in the implementation guide):
# - "single_2":  no replicates, 2 methods. gls with a common residual; only
#                the sum of the method variances is identifiable, so this is
#                the paired-difference analysis.
# - "single_m":  no replicates, >= 3 methods. Method-specific residual SDs
#                (Grubbs-type); the method-by-item variation is absorbed into
#                them. Items are random here: with fixed item effects the
#                likelihood is unbounded (one residual SD goes to zero).
# - "reps":      replicates. Fixed item effects, random method-by-item effects
#                (tau), method-specific residual SDs, and for linked
#                replicates a random item-by-replicate effect (omega).
mc_vc_design = function(df,
                        replicates,
                        tau) {
  M = attr(df, "n_methods")
  n_mi = table(df$method, df$id)
  any_reps = any(n_mi > 1)
  has_reps = attr(df, "has_reps")

  if (tau == "method" && M == 2) {
    stop("Method-specific tau is not identifiable with two methods; only ",
         "their sum is used in the limits. Use tau = 'common'.",
         call. = FALSE)
  }

  if (!any_reps) {
    if (replicates == "linked") {
      message("No item has replicate measurements, so replicates = 'linked' ",
              "has no effect.")
    }
    if (M >= 3) {
      message("No replicate measurements: the method-by-item variation is ",
              "absorbed into the method-specific residual SDs, and ",
              "repeatability cannot be estimated.")
      model_type = "single_m"
    } else {
      model_type = "single_2"
    }
    return(list(model_type = model_type, linked = FALSE, tau = "absorbed",
                has_reps = FALSE, n_methods = M,
                n_items = attr(df, "n_items")))
  }

  if (!has_reps) {
    no_rep = rownames(n_mi)[apply(n_mi, 1, max) < 2]
    if (M == 2) {
      stop("Method(s) ", paste(no_rep, collapse = ", "), " have no replicate ",
           "measurements, so the residual SDs cannot be separated from the ",
           "method-by-item variation. Supply replicates for both methods, or ",
           "analyze one measurement per item (replicate = NULL).",
           call. = FALSE)
    }
    warning("Method(s) ", paste(no_rep, collapse = ", "), " have no ",
            "replicate measurements; their residual SDs are separated from ",
            "the method-by-item variation only through the model assumptions.",
            call. = FALSE)
  }

  # replicate pairs available to estimate each residual SD
  rep_df = rowSums(pmax(n_mi - 1, 0))
  few = names(rep_df)[rep_df < 3]
  if (length(few) > 0) {
    warning("Fewer than 3 replicate pairs for method(s) ",
            paste(few, collapse = ", "), "; the residual SD (sigma) is ",
            "poorly estimated.", call. = FALSE)
  }

  list(model_type = "reps",
       linked = replicates == "linked",
       tau = tau,
       has_reps = TRUE,
       n_methods = M,
       n_items = attr(df, "n_items"))
}

# fitting ----
# Fits by REML on the transformed scale (y_t). For linked replicates the
# item-by-replicate effect is a random intercept for replicate nested in item
# (as in MethComp::VC.est). The pdBlocked parameterization with an
# item-by-replicate indicator is equivalent, but has more random effects than
# observations per item, which lme does not allow.
mc_vc_fit = function(df,
                     design,
                     control) {
  vf = nlme::varIdent(form = ~ 1 | method)

  fit_fun = function() {
    switch(
      design$model_type,
      single_2 = {
        fixed = y_t ~ -1 + method + id
        fit = nlme::gls(fixed, data = df)
        mc_self_contained(fit, df, fixed = fixed)
      },
      single_m = {
        fixed = y_t ~ -1 + method
        random = ~ 1 | id
        fit = nlme::lme(fixed = fixed, random = random, weights = vf,
                        data = df, control = control)
        mc_self_contained(fit, df, fixed = fixed, random = random,
                          weights = vf, control = control)
      },
      reps = {
        fixed = y_t ~ -1 + method + id
        pd = if (design$tau == "method") {
          nlme::pdDiag(~ method - 1)
        } else {
          nlme::pdIdent(~ method - 1)
        }
        random = if (design$linked) {
          list(id = pd, repl = ~ 1)
        } else {
          list(id = pd)
        }
        fit = nlme::lme(fixed = fixed, random = random, weights = vf,
                        data = df, control = control)
        mc_self_contained(fit, df, fixed = fixed, random = random,
                          weights = vf, control = control)
      }
    )
  }

  tryCatch(
    fit_fun(),
    error = function(e) {
      stop("The variance component model could not be fit: ",
           conditionMessage(e),
           "\nTry control = nlme::lmeControl(opt = 'optim').",
           call. = FALSE)
    }
  )
}

# The calls above refer to objects local to mc_vc_fit(); replace them with
# the objects so update() and getData() work on the returned model (same
# pattern as model_self_contained() in tolerance_limit.R).
mc_self_contained = function(fit,
                             df,
                             fixed,
                             random = NULL,
                             weights = NULL,
                             control = NULL) {
  data_env = new.env(parent = baseenv())
  assign("mc_data", as.data.frame(df), envir = data_env)
  if (inherits(fit, "lme")) {
    fit$call[[1]] = quote(nlme::lme)
    fit$call$fixed = fixed
    fit$call$random = random
    fit$call$weights = weights
    fit$call$control = control
  } else {
    fit$call[[1]] = quote(nlme::gls)
    fit$call$model = fixed
  }
  fit$call$data = call("get", "mc_data", envir = data_env)
  fit
}

# variance components ----
# Point estimates of the variance components, with a matrix A mapping the
# variance parameters of the model (attr(apVar, "Pars"): log SDs of the
# random effects, log SD ratios of varIdent, and lSigma) to the components:
# log(var_j) = 2 * A[j, ] %*% pars.
mc_vc_extract = function(fit,
                         design,
                         lv) {
  M = length(lv)

  if (design$model_type == "single_2") {
    return(dplyr::tibble(component = "sigma",
                         method = NA_character_,
                         sd = sigma(fit),
                         var = sigma(fit)^2))
  }

  sig = fit$sigma
  ratio = stats::coef(fit$modelStruct$varStruct, unconstrained = FALSE,
                      allCoef = TRUE)
  sigma_m = sig * unname(ratio[lv])

  rows = list()
  if (design$model_type == "reps") {
    re = as.matrix(fit$modelStruct$reStruct)
    tau_v = diag(re$id) * sig^2
    if (design$tau == "common") {
      rows$tau = dplyr::tibble(component = "tau", method = NA_character_,
                               var = unname(tau_v[1]))
    } else {
      rows$tau = dplyr::tibble(component = "tau", method = lv,
                               var = unname(tau_v))
    }
    if (design$linked) {
      rows$omega = dplyr::tibble(component = "omega", method = NA_character_,
                                 var = unname(re$repl[1, 1]) * sig^2)
    }
  }
  rows$sigma = dplyr::tibble(component = "sigma", method = lv,
                             var = sigma_m^2)

  dplyr::bind_rows(rows) %>%
    dplyr::mutate(sd = sqrt(var)) %>%
    dplyr::select(component, method, sd, var)
}

# A matrix: rows = components (as in mc_vc_extract), columns = apVar pars
mc_vc_parmap = function(fit,
                        design,
                        varcomp,
                        lv) {
  pars = attr(fit$apVar, "Pars")
  A = matrix(0, nrow(varcomp), length(pars),
             dimnames = list(NULL, names(pars)))
  vs_groups = attr(fit$modelStruct$varStruct, "groupNames")
  vs_pars = grep("^varStruct", names(pars), value = TRUE)
  for (j in seq_len(nrow(varcomp))) {
    comp = varcomp$component[j]
    m = varcomp$method[j]
    if (comp == "tau") {
      nm = if (is.na(m)) "reStruct.id" else paste0("reStruct.id", match(m, lv))
      A[j, nm] = 1
    } else if (comp == "omega") {
      A[j, "reStruct.repl"] = 1
    } else if (comp == "sigma") {
      A[j, "lSigma"] = 1
      g = match(m, vs_groups)
      if (g > 1) {
        A[j, vs_pars[g - 1]] = 1
      }
    }
  }
  A
}

# Approximate covariance of the variance components (delta method on apVar),
# Wald CIs on the log-SD scale, and component df 2 * v^2 / var(v).
mc_vc_delta = function(fit,
                       design,
                       varcomp,
                       lv,
                       conf_level) {
  q = qnorm((1 + conf_level) / 2)
  K = nrow(varcomp)

  ## exact: common residual, paired differences ----
  if (design$model_type == "single_2") {
    nu = fit$dims$N - fit$dims$p
    v = varcomp$var
    varcomp$lower = sqrt(nu * v / qchisq((1 + conf_level) / 2, nu))
    varcomp$upper = sqrt(nu * v / qchisq((1 - conf_level) / 2, nu))
    varcomp$df = nu
    return(list(varcomp = varcomp,
                cov = matrix(2 * v^2 / nu, 1, 1)))
  }

  av = fit$apVar
  ok = !is.null(av) && !is.character(av)
  if (ok) {
    A = tryCatch(mc_vc_parmap(fit, design, varcomp, lv),
                 error = function(e) NULL)
    pars = attr(av, "Pars")
    # check that the map reproduces the estimates
    ok = !is.null(A) &&
      isTRUE(all.equal(unname(exp(2 * drop(A %*% pars))), unname(varcomp$var),
                       tolerance = 1e-6))
  }
  if (!ok) {
    warning("Approximate covariance of the variance components is ",
            "unavailable (a component may be estimated near zero). ",
            "Confidence intervals for variance components, RC, and limits ",
            "are not computed.", call. = FALSE)
    varcomp$lower = varcomp$upper = varcomp$df = NA_real_
    return(list(varcomp = varcomp,
                cov = matrix(NA_real_, K, K)))
  }

  cov_log = 4 * A %*% av %*% t(A)
  v = varcomp$var
  cov_v = cov_log * outer(v, v)
  se_log_sd = sqrt(diag(cov_log)) / 2
  varcomp$lower = exp(log(varcomp$sd) - q * se_log_sd)
  varcomp$upper = exp(log(varcomp$sd) + q * se_log_sd)
  varcomp$df = 2 / diag(cov_log)
  list(varcomp = varcomp,
       cov = cov_v)
}

# boundary estimates ----
mc_vc_boundary = function(varcomp) {
  re = varcomp[varcomp$component %in% c("tau", "omega"), ]
  if (nrow(re) == 0) {
    return(invisible(NULL))
  }
  min_sigma = min(varcomp$sd[varcomp$component == "sigma"])
  small = re$sd < 1e-4 * min_sigma
  if (any(small)) {
    lab = ifelse(is.na(re$method[small]), re$component[small],
                 paste0(re$component[small], " (", re$method[small], ")"))
    warning("Variance component(s) ", paste(lab, collapse = ", "),
            " estimated at or near zero (boundary estimate). Confidence ",
            "intervals that depend on them may be unreliable.",
            call. = FALSE)
  }
  invisible(NULL)
}

# method effects ----
# alpha_m: mean of method m averaged over the items in the model (method
# coefficient plus the average item effect), with its covariance. Biases are
# differences of these, so they do not depend on the item coding.
mc_vc_alpha = function(fit,
                       lv) {
  b = if (inherits(fit, "lme")) nlme::fixef(fit) else stats::coef(fit)
  V = as.matrix(stats::vcov(fit))
  nm = names(b)
  meth_cols = paste0("method", lv)
  id_cols = grep("^id", nm)
  L = matrix(0, length(lv), length(b), dimnames = list(lv, nm))
  for (m in seq_along(lv)) {
    L[m, meth_cols[m]] = 1
    if (length(id_cols) > 0) {
      # treatment coding: the first item has effect 0
      L[m, id_cols] = 1 / (length(id_cols) + 1)
    }
  }
  list(alpha = drop(L %*% b),
       vcov = L %*% V %*% t(L))
}

# Containment degrees of freedom for the method contrasts: the method by item
# cells, minus the item and method parameters. This is (I - 1)(M - 1) for
# complete data; with no method-by-item effect in the model the cells are the
# observations and it is the residual df. nlme's own containment df (fixDF)
# ignores the random method-by-item slopes and would use the residual df.
mc_vc_bias_df = function(df) {
  n_cells = nrow(unique(df[, c("method", "id")]))
  n_cells - nlevels(df$id) - nlevels(df$method) + 1
}

# pairs of methods, earlier level first
mc_pairs = function(lv) {
  idx = utils::combn(length(lv), 2)
  data.frame(m = idx[1, ], k = idx[2, ],
             method1 = lv[idx[1, ]], method2 = lv[idx[2, ]],
             stringsAsFactors = FALSE)
}

mc_vc_bias = function(alpha,
                      lv,
                      bias_df,
                      conf_level) {
  pr = mc_pairs(lv)
  est = se = numeric(nrow(pr))
  for (j in seq_len(nrow(pr))) {
    cvec = numeric(length(lv))
    cvec[pr$m[j]] = 1
    cvec[pr$k[j]] = -1
    est[j] = sum(cvec * alpha$alpha)
    se[j] = sqrt(drop(t(cvec) %*% alpha$vcov %*% cvec))
  }
  qt_b = qt((1 + conf_level) / 2, bias_df)
  dplyr::tibble(method1 = pr$method1,
                method2 = pr$method2,
                bias = est,
                se = se,
                df = bias_df,
                lower.CL = est - qt_b * se,
                upper.CL = est + qt_b * se)
}

# limits ----
# Weights of each variance component in the variance of a difference between
# single measurements of methods m and k on a new item (Carstensen 2010,
# p. 57). omega cancels when both methods are applied on the same occasion;
# include_omega adds 2 * omega^2 (methods applied on different occasions).
mc_vc_weights = function(varcomp,
                         m,
                         k,
                         include_omega = FALSE) {
  w = numeric(nrow(varcomp))
  for (j in seq_len(nrow(varcomp))) {
    comp = varcomp$component[j]
    meth = varcomp$method[j]
    if (comp == "omega") {
      w[j] = if (include_omega) 2 else 0
    } else if (is.na(meth)) {
      w[j] = 2
    } else {
      w[j] = as.numeric(meth %in% c(m, k))
    }
  }
  w
}

# Limits for each pair. The SD of the difference, s = sqrt(sum w_j v_j), has
# Satterthwaite df 2 s^4 / var(s^2), with var(s^2) = w' Cov(v) w from the
# delta method (accounting for the covariance of the estimated components).
# - agreement: limits bias +/- z s, with confidence bounds from the MOVER as
#   in agreement_limit(): z_side * SE(bias) combined with z * (U - s), where
#   U is the one-sided upper confidence bound for s (sd_upper_at()).
# - tolerance: analytic_bounds() from tolerance_limit() ("joint": Howe-type;
#   "iu": MOVER, or the exact noncentral t for independent data).
mc_limits = function(bias_tab,
                     varcomp,
                     cov_v,
                     design,
                     interval,
                     pred_level,
                     conf_level,
                     bound_type,
                     include_omega,
                     sd_df = c("satterthwaite", "components")) {
  sd_df = match.arg(sd_df)
  z = qnorm((1 + pred_level) / 2)
  n = nrow(bias_tab)
  K = nrow(varcomp)
  s = nu = numeric(n)
  shares = df_comp = matrix(0, n, K)
  for (j in seq_len(n)) {
    w = mc_vc_weights(varcomp, bias_tab$method1[j], bias_tab$method2[j],
                      include_omega = include_omega)
    s2 = sum(w * varcomp$var)
    v_s2 = drop(t(w) %*% cov_v %*% w)
    s[j] = sqrt(s2)
    nu[j] = 2 * s2^2 / v_s2
    shares[j, ] = w * varcomp$var / s2
    df_comp[j, ] = ifelse(w > 0, varcomp$df, 0)
  }
  b = bias_tab$bias
  sem = bias_tab$se
  out = dplyr::tibble(method1 = bias_tab$method1,
                      method2 = bias_tab$method2,
                      bias = b,
                      sd = s,
                      df_sd = nu)
  # "satterthwaite" (default): one chi-square for s^2 with df nu, which
  # accounts for the covariance of the estimated components.
  # "components": MOVER over the components, each with its own df, treating
  # them as independent (kept for comparison in simulations).
  info = if (sd_df == "satterthwaite" || design$model_type == "single_2") {
    list(type = "df", nu = nu)
  } else {
    list(type = "comp", shares = shares, df = df_comp)
  }

  if (interval == "agreement") {
    side_level = switch(bound_type,
                        iu = conf_level,
                        joint = 1 - (1 - conf_level) / 2)
    u_s = sd_upper_at(s, side_level, info)
    lme_ci = sqrt(qnorm(side_level)^2 * sem^2 + z^2 * (u_s - s)^2)
    out$lower_loa = b - z * s
    out$upper_loa = b + z * s
    out$lower_loa_ci = out$lower_loa - lme_ci
    out$upper_loa_ci = out$upper_loa + lme_ci
  } else {
    lim = analytic_bounds(b = b,
                          s = s,
                          sem = sem,
                          df = bias_tab$df,
                          level = conf_level,
                          info = info,
                          pred_level = pred_level,
                          bound_type = bound_type,
                          independent = design$model_type == "single_2")
    out$lower.TL = lim$lower
    out$upper.TL = lim$upper
  }
  out
}

# repeatability ----
# Repeatability coefficient RC_m = z * sqrt(2) * sigma_m (Carstensen 2010,
# ch. 9), the limit for the absolute difference between two measurements by
# method m on the same item under the same conditions; its CI is the sigma
# CI scaled. For linked replicates, the reproducibility coefficient
# RpC_m = z * sqrt(2) * sqrt(sigma_m^2 + omega^2) covers two measurements on
# different occasions, with a CI on the log-SD scale from the delta method.
# Not available without replicates.
mc_vc_repeatability = function(varcomp,
                               cov_v,
                               design,
                               pred_level,
                               conf_level) {
  if (design$model_type != "reps") {
    return(NULL)
  }
  z = qnorm((1 + pred_level) / 2)
  q = qnorm((1 + conf_level) / 2)
  k = z * sqrt(2)
  is_sig = varcomp$component == "sigma"
  out = dplyr::tibble(method = varcomp$method[is_sig],
                      sigma = varcomp$sd[is_sig],
                      rc = k * varcomp$sd[is_sig],
                      rc_lower = k * varcomp$lower[is_sig],
                      rc_upper = k * varcomp$upper[is_sig])
  if (design$linked) {
    j_om = which(varcomp$component == "omega")
    rpc = lower = upper = numeric(nrow(out))
    for (i in seq_len(nrow(out))) {
      w = numeric(nrow(varcomp))
      w[which(is_sig)[i]] = 1
      w[j_om] = 1
      v = sum(w * varcomp$var)
      se_log_sd = sqrt(drop(t(w) %*% cov_v %*% w)) / (2 * v)
      rpc[i] = k * sqrt(v)
      lower[i] = rpc[i] * exp(-q * se_log_sd)
      upper[i] = rpc[i] * exp(q * se_log_sd)
    }
    out$rpc = rpc
    out$rpc_lower = lower
    out$rpc_upper = upper
  }
  out
}

# diagnostics ----
# Fitted values, Pearson residuals, and predicted random effects, stored in
# the object so check() works without the model.
mc_vc_diag = function(fit,
                      df,
                      design) {
  if (inherits(fit, "lme")) {
    fitted_v = stats::fitted(fit, level = ncol(fit$fitted) - 1)
    resid_v = stats::residuals(fit, type = "pearson",
                               level = ncol(fit$fitted) - 1)
  } else {
    fitted_v = stats::fitted(fit)
    resid_v = stats::residuals(fit, type = "pearson")
  }
  res = dplyr::tibble(method = df$method,
                      id = df$id,
                      fitted = unname(fitted_v),
                      resid = unname(resid_v))

  re = NULL
  if (inherits(fit, "lme")) {
    rr = nlme::ranef(fit)
    if (is.data.frame(rr)) {
      rr = list(id = rr)
    }
    parts = list()
    if (design$model_type == "reps") {
      for (cn in names(rr$id)) {
        parts[[cn]] = dplyr::tibble(
          effect = "Method-by-item",
          method = sub("^method", "", cn),
          value = rr$id[[cn]]
        )
      }
      if (!is.null(rr$repl)) {
        parts$repl = dplyr::tibble(effect = "Item-by-replicate",
                                   method = NA_character_,
                                   value = rr$repl[[1]])
      }
    } else {
      parts$item = dplyr::tibble(effect = "Item",
                                 method = NA_character_,
                                 value = rr$id[[1]])
    }
    re = dplyr::bind_rows(parts)
  }
  list(resid = res, ranef = re)
}

# results on the original scale ----
# A difference d on the analysis scale, centered on the level t (the average
# of the two measurements on that scale), is
#   h(d) = inv(t + d / 2) - inv(t - d / 2)
# in original units. For fixed t, h is increasing in d wherever inv is
# increasing, so quantiles (limits), confidence bounds, and the median
# difference (the back-transformed bias) carry over exactly.
mc_back_diff = function(tf, t, d) {
  lo = t - d / 2
  hi = t + d / 2
  # ranges where the inverse is increasing
  ok = switch(tf$name,
              sqrt = lo >= 0 & hi >= 0,
              asinsqrt = lo >= 0 & hi >= 0 & lo <= pi / 2 & hi <= pi / 2,
              rep(TRUE, length(d)))
  out = tf$inv(hi) - tf$inv(lo)
  out[!ok] = NA_real_
  out
}

# Bias, limits, and repeatability coefficients in original units at one or
# more levels. `at` is on the original scale; NULL uses the median of the
# item averages on the analysis scale. Returns the limits table, with the
# repeatability table (if any) in attribute "repeatability".
mc_original_scale = function(x,
                             at = NULL) {
  tf = x$transform
  if (is.null(at)) {
    item_avg = tapply(x$data$y_t, x$data$id, mean)
    t_at = stats::median(item_avg)
    at = tf$inv(t_at)
    at_median = TRUE
  } else {
    if (!is.numeric(at) || length(at) < 1 || anyNA(at)) {
      stop("`at` must be a numeric vector of levels on the original scale.",
           call. = FALSE)
    }
    if (!isTRUE(tf$domain_check(at))) {
      stop("`at` must be within the domain of transform '", tf$name, "' (",
           tf$domain, ").", call. = FALSE)
    }
    t_at = tf$fun(at)
    at_median = FALSE
  }
  is_tol = x$design$interval == "tolerance"
  b = x$bias
  l = x$limits
  cols = if (is_tol) c("lower.TL", "upper.TL") else
    c("lower_loa", "upper_loa", "lower_loa_ci", "upper_loa_ci")

  lim = do.call(rbind, lapply(seq_along(t_at), function(j) {
    h = function(d) mc_back_diff(tf, t_at[j], d)
    out = data.frame(method1 = b$method1, method2 = b$method2, at = at[j],
                     bias = h(b$bias), lower.CL = h(b$lower.CL),
                     upper.CL = h(b$upper.CL))
    for (nm in cols) out[[nm]] = h(l[[nm]])
    out
  }))

  rp = x$repeatability
  if (!is.null(rp)) {
    rc_cols = intersect(c("rc", "rc_lower", "rc_upper",
                          "rpc", "rpc_lower", "rpc_upper"), names(rp))
    rp = do.call(rbind, lapply(seq_along(t_at), function(j) {
      out = data.frame(method = rp$method, at = at[j])
      for (nm in rc_cols) out[[nm]] = mc_back_diff(tf, t_at[j], rp[[nm]])
      out
    }))
  }
  structure(lim, repeatability = rp, at_median = at_median)
}
