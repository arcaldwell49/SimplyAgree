# Maximum likelihood fit of the scaled model (lavaan) ----
# The structural scaled model (true values mu_i ~ N),
#   y_mir = alpha_m + beta_m (mu_i + a_ir + c_mi) + e_mir,
# is a confirmatory factor model, fit by full ML with lavaan:
#   - wide data, one row per item, one column per method x replicate
#     (y<m>_<r>, m = method index, r = replicate index);
#   - factors for the true value (mu), each replicate (a<r>, linked only) and
#     each method (c<m>), all loading b<m> on method m (b1 fixed at 1);
#   - common om2 (linked) and t2, residual variance s<m> per method,
#     intercepts a<m> (a1 fixed at 0). With exchangeable replicates the
#     intercepts, loadings and residual variances are equal within method,
#     so the column order within an item does not matter.
#   - missing = "ml" for unbalanced data.
# Conversions for every ordered pair (to k, from m) are `:=` parameters, so
# their SEs come from lavaan (delta method on the ML covariance):
#   bc_k_m = b_k / b_m, ac_k_m = a_k - a_m b_k / b_m,
#   sd_k_m = sqrt(bc^2 (b_m^2 t2 + s_m) + b_k^2 t2 + s_k).
# Coverage of the Wald intervals with t(I - 1) quantiles was checked by
# simulation (data-raw/sims/sim_method_convert_lavaan.R): 92-97% for 95%
# intervals of the intercepts and slopes with 15-60 items, 2 or 3 methods,
# linked or exchangeable replicates, missing data, and non-normal true
# values; 92-95% for sd_pred on the log scale.

# lavaan is in Suggests (a wrapper, so tests can mock it)
mc_has_lavaan = function() {
  requireNamespace("lavaan", quietly = TRUE)
}

# column index of each row of the processed data in the wide data;
# exchangeable replicates are renumbered within item and method so that the
# observed values fill the first columns
mc_lav_index = function(df,
                        linked) {
  m = as.integer(df$method)
  r = if (linked) {
    as.integer(factor(df$repl))
  } else {
    stats::ave(seq_along(df$y_t), df$id, m, FUN = seq_along)
  }
  list(m = m, r = r, key = paste0("y", m, "_", r))
}

mc_lav_wide = function(df,
                       idx) {
  ids = levels(droplevels(df$id))
  cols = unique(idx$key[order(idx$m, idx$r)])
  w = matrix(NA_real_, length(ids), length(cols),
             dimnames = list(NULL, cols))
  w[cbind(match(as.character(df$id), ids), match(idx$key, cols))] = df$y_t
  as.data.frame(w)
}

# model syntax from the wide column names; `start` is a list with b, a, s
# (one per method; b[1] = 1, a[1] = 0), mu_mean, mu_var, t2, om2
mc_lav_syntax = function(cols,
                         linked,
                         start) {
  m = as.integer(sub("^y([0-9]+)_.*", "\\1", cols))
  r = as.integer(sub("^y[0-9]+_([0-9]+)$", "\\1", cols))
  M = max(m)
  ld = ifelse(m == 1, "1", paste0("b", m))
  load = function(sel) {
    free = sel & m > 1
    paste(c(paste0(ld[sel], "*", cols[sel]),
            sprintf("start(%.8g)*%s", start$b[m[free]], cols[free])),
          collapse = " + ")
  }

  syn = c(paste("mu =~", load(rep(TRUE, length(cols)))),
          sprintf("mu ~~ start(%.8g)*mu", start$mu_var),
          sprintf("mu ~ start(%.8g)*1", start$mu_mean))
  if (linked) {
    for (rr in sort(unique(r))) {
      syn = c(syn,
              sprintf("a%d =~ %s", rr, load(r == rr)),
              sprintf("a%d ~~ om2*a%d + start(%.8g)*a%d", rr, rr, start$om2,
                      rr))
    }
  }
  for (mm in seq_len(M)) {
    syn = c(syn,
            sprintf("c%d =~ %s", mm, load(m == mm)),
            sprintf("c%d ~~ t2*c%d + start(%.8g)*c%d", mm, mm, start$t2, mm))
  }
  syn = c(syn,
          sprintf("%s ~~ s%d*%s + start(%.8g)*%s", cols, m, cols,
                  start$s[m], cols),
          ifelse(m == 1,
                 sprintf("%s ~ 0*1", cols),
                 sprintf("%s ~ a%d*1 + start(%.8g)*1", cols, m, start$a[m])))

  b = function(k) if (k == 1) "1" else paste0("b", k)
  a = function(k) if (k == 1) "0" else paste0("a", k)
  for (k in seq_len(M)) {
    for (mm in seq_len(M)) {
      if (k == mm) next
      bc = sprintf("(%s)/(%s)", b(k), b(mm))
      syn = c(syn,
              sprintf("bc_%d_%d := %s", k, mm, bc),
              sprintf("ac_%d_%d := %s - (%s)*%s", k, mm, a(k), a(mm), bc),
              sprintf(paste0("sd_%d_%d := sqrt((%s)^2*((%s)^2*t2 + s%d) + ",
                             "(%s)^2*t2 + s%d)"),
                      k, mm, bc, b(mm), mm, b(k), k))
    }
  }
  paste(syn, collapse = "\n")
}

# starting values from an mc_altreg() fit, on the scale of the first
# method (the alternating regression's own scale is arbitrary)
mc_lav_start = function(alt,
                        n_items) {
  b = alt$b / alt$b[1]
  a = alt$a - b * alt$a[1]
  mu = alt$a[1] + alt$b[1] * alt$mu
  v_mu = stats::var(mu)
  # undo the I / (I - 2) factor; keep variances away from zero
  floor_v = 1e-3 * v_mu
  list(b = unname(b), a = unname(a),
       s = pmax(alt$sigma_y^2, floor_v),
       mu_mean = mean(mu), mu_var = v_mu,
       t2 = max((alt$tau_z * alt$b[1] * (n_items - 2) / n_items)^2, floor_v),
       om2 = max((alt$omega_z * alt$b[1])^2, floor_v))
}

# fit; returns the pieces used by mc_convert_scaled()
mc_scaled_ml = function(df,
                        alt,
                        linked,
                        conf_level) {
  if (!mc_has_lavaan()) {
    stop("estimator = 'ml' requires the lavaan package. Install it with ",
         "install.packages(\"lavaan\"), or use estimator = 'altreg'.",
         call. = FALSE)
  }
  lv = levels(df$method)
  M = length(lv)
  n_items = length(unique(df$id))
  idx = mc_lav_index(df, linked)
  w = mc_lav_wide(df, idx)
  syn = mc_lav_syntax(names(w), linked = linked,
                      start = mc_lav_start(alt, n_items))

  # negative variances are reported by our own warning below
  fit = withCallingHandlers(
    lavaan::lavaan(syn, data = w, missing = "ml", meanstructure = TRUE,
                   estimator = "ML"),
    warning = function(cnd) {
      if (grepl("variances are negative|not positive definite",
                conditionMessage(cnd))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  converged = lavaan::lavInspect(fit, "converged")
  pe = lavaan::parameterEstimates(fit, ci = FALSE)
  par = function(lab) pe$est[match(lab, pe$label)]

  b = c(1, par(paste0("b", seq_len(M))[-1]))
  a = c(0, par(paste0("a", seq_len(M))[-1]))
  s = par(paste0("s", seq_len(M)))
  t2 = par("t2")
  om2 = if (linked) par("om2") else 0
  v_mu = pe$est[pe$lhs == "mu" & pe$op == "~~" & pe$rhs == "mu"]
  names(a) = names(b) = names(s) = lv

  if (converged && v_mu <= 0) {
    stop("The maximum likelihood fit gave a variance of the true values ",
         "that is not positive, so the scaled model is not identified for ",
         "these data. Use estimator = 'altreg'.", call. = FALSE)
  }
  vars = c(t2 = t2, om2 = if (linked) om2, stats::setNames(s, paste0("s_",
                                                                     lv)))
  heywood = names(vars)[vars <= 1e-8]

  ## conversion table ----
  df_t = n_items - 1
  q = stats::qt((1 + conf_level) / 2, df_t)
  dp = pe[pe$op == ":=", ]
  rows = list()
  for (k in seq_len(M)) {
    for (m in seq_len(M)) {
      if (k == m) {
        rows[[length(rows) + 1]] = dplyr::tibble(
          to = lv[k], from = lv[m], alpha = 0, beta = 1,
          sd_pred = sqrt(2 * max(s[[m]], 0)),
          alpha_se = NA_real_, beta_se = NA_real_, sd_se = NA_real_,
          df = NA_real_)
        next
      }
      g = function(pre) dp[dp$lhs == sprintf("%s_%d_%d", pre, k, m), ]
      ac = g("ac")
      bc = g("bc")
      sd = g("sd")
      rows[[length(rows) + 1]] = dplyr::tibble(
        to = lv[k], from = lv[m], alpha = ac$est, beta = bc$est,
        sd_pred = sd$est, alpha_se = ac$se, beta_se = bc$se, sd_se = sd$se,
        df = df_t)
    }
  }
  conversion = dplyr::bind_rows(rows)
  conversion$alpha_lower = conversion$alpha - q * conversion$alpha_se
  conversion$alpha_upper = conversion$alpha + q * conversion$alpha_se
  conversion$beta_lower = conversion$beta - q * conversion$beta_se
  conversion$beta_upper = conversion$beta + q * conversion$beta_se
  # prediction SDs: Wald on the log scale
  ok = !is.na(conversion$sd_se) & conversion$sd_pred > 0
  conversion$sd_lower = NA_real_
  conversion$sd_upper = NA_real_
  conversion$sd_lower[ok] = conversion$sd_pred[ok] *
    exp(-q * conversion$sd_se[ok] / conversion$sd_pred[ok])
  conversion$sd_upper[ok] = conversion$sd_pred[ok] *
    exp(q * conversion$sd_se[ok] / conversion$sd_pred[ok])
  conversion$sd_se = NULL

  ## variance components (SDs on each method's scale) ----
  rt = function(v) sqrt(pmax(v, 0))
  varcomp = dplyr::tibble(component = rep(c("tau", "omega", "sigma"),
                                          each = M),
                          method = rep(lv, 3),
                          sd = unname(c(b * rt(t2), b * rt(om2), rt(s))),
                          lower = NA_real_,
                          upper = NA_real_)
  if (!linked) {
    varcomp = varcomp[varcomp$component != "omega", ]
  }

  ## item values and residuals (from the factor scores) ----
  fs = lavaan::lavPredict(fit, type = "lv")
  row = match(as.character(df$id), levels(droplevels(df$id)))
  eta = fs[row, "mu"] + fs[cbind(row, match(paste0("c", idx$m),
                                            colnames(fs)))]
  if (linked) {
    eta = eta + fs[cbind(row, match(paste0("a", idx$r), colnames(fs)))]
  }
  eta = unname(eta)
  diagnostics = dplyr::tibble(
    method = df$method,
    id = df$id,
    fitted = eta,
    resid = (df$y_t - a[idx$m] - b[idx$m] * eta) / sqrt(pmax(s[idx$m],
                                                           1e-12))
  )
  mu = stats::setNames(unname(fs[, "mu"]), levels(droplevels(df$id)))

  fm = lavaan::fitMeasures(fit, c("logl", "chisq", "df", "pvalue", "rmsea",
                                  "srmr"))
  list(conversion = conversion,
       varcomp = varcomp,
       methods = dplyr::tibble(method = lv, alpha = unname(a),
                               beta = unname(b)),
       mu = mu,
       mu_scale = c(tau = rt(t2), omega = rt(om2),
                    stats::setNames(rt(s) / b, paste0("sigma_", lv))),
       diagnostics = diagnostics,
       fit = fit,
       converged = converged,
       iterations = lavaan::lavInspect(fit, "iterations"),
       heywood = heywood,
       logl = unname(fm[["logl"]]),
       fit_measures = fm[c("chisq", "df", "pvalue", "rmsea", "srmr")])
}

# whether a simple_mconv fit used the ML estimator (fits made before the
# estimator argument have no fit_info$estimator)
mc_is_ml = function(fi) {
  identical(fi$estimator, "ml")
}

# one or two lines describing the scaled model fit, for print and summary
mc_cat_fit = function(fi,
                      fit_measures = FALSE) {
  conv = if (isTRUE(fi$converged)) "converged" else "NOT converged"
  if (!mc_is_ml(fi)) {
    cat("Alternating regression: ", fi$iterations, " iterations, ", conv,
        " (criterion ", signif(fi$criterion, 3), ")\n", sep = "")
    return(invisible(NULL))
  }
  cat("Maximum likelihood (lavaan): ", fi$iterations, " iterations, ", conv,
      "; log-likelihood ", format(round(fi$logl, 2), nsmall = 2), "\n",
      sep = "")
  if (length(fi$heywood) > 0) {
    cat("Zero or negative variance estimate(s): ",
        paste(fi$heywood, collapse = ", "),
        "; intervals may be unreliable\n", sep = "")
  }
  if (fit_measures && !is.null(fi$fit_measures)) {
    fm = fi$fit_measures
    cat("Model fit: chi-square ", format(round(fm[["chisq"]], 2), nsmall = 2),
        " on ", fm[["df"]], " df (p ", format.pval(fm[["pvalue"]], digits = 3,
                                                  eps = 0.001),
        "), RMSEA ", round(fm[["rmsea"]], 3), ", SRMR ", round(fm[["srmr"]], 3),
        "\n  (a poor fit means the normal model for the true values and ",
        "errors is questionable)\n", sep = "")
  }
  invisible(NULL)
}
