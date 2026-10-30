# lavaan fit of the structural scaled model (Phase 1 prototype) ----
#   y_mir = alpha_m + beta_m (mu_i + a_ir + c_mi) + e_mir
# fit by full ML as a CFA with labelled equality constraints:
#   - wide data, one row per item, one column per method x replicate
#     (y<m>_<r>, m = method index, r = replicate index);
#   - factors for the true value (mu), each replicate (a<r>, linked only) and
#     each method (c<m>), all loading b<m> on method m (b1 fixed at 1);
#   - common om2 (linked) and t2, one residual variance s<m> per method,
#     intercepts a<m> (a1 fixed at 0). With exchangeable replicates the
#     intercepts, loadings and residual variances are equal within method,
#     so the column order within an item does not matter.
# Conversions for every ordered pair (to k, from m) are `:=` parameters:
#   bc_k_m = b_k / b_m, ac_k_m = a_k - a_m b_k / b_m,
#   sd_k_m = sqrt(bc^2 (b_m^2 t2 + s_m) + b_k^2 t2 + s_k)
# (Carstensen, 2010, section 7.1; sim_methods_truth()).
# Sourced by sim_method_convert_lavaan.R and lavaan_ox_compare.R.

# Long data (id, method, repl, y) to wide; `methods` gives the order (the
# first is the reference). Exchangeable replicates are renumbered 1, 2, ...
# within item and method so that the observed values fill the first columns.
lav_wide = function(dat, methods, replicates = c("linked", "exchangeable")) {
  replicates = match.arg(replicates)
  m_idx = match(as.character(dat$method), methods)
  r_idx = if (replicates == "linked") {
    as.integer(factor(dat$repl))
  } else {
    stats::ave(seq_along(dat$y), dat$id, m_idx, FUN = seq_along)
  }
  key = paste0("y", m_idx, "_", r_idx)
  ids = unique(dat$id)
  cols = unique(key[order(m_idx, r_idx)])
  w = matrix(NA_real_, length(ids), length(cols), dimnames = list(NULL, cols))
  w[cbind(match(dat$id, ids), match(key, cols))] = dat$y
  data.frame(id = ids, w)
}

# Moment-based starting values from the wide data (lavaan's defaults can
# land in a local optimum with a negative var(mu) when M = 3)
lav_scaled_start = function(w) {
  cols = grep("^y[0-9]+_[0-9]+$", names(w), value = TRUE)
  m = as.integer(sub("^y([0-9]+)_.*", "\\1", cols))
  M = max(m)
  xb = sapply(seq_len(M), function(k) rowMeans(w[, cols[m == k], drop = FALSE],
                                               na.rm = TRUE))
  ok = stats::complete.cases(xb)
  xb = xb[ok, , drop = FALSE]
  b = sign(stats::cor(xb[, 1], xb)) * apply(xb, 2, stats::sd) / stats::sd(xb[, 1])
  a = colMeans(xb) - b * mean(xb[, 1])
  s = sapply(seq_len(M), function(k) {
    v = apply(w[, cols[m == k], drop = FALSE], 1, stats::var, na.rm = TRUE)
    v = mean(v, na.rm = TRUE)
    if (is.finite(v) && v > 0) v else 0.1 * stats::var(xb[, k])
  })
  v_mu = stats::var(xb[, 1])
  list(b = as.numeric(b), a = as.numeric(a), s = s, mu_mean = mean(xb[, 1]),
       mu_var = v_mu, t2 = 0.05 * v_mu, om2 = 0.05 * v_mu)
}

# Model syntax from the wide column names (only columns that are present);
# `start` is a list from lav_scaled_start() or NULL (lavaan's defaults)
lav_scaled_syntax = function(cols, linked = TRUE, start = NULL) {
  cols = grep("^y[0-9]+_[0-9]+$", cols, value = TRUE)
  m = as.integer(sub("^y([0-9]+)_.*", "\\1", cols))
  r = as.integer(sub("^y[0-9]+_([0-9]+)$", "\\1", cols))
  M = max(m)
  ld = ifelse(m == 1, "1", paste0("b", m))       # loadings, b1 = 1
  st = !is.null(start)
  load = function(sel) {
    out = paste0(ld[sel], "*", cols[sel])
    free = sel & m > 1
    if (st && any(free)) {
      out = c(out, sprintf("start(%.6g)*%s", start$b[m[free]], cols[free]))
    }
    paste(out, collapse = " + ")
  }
  sv = function(lab, x) if (st) sprintf(" + start(%.6g)*%s", x, lab) else ""

  syn = c(paste("mu =~", load(rep(TRUE, length(cols)))),
          paste0("mu ~~ mu", sv("mu", start$mu_var)),
          paste0("mu ~ 1", if (st) sprintf(" + start(%.6g)*1", start$mu_mean)))
  if (linked) {
    for (rr in sort(unique(r))) {
      syn = c(syn, sprintf("a%d =~ %s", rr, load(r == rr)),
              paste0(sprintf("a%d ~~ om2*a%d", rr, rr),
                     sv(paste0("a", rr), start$om2)))
    }
  }
  for (mm in seq_len(M)) {
    syn = c(syn, sprintf("c%d =~ %s", mm, load(m == mm)),
            paste0(sprintf("c%d ~~ t2*c%d", mm, mm),
                   sv(paste0("c", mm), start$t2)))
  }
  syn = c(syn,
          paste0(sprintf("%s ~~ s%d*%s", cols, m, cols),
                 if (st) sprintf(" + start(%.6g)*%s", start$s[m], cols)),
          ifelse(m == 1, sprintf("%s ~ 0*1", cols),
                 paste0(sprintf("%s ~ a%d*1", cols, m),
                        if (st) sprintf(" + start(%.6g)*1", start$a[m]))))

  # conversions for every ordered pair
  b = function(k) if (k == 1) "1" else paste0("b", k)
  a = function(k) if (k == 1) "0" else paste0("a", k)
  for (k in seq_len(M)) for (mm in seq_len(M)) {
    if (k == mm) next
    bc = sprintf("(%s)/(%s)", b(k), b(mm))
    syn = c(syn,
            sprintf("bc_%d_%d := %s", k, mm, bc),
            sprintf("ac_%d_%d := %s - (%s)*%s", k, mm, a(k), a(mm), bc),
            sprintf("sd_%d_%d := sqrt((%s)^2*((%s)^2*t2 + s%d) + (%s)^2*t2 + s%d)",
                    k, mm, bc, b(mm), mm, b(k), k))
  }
  paste(syn, collapse = "\n")
}

# Fit; returns the lavaan object
lav_scaled_fit = function(dat, methods, replicates = c("linked", "exchangeable"),
                          estimator = "ML", start = TRUE, ...) {
  replicates = match.arg(replicates)
  w = lav_wide(dat, methods, replicates)
  sv = if (start) lav_scaled_start(w) else NULL
  lavaan::lavaan(lav_scaled_syntax(names(w), linked = replicates == "linked",
                                   start = sv),
                 data = w, missing = "ml", meanstructure = TRUE,
                 estimator = estimator, ...)
}

# Conversion table (to, from, alpha, beta, sd_pred with SEs and Wald CIs)
lav_scaled_conversion = function(fit, methods, level = 0.95) {
  pe = lavaan::parameterEstimates(fit, level = level)
  pe = pe[pe$op == ":=", ]
  get = function(pre) {
    p = pe[startsWith(pe$lhs, pre), ]
    k = as.integer(sub("^[a-z]+_([0-9]+)_.*", "\\1", p$lhs))
    mm = as.integer(sub(".*_([0-9]+)$", "\\1", p$lhs))
    data.frame(to = methods[k], from = methods[mm], est = p$est, se = p$se,
               lower = p$ci.lower, upper = p$ci.upper)
  }
  a = get("ac_"); b = get("bc_"); s = get("sd_")
  data.frame(to = a$to, from = a$from,
             alpha = a$est, alpha_se = a$se,
             alpha_lower = a$lower, alpha_upper = a$upper,
             beta = b$est, beta_se = b$se,
             beta_lower = b$lower, beta_upper = b$upper,
             sd_pred = s$est, sd_se = s$se)
}

# Variance estimates (mu, om2, t2, s<m>) for Heywood checks
lav_scaled_variances = function(fit) {
  pe = lavaan::parameterEstimates(fit)
  v = pe[pe$op == "~~" & pe$lhs == pe$rhs, ]
  v$label[v$lhs == "mu"] = "mu"
  v = v[!duplicated(v$label) & v$label != "", ]
  stats::setNames(v$est, v$label)
}
