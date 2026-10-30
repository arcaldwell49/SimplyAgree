# Plot builders for method_vc() and method_convert() results ----

# orient a pair ----
# Returns the rows of bias and limits for the pair c(m, k), with signs and
# limits flipped when the stored pair is (k, m).
mc_pair_rows = function(x,
                        pair = NULL) {
  lv = levels(x$data$method)
  if (is.null(pair)) {
    pair = c(x$bias$method1[1], x$bias$method2[1])
  }
  pair = as.character(pair)
  if (length(pair) != 2 || !all(pair %in% lv) || pair[1] == pair[2]) {
    stop("`pair` must name two different methods from: ",
         paste(lv, collapse = ", "), ".", call. = FALSE)
  }
  j = which(x$bias$method1 == pair[1] & x$bias$method2 == pair[2])
  flip = length(j) == 0
  if (flip) {
    j = which(x$bias$method1 == pair[2] & x$bias$method2 == pair[1])
  }
  b = x$bias[j, ]
  l = x$limits[j, ]
  if (flip) {
    neg_swap = function(df, lo, hi) {
      tmp = df[[lo]]
      df[[lo]] = -df[[hi]]
      df[[hi]] = -tmp
      df
    }
    b$bias = -b$bias
    b = neg_swap(b, "lower.CL", "upper.CL")
    l$bias = -l$bias
    if ("lower_loa" %in% names(l)) {
      l = neg_swap(l, "lower_loa", "upper_loa")
      l = neg_swap(l, "lower_loa_ci", "upper_loa_ci")
    } else {
      l = neg_swap(l, "lower.PL", "upper.PL")
      l = neg_swap(l, "lower.TL", "upper.TL")
    }
    b$method1 = l$method1 = pair[1]
    b$method2 = l$method2 = pair[2]
  }
  list(m = pair[1], k = pair[2], bias = b, limits = l)
}

# base layer of a Bland-Altman plot (columns avg and d), with the geom
# options of plot.loa()
mc_ba_base = function(pts,
                      geom = "geom_point") {
  p = ggplot(pts, aes(x = avg, y = d))
  switch(
    geom,
    geom_point = p + geom_point(na.rm = TRUE),
    geom_bin2d = p + geom_bin2d(na.rm = TRUE),
    geom_density_2d = p + geom_density_2d(na.rm = TRUE),
    geom_density_2d_filled = p +
      geom_density_2d_filled(na.rm = TRUE,
                             alpha = 0.5,
                             contour_var = "ndensity"),
    stat_density_2d = p +
      stat_density_2d(na.rm = TRUE,
                      geom = "polygon",
                      contour = TRUE,
                      aes(fill = after_stat(level)),
                      contour_var = "ndensity",
                      colour = "black") +
      scale_fill_distiller(palette = "Blues", direction = 1),
    stop("geom option not supported", call. = FALSE)
  )
}

# maximal allowable difference lines, as in plot.loa(); a single value is
# +/- delta, or delta and 1/delta for ratios
mc_ba_delta = function(p,
                       delta,
                       ratio = FALSE) {
  if (is.null(delta)) {
    return(p)
  }
  if (!is.numeric(delta) || !(length(delta) %in% 1:2) ||
      any(!is.finite(delta)) || (ratio && any(delta <= 0))) {
    stop("`delta` must be one or two numbers",
         if (ratio) " greater than 0 (ratios)", ".", call. = FALSE)
  }
  if (length(delta) == 1) {
    delta = if (ratio) c(delta, 1 / delta) else c(delta, -delta)
  }
  p +
    geom_hline(data = data.frame(y1 = delta),
               aes(yintercept = y1),
               linetype = 2) +
    scale_y_continuous(sec.axis = dup_axis(
      breaks = delta,
      name = "Maximal Allowable Difference"))
}

# axis labels for the differences of a pair on the display scale
mc_ba_labels = function(tf,
                        m,
                        k) {
  if (tf$name == "log") {
    list(x = paste0("Geometric mean of ", m, " & ", k),
         y = paste0("Ratio of methods (", m, " / ", k, ")"))
  } else if (tf$name == "none") {
    list(x = paste0("Average of ", m, " & ", k),
         y = paste0("Difference between methods (", m, " - ", k, ")"))
  } else {
    list(x = paste0("Average of ", tf$label(m), " & ", tf$label(k)),
         y = paste0("Difference (", tf$label(m), " - ", tf$label(k), ")"))
  }
}

# Bland-Altman plot for one pair of a simple_mvc object ----
# The bias is constant, so the bias and limits are drawn as points with
# their confidence intervals to the left of the data, as in plot.loa().
mc_ba_plot = function(x,
                      pair = NULL,
                      pair_seed = NULL,
                      geom = "geom_point",
                      delta = NULL) {
  pr = mc_pair_rows(x, pair)
  m = pr$m
  k = pr$k
  tf = x$transform
  des = x$design
  pp = mc_pair(x$data, m, k, replicates = des$replicates, seed = pair_seed)

  b = pr$bias
  l = pr$limits
  is_tol = des$interval == "tolerance"
  # points at the limits (LoA, or prediction limits), whiskers out to their
  # confidence bounds (LoA CI, or tolerance limits)
  lim_lo = if (is_tol) l$lower.PL else l$lower_loa
  lim_hi = if (is_tol) l$upper.PL else l$upper_loa
  bnd_lo = if (is_tol) l$lower.TL else l$lower_loa_ci
  bnd_hi = if (is_tol) l$upper.TL else l$upper_loa_ci
  lim_lab = if (is_tol) c("Upper Limit", "Bias", "Lower Limit") else
    c("Upper LoA", "Bias", "Lower LoA")

  df_loa = data.frame(text = factor(lim_lab, levels = lim_lab),
                      estimate = c(lim_hi, b$bias, lim_lo),
                      lower.ci = c(lim_hi, b$lower.CL, bnd_lo),
                      upper.ci = c(bnd_hi, b$upper.CL, lim_lo))

  pts = data.frame(avg = pp$avg, d = pp$d)
  # display scale ----
  if (tf$name == "log") {
    pts$avg = exp(pts$avg)
    pts$d = exp(pts$d)
    df_loa[c("estimate", "lower.ci", "upper.ci")] =
      exp(df_loa[c("estimate", "lower.ci", "upper.ci")])
  }
  labs_xy = mc_ba_labels(tf, m, k)
  rng = range(pts$avg, na.rm = TRUE)
  df_loa$x = rng[1]
  pd2 = position_dodge2(.03 * (rng[2] - rng[1]))

  conf_level = des$conf_level
  lim_ci_level = if (des$bound_type == "joint") conf_level else
    1 - 2 * (1 - conf_level)
  cap = paste0(
    if (is_tol) "Prediction = " else "Agreement = ",
    mc_pct(des$pred_level), "%",
    if (is_tol) paste0("; tolerance limits (bands): ",
                       mc_pct(des$pred_level), "% content, ",
                       conf_level * 100, "% confidence (",
                       des$bound_type, ")"),
    "\nConfidence Level = ", conf_level * 100, "% (Bias)",
    if (!is_tol) paste0(" & ", lim_ci_level * 100, "% (LoA)"),
    if (des$replicates == "exchangeable" && des$has_reps)
      "\nExchangeable replicates paired at random"
  )

  p = mc_ba_base(pts, geom = geom) +
    geom_pointrange(data = df_loa,
                    aes(x = x,
                        y = estimate,
                        ymin = lower.ci,
                        ymax = upper.ci,
                        color = text),
                    position = pd2,
                    inherit.aes = FALSE) +
    scale_color_viridis_d(option = "C", end = .8, drop = FALSE) +
    labs(x = labs_xy$x, y = labs_xy$y, caption = cap, color = "") +
    theme_bw() +
    theme(legend.position = "left")
  mc_ba_delta(p, delta, ratio = tf$name == "log")
}

# Matrix plot (MethComp plot.Meth layout) ----
# Diagonal: method names. Upper triangle (row r, column c): method c against
# method r on the original scale, with the identity line (dashed) and the
# constant-bias conversion line with prediction limits. Lower triangle:
# Bland-Altman plot for the pair (column method - row method).
mc_matrix_plot = function(x,
                          pair_seed = NULL) {
  lv = levels(x$data$method)
  M = length(lv)
  if (M > 5) {
    message("With ", M, " methods the matrix plot may be crowded; ",
            "consider plot(x, type = 'ba', pair = ...) for single pairs.")
  }
  tf = x$transform
  z = qnorm((1 + x$design$pred_level) / 2)
  panels = vector("list", M * M)

  for (r in seq_len(M)) {
    for (cc in seq_len(M)) {
      idx = (r - 1) * M + cc
      if (r == cc) {
        panels[[idx]] = ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = lv[r], size = 6) +
          xlim(0, 1) + ylim(0, 1) +
          theme_void() +
          theme(panel.border = element_rect(fill = NA, color = "grey50"))
      } else if (r < cc) {
        panels[[idx]] = mc_conv_panel(x, lv[r], lv[cc], z = z,
                                      pair_seed = pair_seed)
      } else {
        panels[[idx]] = mc_ba_plot(x, pair = c(lv[cc], lv[r]),
                                   pair_seed = pair_seed) +
          labs(x = NULL, y = NULL, caption = NULL,
               subtitle = paste0(lv[cc], " - ", lv[r])) +
          theme(legend.position = "none")
      }
    }
  }
  patchwork::wrap_plots(panels, ncol = M)
}

# scatter of method k (y) against method m (x) with the constant-bias
# conversion y_k = y_m + (alpha_k - alpha_m) and prediction limits,
# computed on the transformed scale and back-transformed
mc_conv_panel = function(x,
                         m,
                         k,
                         z,
                         pair_seed = NULL) {
  tf = x$transform
  pp = mc_pair(x$data, m, k, replicates = x$design$replicates,
               seed = pair_seed)
  pr = mc_pair_rows(x, c(k, m))
  b = pr$bias$bias          # k - m
  s = pr$limits$sd
  rng = range(x$data$y[x$data$method %in% c(m, k)])
  grid = seq(rng[1], rng[2], length.out = 100)
  # keep the grid inside the transformation's domain
  ok = vapply(grid, function(g) isTRUE(tf$domain_check(g)), logical(1))
  grid = grid[ok]
  gt = tf$fun(grid)
  lines = data.frame(x = grid,
                     fit = tf$inv(gt + b),
                     lwr = tf$inv(gt + b - z * s),
                     upr = tf$inv(gt + b + z * s))
  ggplot(data.frame(x = pp$y1_orig, y = pp$y2_orig), aes(x = x, y = y)) +
    geom_ribbon(data = lines, aes(x = x, ymin = lwr, ymax = upr),
                inherit.aes = FALSE, alpha = 0.2, fill = "#0D0887") +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey40") +
    geom_line(data = lines, aes(x = x, y = fit), color = "#0D0887",
              inherit.aes = FALSE) +
    geom_point(na.rm = TRUE, size = 1) +
    labs(x = m, y = k) +
    theme_bw()
}

# Variance component plot ----
# Point estimates and confidence intervals of the SDs, with the repeatability
# (and reproducibility) coefficients.
mc_vc_plot = function(x) {
  vc = x$varcomp
  comp_lab = c(tau = "tau (method x item)",
               omega = "omega (item x replicate)",
               sigma = "sigma (residual)")
  df = data.frame(facet = unname(comp_lab[vc$component]),
                  label = ifelse(is.na(vc$method), "common", vc$method),
                  est = vc$sd,
                  lower = vc$lower,
                  upper = vc$upper)
  rp = x$repeatability
  if (!is.null(rp)) {
    df = rbind(df, data.frame(facet = "RC (repeatability)",
                              label = rp$method,
                              est = rp$rc,
                              lower = rp$rc_lower,
                              upper = rp$rc_upper))
    if ("rpc" %in% names(rp)) {
      df = rbind(df, data.frame(facet = "RpC (reproducibility)",
                                label = rp$method,
                                est = rp$rpc,
                                lower = rp$rpc_lower,
                                upper = rp$rpc_upper))
    }
  }
  df$facet = factor(df$facet, levels = unique(df$facet))
  df$label = factor(df$label, levels = rev(unique(c("common",
                                                    levels(x$data$method)))))
  df$label = droplevels(df$label)
  tf_note = if (x$transform$name != "none") {
    paste0(" (", x$transform$name, " scale)")
  } else ""

  ggplot(df, aes(x = est, y = label, color = label)) +
    geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2,
                  orientation = "y",
                   na.rm = TRUE) +
    geom_point(size = 2) +
    facet_wrap(~ facet, scales = "free") +
    scale_color_viridis_d(option = "C", end = .8) +
    labs(x = paste0("Estimate", tf_note), y = NULL,
         caption = paste0(x$design$conf_level * 100,
                          "% confidence intervals")) +
    theme_bw() +
    theme(legend.position = "none")
}

# Plots for simple_mconv objects ----

# pair default and validation
mc_conv_pair = function(x,
                        pair) {
  lv = levels(x$data$method)
  if (is.null(pair)) {
    pair = lv[1:2]
  }
  pair = as.character(pair)
  if (length(pair) != 2 || !all(pair %in% lv) || pair[1] == pair[2]) {
    stop("`pair` must name two different methods from: ",
         paste(lv, collapse = ", "), ".", call. = FALSE)
  }
  pair
}

# paired data oriented as pair[1], pair[2]
mc_conv_pairdata = function(x,
                            pair) {
  key = paste0(pair[1], "-", pair[2])
  pp = x$pairs[[key]]
  if (!is.null(pp)) {
    return(data.frame(y1 = pp$y1, y2 = pp$y2,
                      y1_orig = pp$y1_orig, y2_orig = pp$y2_orig))
  }
  pp = x$pairs[[paste0(pair[2], "-", pair[1])]]
  data.frame(y1 = pp$y2, y2 = pp$y1,
             y1_orig = pp$y2_orig, y2_orig = pp$y1_orig)
}

# conversion plot: to (y) against from (x), original scale ----
mc_conversion_plot = function(x,
                              pair = NULL) {
  pair = mc_conv_pair(x, pair)
  from = pair[1]
  to = pair[2]
  pts = mc_conv_pairdata(x, pair)
  rng = range(c(pts$y1_orig, x$data$y[x$data$method == from]))
  grid = mc_conv_grid(x, from = from, to = to,
                      values = seq(rng[1], rng[2], length.out = 200))
  grid = grid[!is.na(grid$fit), ]
  cv = x$conversion[x$conversion$to == to & x$conversion$from == from, ]
  tf = x$transform
  eq = paste0(if (tf$name != "none") tf$label(to) else to, " = ",
              signif(cv$alpha, 4), if (cv$beta < 0) " - " else " + ",
              signif(abs(cv$beta), 4), " * ",
              if (tf$name != "none") tf$label(from) else from)
  cap = paste0(eq, "\n", mc_pct(x$fit_info$pred_level),
               "% prediction limits",
               if (tf$name != "none") " (back-transformed)")
  ggplot(data.frame(x = pts$y1_orig, y = pts$y2_orig), aes(x = x, y = y)) +
    geom_ribbon(data = grid,
                aes(x = from_value, ymin = lower, ymax = upper),
                inherit.aes = FALSE, alpha = 0.2, fill = "#0D0887") +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey40") +
    geom_line(data = grid, aes(x = from_value, y = fit),
              inherit.aes = FALSE, color = "#0D0887", linewidth = 0.8) +
    geom_point(na.rm = TRUE) +
    labs(x = from, y = to, caption = cap) +
    theme_bw()
}

# Bland-Altman plot implied by the conversion model ----
# differences pair[1] - pair[2] against averages (transformed scale, ratios
# for log), with the bias and prediction limit lines of the model. A
# conversion from a method_vc() fit has a constant bias and gets the plot
# of that fit (points with confidence intervals).
mc_conv_ba_plot = function(x,
                           pair = NULL,
                           geom = "geom_point",
                           delta = NULL) {
  pair = mc_conv_pair(x, pair)
  if (!is.null(x$vc_fit)) {
    vc = c(list(data = x$data, transform = x$transform), x$vc_fit)
    return(mc_ba_plot(vc, pair = pair, pair_seed = x$call$pair_seed,
                      geom = geom, delta = delta))
  }
  m = pair[1]
  k = pair[2]
  tf = x$transform
  z = qnorm((1 + x$fit_info$pred_level) / 2)
  pts = mc_conv_pairdata(x, pair)
  pts$avg = (pts$y1 + pts$y2) / 2
  pts$d = pts$y1 - pts$y2
  grid = seq(min(pts$avg), max(pts$avg), length.out = 100)

  par = x$limits_par[[paste0(m, "|", k)]]
  if (par$type == "constant") {
    lines = data.frame(avg = grid, bias = par$alpha,
                       lo = par$alpha - z * par$sd,
                       hi = par$alpha + z * par$sd)
  } else if (par$type == "scaled") {
    # y_m = alpha + beta y_k (sd) implies D = y_m - y_k =
    # 2 alpha / (1 + beta) + 2 (beta - 1) / (1 + beta) A, SD 2 sd / (1 + beta)
    # (MethComp::y2DA)
    slope = 2 * (par$beta - 1) / (1 + par$beta)
    int = 2 * par$alpha / (1 + par$beta)
    sd_d = 2 * par$sd / (1 + par$beta)
    lines = data.frame(avg = grid,
                       bias = int + slope * grid,
                       lo = int - z * sd_d + slope * grid,
                       hi = int + z * sd_d + slope * grid)
  } else {
    sgn = if (par$m1 == m) 1 else -1
    lo = sgn * ((par$a - z * par$c_sd) + (par$b - z * par$d_sd) * grid)
    hi = sgn * ((par$a + z * par$c_sd) + (par$b + z * par$d_sd) * grid)
    lines = data.frame(avg = grid,
                       bias = sgn * (par$a + par$b * grid),
                       lo = pmin(lo, hi),
                       hi = pmax(lo, hi))
  }
  if (tf$name == "log") {
    pts$avg = exp(pts$avg)
    pts$d = exp(pts$d)
    lines$avg = exp(lines$avg)
    lines[c("bias", "lo", "hi")] = exp(lines[c("bias", "lo", "hi")])
  }
  labs_xy = mc_ba_labels(tf, m, k)
  # the bias and limits vary with the level, so they are lines, as in
  # plot.loa() with prop_bias = TRUE (there are no confidence bands)
  lim_lab = c("Upper Limit", "Bias", "Lower Limit")
  df_lines = data.frame(text = factor(rep(lim_lab, each = nrow(lines)),
                                      levels = lim_lab),
                        x = rep(lines$avg, 3),
                        estimate = c(lines$hi, lines$bias, lines$lo))
  p = mc_ba_base(pts, geom = geom) +
    geom_line(data = df_lines,
              aes(x = x, y = estimate, color = text),
              linewidth = 0.8,
              inherit.aes = FALSE) +
    scale_color_viridis_d(option = "C", end = .8) +
    labs(x = labs_xy$x, y = labs_xy$y, color = "",
         caption = paste0("Prediction = ", mc_pct(x$fit_info$pred_level),
                          "% (", x$fit_info$bias, " bias",
                          if (x$fit_info$bias == "linear")
                            paste0(", ", x$fit_info$variance, " SD"),
                          ")")) +
    theme_bw() +
    theme(legend.position = "left",
          legend.title = element_blank())
  mc_ba_delta(p, delta, ratio = tf$name == "log")
}

# check() panels for simple_mvc and simple_mconv objects ----

# normality: QQ plots by group, with a Shapiro-Wilk test per group in the
# caption, in the style of check.loa()
mc_check_norm = function(y,
                         group,
                         title = "Normality of Residuals",
                         y_lab = "Standardized residuals",
                         scales = "fixed") {
  dat = data.frame(y = y, group = group)
  dat = dat[is.finite(dat$y), ]
  nt = lapply(split(dat$y, dat$group, drop = TRUE), .norm_test)
  p_val = vapply(nt, function(t) t$p.value, numeric(1))
  if (length(p_val) == 1) {
    p_val = unname(p_val)
  }
  test = paste(unique(vapply(nt, function(t) t$name, character(1))),
               collapse = " / ")
  plot_qq(dat) +
    facet_wrap(~ group, scales = scales) +
    labs(title = title,
         y = y_lab,
         caption = .check_caption("Normality", test, p_val)) +
    .check_theme()
}

# homogeneity of variance: sqrt(|standardized residuals|) against x by
# group, with a Breusch-Pagan test per group in the caption. Residuals are
# standardized by their SD within each group. Returns the plot and the
# p-values.
mc_check_het = function(x,
                        resid,
                        group,
                        x_lab) {
  dat = data.frame(x = x, r = resid, group = group)
  dat = dat[is.finite(dat$x) & is.finite(dat$r), ]
  p_val = vapply(split(dat, dat$group, drop = TRUE),
                 function(d) .bp_test(d$r, d$x), numeric(1))
  if (length(p_val) == 1) {
    p_val = unname(p_val)
  }
  dat$y = sqrt(abs(dat$r / stats::ave(dat$r, dat$group, FUN = stats::sd)))
  p = plot_het(dat, x_lab = x_lab) +
    facet_wrap(~ group, scales = "free") +
    labs(caption = .check_caption("Heteroskedasticity",
                                  "Breusch-Pagan Test", p_val)) +
    .check_theme()
  list(plot = p, p.value = p_val)
}
