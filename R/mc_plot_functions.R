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
      l = neg_swap(l, "lower.TL", "upper.TL")
    }
    b$method1 = l$method1 = pair[1]
    b$method2 = l$method2 = pair[2]
  }
  list(m = pair[1], k = pair[2], bias = b, limits = l)
}

# Bland-Altman plot for one pair of a simple_mvc object ----
mc_ba_plot = function(x,
                      pair = NULL,
                      pair_seed = NULL) {
  pr = mc_pair_rows(x, pair)
  m = pr$m
  k = pr$k
  tf = x$transform
  des = x$design
  pp = mc_pair(x$data, m, k, replicates = des$replicates, seed = pair_seed)

  b = pr$bias
  l = pr$limits
  is_tol = des$interval == "tolerance"
  lim_lo = if (is_tol) l$lower.TL else l$lower_loa
  lim_hi = if (is_tol) l$upper.TL else l$upper_loa
  lim_lab = if (is_tol) c("Upper TL", "Bias", "Lower TL") else
    c("Upper LoA", "Bias", "Lower LoA")

  lines = data.frame(text = factor(lim_lab, levels = lim_lab),
                     y = c(lim_hi, b$bias, lim_lo))
  bands = data.frame(text = factor(lim_lab[2], levels = lim_lab),
                     ymin = b$lower.CL,
                     ymax = b$upper.CL)
  if (!is_tol) {
    bands = rbind(bands,
                  data.frame(text = factor(lim_lab[c(1, 3)], levels = lim_lab),
                             ymin = c(l$upper_loa, l$lower_loa_ci),
                             ymax = c(l$upper_loa_ci, l$lower_loa)))
  }

  pts = data.frame(avg = pp$avg, d = pp$d)
  # display scale ----
  if (tf$name == "log") {
    pts$avg = exp(pts$avg)
    pts$d = exp(pts$d)
    lines$y = exp(lines$y)
    bands$ymin = exp(bands$ymin)
    bands$ymax = exp(bands$ymax)
    x_lab = paste0("Geometric mean of ", m, " & ", k)
    y_lab = paste0("Ratio of methods (", m, " / ", k, ")")
  } else if (tf$name == "none") {
    x_lab = paste0("Average of ", m, " & ", k)
    y_lab = paste0("Difference between methods (", m, " - ", k, ")")
  } else {
    x_lab = paste0("Average of ", tf$label(m), " & ", tf$label(k))
    y_lab = paste0("Difference (", tf$label(m), " - ", tf$label(k), ")")
  }

  conf_level = des$conf_level
  lim_ci_level = if (des$bound_type == "joint") conf_level else
    1 - 2 * (1 - conf_level)
  cap = paste0(
    if (is_tol) "Tolerance limits: " else "Agreement = ",
    des$pred_level * 100, "%",
    if (is_tol) paste0(" content, ", conf_level * 100, "% confidence (",
                       des$bound_type, ")"),
    "\nConfidence Level = ", conf_level * 100, "% (Bias)",
    if (!is_tol) paste0(" & ", lim_ci_level * 100, "% (LoA)"),
    if (des$replicates == "exchangeable" && des$has_reps)
      "\nExchangeable replicates paired at random"
  )

  ggplot(pts, aes(x = avg, y = d)) +
    geom_rect(data = bands,
              aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
                  fill = text),
              alpha = 0.25,
              inherit.aes = FALSE) +
    geom_hline(data = lines,
               aes(yintercept = y, color = text),
               linewidth = 0.8) +
    geom_point(na.rm = TRUE) +
    scale_color_viridis_d(option = "C", end = .8, drop = FALSE) +
    scale_fill_viridis_d(option = "C", end = .8, drop = FALSE) +
    labs(x = x_lab, y = y_lab, caption = cap, color = "", fill = "") +
    theme_bw() +
    theme(legend.position = "left")
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
              signif(cv$alpha, 4), " + ", signif(cv$beta, 4), " * ",
              if (tf$name != "none") tf$label(from) else from)
  cap = paste0(eq, "\n", x$fit_info$pred_level * 100,
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
# for log), with the bias and prediction limit lines of the model
mc_conv_ba_plot = function(x,
                           pair = NULL) {
  pair = mc_conv_pair(x, pair)
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
    x_lab = paste0("Geometric mean of ", m, " & ", k)
    y_lab = paste0("Ratio of methods (", m, " / ", k, ")")
  } else if (tf$name == "none") {
    x_lab = paste0("Average of ", m, " & ", k)
    y_lab = paste0("Difference between methods (", m, " - ", k, ")")
  } else {
    x_lab = paste0("Average of ", tf$label(m), " & ", tf$label(k))
    y_lab = paste0("Difference (", tf$label(m), " - ", tf$label(k), ")")
  }
  ggplot(pts, aes(x = avg, y = d)) +
    geom_ribbon(data = lines, aes(x = avg, ymin = lo, ymax = hi),
                inherit.aes = FALSE, alpha = 0.15, fill = "#0D0887") +
    geom_line(data = lines, aes(x = avg, y = bias), color = "#B12A90",
              linewidth = 0.8, inherit.aes = FALSE) +
    geom_line(data = lines, aes(x = avg, y = lo), color = "#0D0887",
              linetype = 2, inherit.aes = FALSE) +
    geom_line(data = lines, aes(x = avg, y = hi), color = "#0D0887",
              linetype = 2, inherit.aes = FALSE) +
    geom_point(na.rm = TRUE) +
    labs(x = x_lab, y = y_lab,
         caption = paste0("Bias and ", x$fit_info$pred_level * 100,
                          "% prediction limits (", x$fit_info$bias,
                          " bias)")) +
    theme_bw()
}
