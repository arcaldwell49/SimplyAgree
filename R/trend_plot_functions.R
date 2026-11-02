# Concordance (4-quadrant) and polar plots of changes ----
# Shared by plot.simple_mvc(), plot.loa() and plot.tolerance_delta() with
# type = "quadrant" or "polar" (trending_agreement_plan.md, item 2).

# Successive changes within each id: `df` has columns id, ord (occasion
# order), x and y (one row per occasion, both methods measured), and
# optionally x_orig, y_orig (original scale, for percentage changes). `log`:
# x and y are logs (percentage changes are then 100 (exp(change) - 1)).
trend_changes = function(df,
                         relative = FALSE,
                         log = FALSE) {
  df = df[order(df$id, df$ord), ]
  if (is.null(df$x_orig)) {
    df$x_orig = df$x
    df$y_orig = df$y
  }
  out = lapply(split(df, df$id, drop = TRUE), function(d) {
    n = nrow(d)
    if (n < 2) {
      return(NULL)
    }
    i = seq_len(n - 1)
    dx = d$x[i + 1] - d$x[i]
    dy = d$y[i + 1] - d$y[i]
    if (relative) {
      if (log) {
        dx = 100 * (exp(dx) - 1)
        dy = 100 * (exp(dy) - 1)
      } else {
        dx = 100 * (d$x_orig[i + 1] - d$x_orig[i]) / d$x_orig[i]
        dy = 100 * (d$y_orig[i + 1] - d$y_orig[i]) / d$y_orig[i]
      }
    }
    data.frame(id = d$id[i], dx = dx, dy = dy)
  })
  ch = do.call(rbind, out)
  if (is.null(ch) || nrow(ch) == 0) {
    stop("No item has measurements on two or more occasions, so there are ",
         "no changes to plot.", call. = FALSE)
  }
  ch$mean = (ch$dx + ch$dy) / 2
  ch
}

# check the plot arguments shared by both types
trend_check_args = function(exclusion,
                            relative,
                            expected) {
  if (!is.numeric(exclusion) || length(exclusion) != 1 || is.na(exclusion) ||
      exclusion < 0) {
    stop("`exclusion` must be a single non-negative number.", call. = FALSE)
  }
  if (!isTRUE(relative) && !isFALSE(relative)) {
    stop("`relative` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!isTRUE(expected) && !isFALSE(expected)) {
    stop("`expected` must be TRUE or FALSE.", call. = FALSE)
  }
  if (expected && relative) {
    stop("`expected = TRUE` needs absolute changes (relative = FALSE).",
         call. = FALSE)
  }
  invisible(NULL)
}

# Concordance rate among the changes outside the exclusion zone (|mean
# change| >= exclusion): both methods change in the same direction.
trend_concordance = function(ch,
                             exclusion) {
  keep = abs(ch$mean) >= exclusion & !(ch$dx == 0 & ch$dy == 0)
  list(keep = keep,
       n = sum(keep),
       n_excl = sum(!keep),
       rate = mean(ch$dx[keep] * ch$dy[keep] > 0))
}

# Concordance expected from the model at the observed mean changes: a true
# change D gives the same direction in both methods with probability about
# pnorm(|D| / (sqrt(2) sigma_w)), where sigma_w is the within-item SD of the
# differences (Enevoldsen et al., 2026); averaged over the included changes.
trend_expected = function(ch,
                          keep,
                          sd_w) {
  mean(stats::pnorm(abs(ch$mean[keep]) / (sqrt(2) * sd_w)))
}

# Pooled within-item SD of the differences, x - y (one-way ANOVA), for the
# expected concordance when the model does not give it directly.
trend_sd_within = function(df) {
  d = df$x - df$y
  n_i = table(df$id)
  dev = d - stats::ave(d, df$id)
  sqrt(sum(dev^2) / (length(d) - length(n_i)))
}

trend_axis_label = function(name,
                            relative,
                            log) {
  if (relative) {
    paste0("Change in ", name, " (%)")
  } else if (log) {
    paste0("Change in log ", name)
  } else {
    paste0("Change in ", name)
  }
}

trend_pct = function(v) {
  paste0(format(round(100 * v, 1), nsmall = 1), "%")
}

# 4-quadrant plot: change by x against change by y, with the line of
# identity and the exclusion zone (changes whose mean is within +/-
# exclusion, the band between the lines x + y = +/- 2 exclusion).
trend_quadrant_plot = function(ch,
                               x_name,
                               y_name,
                               exclusion = 0,
                               relative = FALSE,
                               log = FALSE,
                               sd_w = NULL) {
  cc = trend_concordance(ch, exclusion)
  ch$included = factor(ifelse(cc$keep, "Included", "Excluded"),
                       levels = c("Included", "Excluded"))
  lim = max(abs(c(ch$dx, ch$dy)), na.rm = TRUE)
  cap = paste0("Concordance rate: ", trend_pct(cc$rate), " (", cc$n,
               " changes",
               if (exclusion > 0) {
                 paste0("; ", cc$n_excl, " excluded with |mean change| < ",
                        format(exclusion), if (relative) "%")
               },
               ")")
  if (!is.null(sd_w)) {
    cap = paste0(cap, "\nExpected from the model at these changes: ",
                 trend_pct(trend_expected(ch, cc$keep, sd_w)))
  }
  p = ggplot(ch, aes(x = dx, y = dy)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_vline(xintercept = 0, colour = "grey50") +
    geom_abline(intercept = 0, slope = 1, linetype = 2)
  if (exclusion > 0) {
    p = p +
      geom_abline(intercept = c(-2, 2) * exclusion, slope = -1,
                  linetype = 3) +
      geom_point(aes(shape = included), na.rm = TRUE) +
      scale_shape_manual(values = c(Included = 16, Excluded = 1),
                         drop = FALSE, name = NULL)
  } else {
    p = p + geom_point(na.rm = TRUE)
  }
  p +
    coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    labs(x = trend_axis_label(x_name, relative, log),
         y = trend_axis_label(y_name, relative, log),
         caption = cap) +
    theme_bw() +
    theme(legend.position = "left")
}

# Polar plot (Critchley et al., 2011): each change is drawn at a distance
# equal to its mean change and at the angle between (dx, dy) and the line of
# identity (positive when y changed more than x). Changes with a negative
# mean are turned through 180 degrees, so all lie in the right half. The
# angular bias is the mean angle of the included changes, with limits at
# bias +/- z SD (z from agree_level).
trend_polar_angles = function(ch) {
  theta = atan2(ch$dy, ch$dx) * 180 / pi - 45
  theta = ifelse(ch$mean < 0, theta + 180, theta)
  # wrap to (-180, 180]
  theta = ((theta + 180) %% 360) - 180
  data.frame(r = abs(ch$mean), theta = theta)
}

trend_polar_plot = function(ch,
                            x_name,
                            y_name,
                            exclusion = 0,
                            relative = FALSE,
                            log = FALSE,
                            agree_level = 0.95,
                            sd_w = NULL) {
  cc = trend_concordance(ch, exclusion)
  pa = trend_polar_angles(ch)
  pa$included = factor(ifelse(cc$keep, "Included", "Excluded"),
                       levels = c("Included", "Excluded"))
  th = pa$theta[cc$keep]
  z = stats::qnorm(1 - (1 - agree_level) / 2)
  bias = mean(th)
  s = stats::sd(th)
  ang = c(bias, bias - z * s, bias + z * s)
  r_max = max(pa$r, na.rm = TRUE)
  rad = function(a) a * pi / 180
  deg = intToUtf8(176)  # degree sign
  pa$px = pa$r * cos(rad(pa$theta))
  pa$py = pa$r * sin(rad(pa$theta))

  # grid: half circles at pretty radii and spokes every 30 degrees
  rings = pretty(c(0, r_max), n = 4)
  rings = rings[rings > 0]
  r_out = max(rings)
  arc = function(r, lab) {
    a = seq(-90, 90, length.out = 181)
    data.frame(x = r * cos(rad(a)), y = r * sin(rad(a)), ring = lab)
  }
  grid_arcs = do.call(rbind, lapply(seq_along(rings),
                                    function(j) arc(rings[j], j)))
  spokes = seq(-90, 90, by = 30)
  df_spk = data.frame(x = 0, y = 0, xend = r_out * cos(rad(spokes)),
                      yend = r_out * sin(rad(spokes)))
  df_spk_lab = data.frame(x = 1.08 * r_out * cos(rad(spokes)),
                          y = 1.08 * r_out * sin(rad(spokes)),
                          lab = paste0(spokes, deg))
  df_ang = data.frame(xend = r_out * cos(rad(ang)),
                      yend = r_out * sin(rad(ang)),
                      what = factor(c("Angular bias", "Radial limits",
                                      "Radial limits"),
                                    levels = c("Angular bias",
                                               "Radial limits")))

  cap = paste0("Angular bias: ", format(round(bias, 1), nsmall = 1),
               deg, "; radial limits of agreement (",
               round(100 * agree_level), "%): ",
               format(round(ang[2], 1), nsmall = 1), deg, " to ",
               format(round(ang[3], 1), nsmall = 1), deg,
               "\n(angle > 0: ", y_name, " changed more than ", x_name,
               "; ", cc$n, " changes",
               if (exclusion > 0) {
                 paste0("; ", cc$n_excl, " excluded with |mean change| < ",
                        format(exclusion), if (relative) "%")
               },
               ")\nConcordance rate: ", trend_pct(cc$rate))
  if (!is.null(sd_w)) {
    cap = paste0(cap, "; expected from the model at these changes: ",
                 trend_pct(trend_expected(ch, cc$keep, sd_w)))
  }
  r_lab = if (relative) "Mean change (%)" else if (log) {
    "Mean change (log scale)"
  } else "Mean change"

  p = ggplot() +
    geom_path(data = grid_arcs, aes(x = x, y = y, group = ring),
              colour = "grey85") +
    geom_segment(data = df_spk, aes(x = x, y = y, xend = xend, yend = yend),
                 colour = "grey85") +
    geom_text(data = df_spk_lab, aes(x = x, y = y, label = lab),
              size = 3, colour = "grey40")
  if (exclusion > 0) {
    p = p + geom_path(data = arc(exclusion, 0), aes(x = x, y = y),
                      linetype = 3)
  }
  p = p +
    geom_segment(data = df_ang, aes(x = 0, y = 0, xend = xend, yend = yend,
                                    linetype = what)) +
    scale_linetype_manual(values = c("Angular bias" = 1,
                                     "Radial limits" = 2), name = NULL)
  p = if (exclusion > 0) {
    p + geom_point(data = pa, aes(x = px, y = py, shape = included),
                   na.rm = TRUE) +
      scale_shape_manual(values = c(Included = 16, Excluded = 1),
                         drop = FALSE, name = NULL)
  } else {
    p + geom_point(data = pa, aes(x = px, y = py), na.rm = TRUE)
  }
  p +
    coord_fixed(xlim = c(0, 1.15 * r_out),
                ylim = c(-1.12 * r_out, 1.12 * r_out)) +
    labs(x = r_lab, y = NULL, caption = cap) +
    theme_bw() +
    theme(legend.position = "left",
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

# dispatch for the three classes; `df` as in trend_changes()
trend_plot = function(df,
                      type,
                      x_name,
                      y_name,
                      exclusion,
                      relative,
                      expected,
                      log = FALSE,
                      sd_w = NULL,
                      agree_level = 0.95) {
  trend_check_args(exclusion, relative, expected)
  ch = trend_changes(df, relative = relative, log = log)
  if (!expected) {
    sd_w = NULL
  } else if (is.null(sd_w)) {
    sd_w = trend_sd_within(df)
  }
  switch(type,
         quadrant = trend_quadrant_plot(ch, x_name = x_name,
                                        y_name = y_name,
                                        exclusion = exclusion,
                                        relative = relative, log = log,
                                        sd_w = sd_w),
         polar = trend_polar_plot(ch, x_name = x_name, y_name = y_name,
                                  exclusion = exclusion,
                                  relative = relative, log = log,
                                  agree_level = agree_level, sd_w = sd_w))
}
