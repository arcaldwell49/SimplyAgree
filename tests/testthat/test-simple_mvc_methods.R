mvc_fit = function(...) {
  dat = sim_methods(n_items = 30, n_reps = 3, methods = c("A", "B", "C"),
                    alpha = c(0, 0.5, -0.2), tau = 0.3, omega = 0.4,
                    sigma = c(0.3, 0.5, 0.4), replicates = "linked",
                    seed = 1)
  method_vc(dat, y = "y", method = "method", id = "id", replicate = "repl",
            replicates = "linked", ...)
}

test_that("print shows limits and variance components", {
  fit = mvc_fit()
  out = capture.output(print(fit))
  expect_true(any(grepl("Variance Component Limits of Agreement", out)))
  expect_true(any(grepl("Linked replicates", out)))
  expect_true(any(grepl("A - B", out)))
  expect_true(any(grepl("omega", out)))
  expect_true(any(grepl("intersection-union", out)))

  out_tol = capture.output(print(mvc_fit(interval = "tolerance")))
  expect_true(any(grepl("Prediction and Tolerance Limits", out_tol)))
  expect_true(any(grepl("Prediction Interval", out_tol)))
  expect_true(any(grepl("expected to contain a single new difference",
                        out_tol)))
})

test_that("print shows ratios for log-transformed data", {
  dat = sim_methods(n_items = 30, methods = c("A", "B"), mu_mean = 3,
                    alpha = c(0, 0.1), sigma = 0.1, transform = "log",
                    seed = 1)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  transform = "log")
  out = capture.output(print(fit))
  expect_true(any(grepl("A / B", out)))
  expect_true(any(grepl("ratios", out)))
  expect_true(any(grepl(format(round(exp(fit$bias$bias), 4)), out,
                        fixed = TRUE)))
})

sat_fit = function(...) {
  dat = sim_methods(n_items = 40, n_reps = 2, methods = c("Ref", "New"),
                    alpha = c(0, -0.15), mu_mean = 1.5, mu_sd = 0.8,
                    tau = 0.15, sigma = c(0.12, 0.18),
                    transform = "pctlogit", seed = 7)
  method_vc(dat, y = "y", method = "method", id = "id", replicate = "repl",
            transform = "pctlogit", ...)
}

test_that("original scale: limits are h(limit) at the level", {
  fit = sat_fit()
  inv = function(z) 100 * plogis(z)
  h = function(d, t) inv(t + d / 2) - inv(t - d / 2)
  t90 = qlogis(0.9)

  os = mc_original_scale(fit, at = 90)
  expect_equal(os$at, 90)
  expect_equal(os$bias, h(fit$bias$bias, t90))
  expect_equal(os$lower.CL, h(fit$bias$lower.CL, t90))
  expect_equal(c(os$lower_loa, os$upper_loa),
               h(c(fit$limits$lower_loa, fit$limits$upper_loa), t90))
  expect_equal(c(os$lower_loa_ci, os$upper_loa_ci),
               h(c(fit$limits$lower_loa_ci, fit$limits$upper_loa_ci), t90))
  rp = attr(os, "repeatability")
  expect_equal(rp$rc, h(fit$repeatability$rc, t90))
  expect_equal(rp$rc_upper, h(fit$repeatability$rc_upper, t90))

  # limits keep their order and narrow toward the boundary
  expect_true(all(os$lower_loa_ci < os$lower_loa & os$lower_loa < os$bias &
                    os$bias < os$upper_loa & os$upper_loa < os$upper_loa_ci))
  os2 = mc_original_scale(fit, at = c(70, 98))
  expect_equal(nrow(os2), 2)
  w = os2$upper_loa - os2$lower_loa
  expect_true(w[2] < w[1])

  # default level: median of the item averages on the analysis scale
  t_med = median(tapply(fit$data$y_t, fit$data$id, mean))
  os_med = mc_original_scale(fit)
  expect_equal(os_med$at, inv(t_med))
  expect_equal(os_med$upper_loa, h(fit$limits$upper_loa, t_med))
  expect_true(attr(os_med, "at_median"))

  # tolerance limits are converted the same way
  fit_tl = sat_fit(interval = "tolerance")
  os_tl = mc_original_scale(fit_tl, at = 90)
  expect_equal(c(os_tl$lower.TL, os_tl$upper.TL),
               h(c(fit_tl$limits$lower.TL, fit_tl$limits$upper.TL), t90))
  expect_equal(c(os_tl$lower.PL, os_tl$upper.PL),
               h(c(fit_tl$limits$lower.PL, fit_tl$limits$upper.PL), t90))
})

test_that("original scale agrees with predict() for a centered reading", {
  # predict() starts from a measurement by one method: from New = x, Ref is
  # inv(g(x) + d). Centered on t, the Ref - New difference at d is
  # inv(t + d/2) - inv(t - d/2), the same as predict() from
  # New = inv(t - bias/2) at the bias itself.
  fit = sat_fit()
  t90 = qlogis(0.9)
  x_new = 100 * plogis(t90 - fit$bias$bias / 2)
  pr = predict(fit, newdata = x_new, from = "New", to = "Ref")
  os = mc_original_scale(fit, at = 90)
  expect_equal(pr$fit - x_new, os$bias)
})

test_that("print with scale = 'original'", {
  fit = sat_fit()
  out = capture.output(print(fit, scale = "original"))
  expect_true(any(grepl("Original scale (back-transformed from pctlogit)",
                        out, fixed = TRUE)))
  expect_true(any(grepl("median of the item averages", out)))
  expect_true(any(grepl("At", out)))
  expect_true(any(grepl("Repeatability Coefficients (95%, original scale)",
                        out, fixed = TRUE)))
  # variance components stay on the analysis scale
  expect_true(any(grepl("Variance Components (SD, pctlogit scale)", out,
                        fixed = TRUE)))
  os = mc_original_scale(fit, at = c(70, 90))
  out_at = capture.output(print(fit, at = c(70, 90)))
  expect_true(any(grepl("the levels in 'At'", out_at)))
  expect_true(any(grepl(format(round(os$upper_loa[1], 4)), out_at,
                        fixed = TRUE)))
  # default print points to the option
  expect_true(any(grepl("scale = \"original\"", capture.output(print(fit)),
                        fixed = TRUE)))
  # log fits keep ratios unless asked
  dat = sim_methods(n_items = 30, methods = c("A", "B"), mu_mean = 3,
                    alpha = c(0, 0.1), sigma = 0.1, transform = "log",
                    seed = 1)
  fit_log = method_vc(dat, y = "y", method = "method", id = "id",
                      transform = "log")
  out_log = capture.output(print(fit_log, at = 20))
  expect_true(any(grepl("A - B", out_log)))
  # no transformation: the argument is ignored
  expect_equal(capture.output(print(mvc_fit(), scale = "original")),
               capture.output(print(mvc_fit())))
  expect_error(print(fit, at = 120), "domain")
  expect_error(print(fit, at = "a"), "numeric")
})

test_that("back-transformed differences are NA outside a monotone range", {
  tf = mc_transform("sqrt")
  expect_true(is.na(mc_back_diff(tf, t = 0.5, d = 2)))
  expect_equal(mc_back_diff(tf, t = 3, d = 1), 3.5^2 - 2.5^2)
})

test_that("coef and vcov", {
  fit = mvc_fit()
  cf = coef(fit)
  expect_named(cf, c("alpha_A", "alpha_B", "alpha_C", "tau", "omega",
                     "sigma_A", "sigma_B", "sigma_C"))
  # differences of the method means are the biases
  expect_equal(unname(cf["alpha_A"] - cf["alpha_B"]), fit$bias$bias[1])
  V = vcov(fit)
  expect_equal(dim(V), c(3, 3))
  expect_equal(rownames(V), c("alpha_A", "alpha_B", "alpha_C"))
  se_ab = sqrt(V[1, 1] + V[2, 2] - 2 * V[1, 2])
  expect_equal(se_ab, fit$bias$se[1])
})

test_that("plot returns a Bland-Altman ggplot for any pair", {
  fit = mvc_fit()
  p = plot(fit)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "Difference between methods (A - B)")

  # bias and limits as points with confidence intervals, left of the data
  pr = geom_data(p, "GeomPointrange")
  l = fit$limits[1, ]
  expect_equal(as.character(pr$text), c("Upper LoA", "Bias", "Lower LoA"))
  expect_equal(pr$estimate, c(l$upper_loa, fit$bias$bias[1], l$lower_loa))
  expect_equal(pr$lower.ci, c(l$upper_loa, fit$bias$lower.CL[1],
                              l$lower_loa_ci))
  expect_equal(pr$upper.ci, c(l$upper_loa_ci, fit$bias$upper.CL[1],
                              l$lower_loa))
  expect_equal(unique(pr$x), min(p$data$avg))

  # reversed pair flips the limits
  p2 = plot(fit, pair = c("C", "A"))
  pr2 = geom_data(p2, "GeomPointrange")
  j = fit$bias$method1 == "A" & fit$bias$method2 == "C"
  expect_equal(pr2$estimate, c(-fit$limits$lower_loa[j], -fit$limits$bias[j],
                               -fit$limits$upper_loa[j]))
  expect_equal(pr2$upper.ci[1], -fit$limits$lower_loa_ci[j])
  expect_equal(pr2$lower.ci[3], -fit$limits$upper_loa_ci[j])
  expect_error(plot(fit, pair = c("A", "Z")), "two different methods")

  # tolerance: points at the prediction limits, whiskers out to the
  # tolerance limits; a reversed pair flips both
  fit_tl = mvc_fit(interval = "tolerance")
  p_tol = plot(fit_tl, pair = c("C", "A"))
  expect_s3_class(p_tol, "ggplot")
  l = fit_tl$limits[j, ]
  pt = geom_data(p_tol, "GeomPointrange")
  expect_equal(as.character(pt$text),
               c("Upper Limit", "Bias", "Lower Limit"))
  expect_equal(pt$estimate, c(-l$lower.PL, -l$bias, -l$upper.PL))
  expect_equal(pt$upper.ci[1], -l$lower.TL)
  expect_equal(pt$lower.ci[3], -l$upper.TL)

  # exchangeable pairing is reproducible with pair_seed
  dat = sim_methods(n_items = 20, n_reps = 2, tau = 0.3, seed = 3)
  fe = method_vc(dat, y = "y", method = "method", id = "id",
                 replicate = "repl")
  expect_equal(plot(fe, pair_seed = 1)$data, plot(fe, pair_seed = 1)$data)
})

test_that("plot uses the transformed or ratio scale", {
  dat = sim_methods(n_items = 30, methods = c("A", "B"), sigma = 0.2,
                    transform = "pctlogit", seed = 2)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  transform = "pctlogit")
  p = plot(fit)
  expect_match(p$labels$y, "logit")

  dat_l = sim_methods(n_items = 30, methods = c("A", "B"), mu_mean = 3,
                      sigma = 0.1, transform = "log", seed = 2)
  fit_l = method_vc(dat_l, y = "y", method = "method", id = "id",
                    transform = "log")
  p_l = plot(fit_l)
  expect_match(p_l$labels$y, "Ratio")
  expect_true(all(p_l$data$d > 0))
  expect_equal(geom_data(p_l, "GeomPointrange")$estimate[2],
               exp(fit_l$bias$bias))

  # delta for ratios: delta and 1/delta
  expect_equal(geom_data(plot(fit_l, delta = 1.25), "GeomHline")$y1,
               c(1.25, 0.8))
  expect_error(plot(fit_l, delta = -1), "greater than 0")
})

test_that("Bland-Altman plot takes geom and delta", {
  fit = mvc_fit()
  p = plot(fit, delta = 1.5)
  expect_equal(geom_data(p, "GeomHline")$y1, c(1.5, -1.5))
  expect_equal(p$scales$get_scales("y")$secondary.axis$name,
               "Maximal Allowable Difference")
  expect_equal(geom_data(plot(fit, delta = c(-1, 2)), "GeomHline")$y1,
               c(-1, 2))
  expect_error(plot(fit, delta = "a"), "one or two numbers")

  p_bin = plot(fit, geom = "geom_bin2d")
  expect_false(inherits(p_bin$layers[[1]]$geom, "GeomPoint"))
  expect_s3_class(geom_data(p_bin, "GeomPointrange"), "data.frame")
  for (g in c("geom_density_2d", "geom_density_2d_filled",
              "stat_density_2d")) {
    expect_s3_class(plot(fit, geom = g), "ggplot")
  }
  expect_error(plot(fit, geom = "geom_line"), "should be one of")
})

test_that("objects survive saveRDS/readRDS", {
  fit = mvc_fit()
  dat_l = sim_methods(n_items = 20, methods = c("A", "B"), mu_mean = 3,
                      sigma = 0.1, transform = "log", seed = 2)
  fit_l = method_vc(dat_l, y = "y", method = "method", id = "id",
                    transform = list(fun = log, inv = exp, name = "mylog"))
  f = tempfile(fileext = ".rds")
  g = tempfile(fileext = ".rds")
  saveRDS(fit, f)
  saveRDS(fit_l, g)
  rm(fit, fit_l)
  fit2 = readRDS(f)
  fit_l2 = readRDS(g)
  expect_output(print(fit2), "Limits of Agreement")
  expect_s3_class(plot(fit2), "ggplot")
  expect_output(print(fit_l2), "mylog")
  expect_s3_class(plot(fit_l2), "ggplot")
  expect_equal(fit_l2$transform$inv(fit_l2$transform$fun(5)), 5)
  unlink(c(f, g))
})

test_that("print shows repeatability coefficients", {
  out = capture.output(print(mvc_fit()))
  expect_true(any(grepl("Repeatability Coefficients", out)))
  expect_true(any(grepl("RpC", out)))
})

test_that("summary prints all parts", {
  fit = mvc_fit()
  s = summary(fit)
  expect_s3_class(s, "summary.simple_mvc")
  out = capture.output(print(s))
  for (txt in c("Design", "Bias", "Variance components",
                "Limits of agreement", "Repeatability")) {
    expect_true(any(grepl(txt, out)), info = txt)
  }
})

test_that("confint matches stored intervals and recomputes other levels", {
  fit = mvc_fit()
  ci = confint(fit)
  expect_equal(nrow(ci), nrow(fit$bias) + nrow(fit$varcomp))
  expect_equal(unname(ci[1:3, 1]), fit$bias$lower.CL)
  expect_equal(unname(ci[-(1:3), 2]), fit$varcomp$upper, tolerance = 1e-8)
  expect_equal(colnames(ci), c("2.5 %", "97.5 %"))
  ci90 = confint(fit, level = 0.9)
  expect_true(all(ci90[, 1] > ci[, 1] & ci90[, 2] < ci[, 2]))
  expect_equal(rownames(confint(fit, parm = "bias")),
               c("bias_A-B", "bias_A-C", "bias_B-C"))
  expect_equal(nrow(confint(fit, parm = "sd")), nrow(fit$varcomp))

  # exact interval for the paired-difference model
  dat = sim_methods(n_items = 30, methods = c("A", "B"), seed = 1)
  fit2 = method_vc(dat, y = "y", method = "method", id = "id")
  ci2 = confint(fit2, parm = "sd")
  expect_equal(unname(ci2[1, ]), c(fit2$varcomp$lower, fit2$varcomp$upper))
})

test_that("matrix and vc plots", {
  fit = mvc_fit()
  pm = plot(fit, type = "matrix")
  expect_s3_class(pm, "patchwork")
  expect_equal(length(pm$patches$plots) + 1, 9)
  pv = plot(fit, type = "vc")
  expect_s3_class(pv, "ggplot")
  expect_setequal(levels(pv$data$facet),
                  c("tau (method x item)", "omega (item x replicate)",
                    "sigma (residual)", "RC (repeatability)",
                    "RpC (reproducibility)"))

  # back-transformed conversion lines stay in the domain
  dat = sim_methods(n_items = 30, methods = c("A", "B"), sigma = 0.2,
                    transform = "pctlogit", seed = 2)
  fp = method_vc(dat, y = "y", method = "method", id = "id",
                 transform = "pctlogit")
  pm2 = plot(fp, type = "matrix")
  conv = pm2$patches$plots[[1]]$layers[[1]]$data
  expect_true(all(conv$lwr > 0 & conv$upr < 100))

  dat6 = sim_methods(n_items = 10, methods = LETTERS[1:6], seed = 1)
  f6 = suppressMessages(method_vc(dat6, y = "y", method = "method",
                                  id = "id"))
  expect_message(plot(f6, type = "matrix"), "crowded")
})

test_that("check returns diagnostic plots and flags level dependence", {
  fit = mvc_fit()
  expect_s3_class(check(fit, pair_seed = 1), "patchwork")

  dat = sim_methods(n_items = 80, methods = c("A", "B"), beta = c(1, 1.5),
                    mu_mean = 10, mu_sd = 3, sigma = 0.3, seed = 3)
  fit2 = method_vc(dat, y = "y", method = "method", id = "id")
  expect_message(p <- check(fit2), "method_convert")
  expect_s3_class(p, "patchwork")

  # spread of the differences that grows with the level
  set.seed(4)
  mu = rnorm(60, 10, 3)
  dat_h = data.frame(id = rep(1:60, 2), method = rep(c("A", "B"), each = 60),
                     y = c(mu + rnorm(60, 0, 0.05 * mu),
                           mu + rnorm(60, 0, 0.05 * mu)))
  fit_h = method_vc(dat_h, y = "y", method = "method", id = "id")
  expect_message(check(fit_h), "transformation")

  # works without the model, and after saveRDS/readRDS
  fit3 = mvc_fit(keep_model = FALSE)
  f = tempfile(fileext = ".rds")
  saveRDS(fit3, f)
  expect_s3_class(check(readRDS(f), pair_seed = 1), "patchwork")
  unlink(f)
})

test_that("check panels have subtitles and test captions", {
  fit = mvc_fit()
  pl = patch_plots(check(fit, pair_seed = 1))
  expect_length(pl, 4)
  for (q in pl) {
    expect_false(is.null(q$labels$subtitle))
  }
  expect_match(pl[[1]]$labels$caption, "Normality \nShapiro-Wilk Test: A: p")
  expect_match(pl[[2]]$labels$caption, "Shapiro-Wilk Test: Item-by-replicate")
  expect_match(pl[[3]]$labels$caption, "Breusch-Pagan Test: A - B: p")
  expect_match(pl[[4]]$labels$caption, "Pitman-Morgan")
  expect_match(pl[[4]]$labels$subtitle, "Error-adjusted")
  # tests for pairs use item means
  expect_equal(nrow(pl[[3]]$data), 3 * 30)
  expect_equal(sort(unique(as.character(pl[[3]]$data$group))),
               c("A - B", "A - C", "B - C"))

  # one measurement by two methods: one residual per item
  dat = sim_methods(n_items = 30, methods = c("A", "B"), seed = 1)
  f2 = method_vc(dat, y = "y", method = "method", id = "id")
  pl2 = patch_plots(check(f2))
  expect_equal(nrow(pl2[[1]]$data), 30)
  expect_equal(unique(pl2[[1]]$data$group), "A - B")
  expect_match(pl2[[2]]$labels$title, "Random Effects")
  expect_match(pl2[[4]]$labels$subtitle, "should be horizontal")
})
