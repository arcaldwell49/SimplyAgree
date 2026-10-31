test_that("exact TDI is the p quantile of |D| and CP is its inverse", {
  for (mu in c(0, 0.7, 3)) {
    for (p in c(0.8, 0.9, 0.95)) {
      k <- tdi_exact(mu, 1.3, p)
      expect_equal(cp_exact(k, mu, 1.3), p, tolerance = 1e-8)
      root <- uniroot(function(x) cp_exact(x, mu, 1.3) - p, c(0, 50),
                      tol = 1e-12)$root
      expect_equal(k, root, tolerance = 1e-8)
    }
  }
  expect_equal(tdi_lin(0.5, 2, 0.9), qnorm(0.95) * sqrt(0.25 + 4))
})

test_that("tdi() on agreement_limit() returns an htest with defaults from the object", {
  data("reps")
  fit <- agreement_limit(x = "x", y = "y", data = reps)
  res <- tdi(fit, seed = 1, B = 2000)
  expect_s3_class(res, c("tdi_htest", "htest"))
  expect_named(res$estimate, "TDI")
  expect_equal(res$settings$p, 0.95)
  expect_equal(attr(res$conf.int, "conf.level"), 0.95)
  expect_equal(res$conf.int[1], 0)
  expect_null(res$p.value)
  expect_null(res$cp)
  d <- na.omit(reps$x - reps$y)
  expect_equal(unname(res$estimate), tdi_exact(mean(d), sd(d), 0.95),
               tolerance = 1e-10)
  expect_gt(res$conf.int[2], res$estimate)
  expect_equal(fit$loa$df_sd, length(d) - 1)
})

test_that("point estimates match MethComp::TDI()", {
  skip_if_not_installed("MethComp")
  data("reps")
  fit <- agreement_limit(x = "x", y = "y", data = reps)
  res <- tdi(fit, p = 0.9, method = "delta")
  cc <- stats::complete.cases(reps[, c("x", "y")])
  mc <- MethComp::TDI(reps$x[cc], reps$y[cc], p = 0.1, boot = FALSE)[[1]]
  expect_equal(unname(res$estimate), unname(mc[2]), tolerance = 1e-4)
  expect_equal(unname(res$lin_approx), unname(mc[1]), tolerance = 1e-10)
})

test_that("delta-method bound follows its formula for simple data", {
  data("reps")
  fit <- agreement_limit(x = "x", y = "y", data = reps)
  res <- tdi(fit, p = 0.9, delta = 2, method = "delta")
  d <- na.omit(reps$x - reps$y)
  n <- length(d); mu <- mean(d); s <- sd(d)
  f <- function(m, v) log(tdi_exact(m, sqrt(v), 0.9))
  g <- c(
    (f(mu + 1e-6, s^2) - f(mu - 1e-6, s^2)) / 2e-6,
    (f(mu, s^2 + 1e-6) - f(mu, s^2 - 1e-6)) / 2e-6)
  se_log <- sqrt(g[1]^2 * s^2 / n + g[2]^2 * 2 * s^4 / (n - 1))
  expect_equal(res$conf.int[2],
               exp(f(mu, s^2) + qt(0.95, n - 1) * se_log), tolerance = 1e-5)
  expect_equal(unname(res$statistic), (log(2) - f(mu, s^2)) / se_log,
               tolerance = 1e-5)
  expect_equal(res$p.value, pt(unname(res$statistic), n - 1, lower.tail = FALSE),
               tolerance = 1e-12)
})

test_that("tolerance bound is the Escaramis et al. noncentral t bound", {
  data("reps")
  tl <- tolerance_limit(x = "x", y = "y", data = reps, pred_level = 0.9)
  res <- tdi(tl, method = "tolerance")
  expect_equal(res$settings$p, 0.9)
  d <- na.omit(reps$x - reps$y)
  n <- length(d); mu <- abs(mean(d)); s <- sd(d)
  # q such that mu + z_q s is the TDI
  q <- uniroot(function(m) m - pnorm(-qnorm(m) - 2 * mu / s) - 0.9,
               c(0.9, 1 - 1e-12), tol = 1e-14)$root
  expect_equal(mu + qnorm(q) * s, unname(res$estimate), tolerance = 1e-8)
  expect_equal(res$conf.int[2],
               mu + qt(0.95, n - 1, ncp = qnorm(q) * sqrt(n)) * s / sqrt(n),
               tolerance = 1e-8)
  # agreement_limit() with the same inputs gives the same bound
  al <- agreement_limit(x = "x", y = "y", data = reps, agree.level = 0.9)
  expect_equal(tdi(al, method = "tolerance")$conf.int[2], res$conf.int[2],
               tolerance = 1e-8)
})

test_that("CP bound and test are consistent with the TDI bound", {
  data("reps")
  fit <- agreement_limit(x = "x", y = "y", data = reps)
  for (m in c("gpq", "delta", "tolerance")) {
    res <- tdi(fit, p = 0.9, delta = 3, method = m, seed = 2, B = 4000)
    expect_true(res$cp[["lower"]] <= res$cp[["CP"]])
    expect_named(res$null.value, "TDI")
    expect_equal(res$alternative, "less")
    # the bound is below delta exactly when the test rejects at 1 - conf
    expect_equal(res$conf.int[2] < 3, res$p.value < 0.05)
    # and then the CP bound is at least p
    expect_equal(res$cp[["lower"]] >= 0.9, res$p.value < 0.05)
  }
})

test_that("gpq is reproducible with a seed and leaves the RNG alone", {
  data("reps")
  fit <- agreement_limit(x = "x", y = "y", data = reps)
  set.seed(99)
  before <- .Random.seed
  r1 <- tdi(fit, seed = 5, B = 1000)
  expect_identical(.Random.seed, before)
  r2 <- tdi(fit, seed = 5, B = 1000)
  expect_identical(r1$conf.int, r2$conf.int)
})

test_that("rows: prop_bias, conditions, and method pairs", {
  data("reps")
  ap <- agreement_limit(x = "x", y = "y", data = reps, prop_bias = TRUE)
  res <- tdi(ap, method = "delta")
  expect_equal(nrow(res$table), 3)
  expect_equal(res$conf.int[2], max(res$table$tdi_upper))
  expect_output(print(res), "By row")

  d3 <- sim_methods(n_items = 20, n_reps = 2, methods = c("R", "A", "B"),
                    alpha = c(0, 0.5, -0.3), tau = 0.4,
                    sigma = c(0.4, 0.7, 0.6), seed = 3)
  f <- method_vc(d3, y = "y", method = "method", id = "id",
                 replicate = "repl")
  rm <- tdi(f, method = "delta", delta = 3)
  expect_equal(nrow(rm$table), 3)
  expect_equal(rm$table$panel, paste(f$limits$method1, "-", f$limits$method2))
  expect_equal(rm$table$bias, f$limits$bias)
  expect_equal(rm$table$sd, f$limits$sd)
  expect_equal(rm$p.value, max(rm$table$p.value))
})

test_that("replicated and nested data use the Satterthwaite df", {
  data("reps")
  ar <- agreement_limit(x = "x", y = "y", id = "id", data = reps,
                        data_type = "reps")
  an <- agreement_limit(x = "x", y = "y", id = "id", data = reps,
                        data_type = "nest")
  expect_true(all(is.finite(c(ar$loa$df_sd, an$loa$df_sd))))
  expect_equal(tdi(ar, method = "delta")$table$df_sd, ar$loa$df_sd)
  # nested data, tolerance_limit() with id, and method_vc() with linked
  # replicates estimate the same SD
  tl <- tolerance_limit(x = "x", y = "y", id = "id", data = reps)
  expect_equal(tdi(an, method = "delta")$estimate,
               tdi(tl, method = "delta")$estimate, tolerance = 1e-5)
})

test_that("log scale reports ratios", {
  data("reps")
  al <- agreement_limit(x = "x", y = "y", data = reps, log_tf = TRUE)
  res <- tdi(al, delta = 1.5, method = "delta")
  expect_named(res$estimate, "TDI (ratio)")
  expect_equal(res$conf.int[1], 1)
  expect_equal(unname(res$estimate), exp(res$table$tdi))
  expect_error(tdi(al, delta = 0.9), "ratio greater than 1")
})

test_that("summary and plot work", {
  data("reps")
  res <- tdi(agreement_limit(x = "x", y = "y", data = reps), delta = 2,
             method = "delta")
  s <- summary(res)
  expect_s3_class(s, "summary.tdi_htest")
  expect_match(s$text, "TDI")
  expect_output(print(s), "absolute differences")
  expect_s3_class(plot(res), "ggplot")
  ap <- agreement_limit(x = "x", y = "y", data = reps, prop_bias = TRUE)
  expect_s3_class(plot(tdi(ap, method = "delta")), "ggplot")
})

test_that("input validation", {
  data("reps")
  fit <- agreement_limit(x = "x", y = "y", data = reps)
  expect_error(tdi(fit, p = 1.2), "between 0 and 1")
  expect_error(tdi(fit, conf.level = 0.3), "between 0.5 and 1")
  expect_error(tdi(fit, delta = -1), "positive")
  expect_error(tdi(fit, B = 10), "at least 100")
  expect_error(tdi(lm(y ~ x, data = reps)), "has methods for")
})

test_that("rows without df give the estimate but no bound, with a warning", {
  data("reps")
  fit <- agreement_limit(x = "x", y = "y", data = reps)
  fit$loa$df_sd <- NA_real_
  expect_warning(res <- tdi(fit, delta = 2, method = "gpq", B = 500),
                 "unavailable")
  expect_true(is.finite(res$estimate))
  expect_true(is.na(res$conf.int[2]))
  expect_true(is.na(res$p.value))
  expect_output(print(res), "TDI")
})

test_that("variance components add up to the variance of a single difference", {
  data("reps")
  for (dt in c("simple", "reps", "nest")) {
    fit <- agreement_limit(x = "x", y = "y", id = "id", data = reps,
                           data_type = dt)
    vc <- fit$var_comp
    expect_equal(sum(vc$v), fit$loa$sd_delta[1]^2, tolerance = 1e-10)
    expect_equal(fit$loa$df_sd[1], sum(vc$v)^2 / sum(vc$v^2 / vc$df),
                 tolerance = 1e-10)
  }
  tl <- tolerance_limit(x = "x", y = "y", id = "id", data = reps)
  res <- tdi(tl, seed = 1, B = 2000)
  expect_true(is.finite(res$conf.int[2]))
  expect_gt(res$conf.int[2], res$estimate)
})

test_that("estimates and the tolerance bound match TDIagree", {
  skip_if_not_installed("TDIagree")
  data("reps")
  r <- reps[stats::complete.cases(reps[, c("x", "y")]), ]
  n <- nrow(r)
  long <- data.frame(id = factor(rep(seq_len(n), 2)),
                     met = factor(rep(c("x", "y"), each = n)),
                     y = c(r$x, r$y))
  ta <- suppressWarnings(
    TDIagree::TDI(long, y = "y", id = "id", met = "met", p = 0.9,
                  method = c("Choudhary P", "Escaramis et al."),
                  choose.model.ch.p = FALSE, choose.model.es = FALSE)
  )$result
  res <- tdi(agreement_limit(x = "x", y = "y", data = r), p = 0.9,
             method = "tolerance")
  d <- r$x - r$y
  # Escaramis et al.: REML SD, same estimate
  expect_equal(unname(res$estimate), ta$tdi.es.p, tolerance = 1e-6)
  # Choudhary P: the ML SD
  expect_equal(tdi_exact(mean(d), sd(d) * sqrt((n - 1) / n), 0.9),
               ta$tdi.ch.p, tolerance = 1e-6)
  # TDIagree's bound counts measurements, not differences (n = 2 * subjects,
  # df = subjects); with those inputs the same formula reproduces it
  inp <- data.frame(bias = mean(d), se = sd(d) / sqrt(2 * n), df_bias = n,
                    sd = sd(d), df_sd = n, scale_sd = TRUE)
  expect_equal(.tdi_tol(inp, list(type = "df", nu = n), 0.9, NULL, 0.95)$upper,
               ta$ub.es.p, tolerance = 1e-6)
  # with one difference per subject, the bound is wider
  expect_gt(res$conf.int[2], ta$ub.es.p)
})
