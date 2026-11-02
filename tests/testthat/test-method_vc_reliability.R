context("method_vc: within-item reliability and residual SDs at zero")

# true_ij = mu_i + w_ij; ref = true + e; alt = mu_i + b_w * w_ij + u_i + e,
# with a method-by-item effect u_i (sd 0.25), in the
# long format of method_vc() (b_w: within-item slope of alt)
trend_dat = function(n = 60, m = 4, sd_w = 0.5, b_w = 1, sd_alt = 0.25,
                     seed = 1) {
  set.seed(seed)
  id = rep(seq_len(n), each = m)
  mu = rep(rnorm(n, 5, 1), each = m)
  w = rnorm(n * m, 0, sd_w)
  u = rep(rnorm(n, 0, 0.25), each = m)
  repl = rep(seq_len(m), n)
  rbind(data.frame(id = id, repl = repl, method = "ref",
                   y = mu + w + rnorm(n * m, 0, 0.25)),
        data.frame(id = id, repl = repl, method = "alt",
                   y = mu + b_w * w + u + rnorm(n * m, 0, sd_alt)))
}

test_that("reliability is omega^2 / (omega^2 + sigma^2) with a logit CI", {
  dat = trend_dat()
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked")
  rl = fit$reliability
  vc = fit$varcomp
  om2 = vc$var[vc$component == "omega"]
  s2 = stats::setNames(vc$var[vc$component == "sigma"],
                       vc$method[vc$component == "sigma"])
  expect_equal(rl$method, c("alt", "ref", "average"))
  expect_equal(rl$reliability,
               unname(om2 / (om2 + c(s2, mean(s2)))))
  expect_equal(rl$omega, rep(sqrt(om2), 3))
  # delta method on logit R = log omega^2 - log sigma^2
  j_om = which(vc$component == "omega")
  j_s = which(vc$component == "sigma")
  g = numeric(nrow(vc))
  g[j_om] = 1 / om2
  g[j_s] = -c(0.5, 0.5) / mean(s2)
  se = sqrt(drop(t(g) %*% fit$vc_cov %*% g))
  lr = log(om2 / mean(s2))
  expect_equal(rl$lower[3], plogis(lr - qnorm(0.975) * se))
  expect_equal(rl$upper[3], plogis(lr + qnorm(0.975) * se))
  expect_true(all(rl$lower < rl$reliability & rl$reliability < rl$upper))

  # common SD: one row
  fc = method_vc(dat, y = "y", method = "method", id = "id",
                 replicate = "repl", replicates = "linked",
                 sigma = "common")
  expect_equal(nrow(fc$reliability), 1)
  expect_true(is.na(fc$reliability$method))

  # the F interval is available internally
  rf = mc_vc_reliability(vc, fit$vc_cov, fit$design, 0.95, ci = "F")
  expect_equal(rf$reliability, rl$reliability)
  expect_true(all(rf$lower < rf$reliability & rf$reliability < rf$upper))

  expect_output(print(fit), "Within-item Reliability")
  expect_output(print(summary(fit)), "Within-item reliability")
})

test_that("the average equals the ICC of item-centred data", {
  dat = trend_dat(sd_alt = 0.4)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked")
  dat$yc = dat$y - ave(dat$y, dat$id, dat$method)
  dat$target = paste(dat$id, dat$repl, sep = ":")
  icc = icc_test(yc ~ method | target, data = dat, model = "twoway",
                 type = "consistency")
  expect_equal(fit$reliability$reliability[3], unname(icc$estimate),
               tolerance = 0.02)
})

test_that("reliability only with linked replicates", {
  dat = trend_dat()
  # (tau can be at zero here: the occasion effects are ignored)
  fe = suppressWarnings(method_vc(dat, y = "y", method = "method",
                                  id = "id", replicate = "repl",
                                  replicates = "exchangeable"))
  expect_null(fe$reliability)
  f1 = suppressMessages(method_vc(dat[dat$repl == 1, ], y = "y",
                                  method = "method", id = "id"))
  expect_null(f1$reliability)
})

test_that("a residual SD at zero with linked replicates is noted", {
  # alt follows half of the within-item change: its SD goes to zero
  dat = trend_dat(n = 100, m = 6, sd_w = 1.5, b_w = 0.5)
  expect_no_warning(fit <- method_vc(dat, y = "y", method = "method",
                                     id = "id", replicate = "repl",
                                     replicates = "linked"))
  expect_equal(fit$design$sigma_zero, "alt")
  expect_output(print(fit), "Residual SD estimated at zero: alt")
  rl = fit$reliability
  expect_equal(rl$upper[rl$method == "alt"], 1)
  expect_true(is.na(rl$lower[rl$method == "alt"]))
  # not with a common SD
  fc = method_vc(dat, y = "y", method = "method", id = "id",
                 replicate = "repl", replicates = "linked", sigma = "common")
  expect_length(fc$design$sigma_zero, 0)
  expect_false(any(grepl("Residual SD estimated at zero",
                         capture.output(print(fc)))))
})

test_that("omega at zero gives a one-sided reliability interval", {
  # no within-item change: omega is estimated at zero and left out
  dat = trend_dat(sd_w = 0, seed = 3)
  fit = suppressWarnings(method_vc(dat, y = "y", method = "method",
                                   id = "id", replicate = "repl",
                                   replicates = "linked"))
  skip_if_not("omega" %in% fit$design$drop)
  rl = fit$reliability
  expect_equal(rl$reliability, rep(0, 3))
  expect_equal(rl$lower, rep(0, 3))
  expect_true(all(is.na(rl$upper)))
})
