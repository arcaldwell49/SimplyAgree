# balanced replicates: 3 of each method per subject
bls_sim <- function(n = 25, m = 3, seed = 5, sx = 0.6, sy = 0.9) {
  set.seed(seed)
  tx <- rnorm(n, 10, 3)
  X <- sapply(seq_len(m), function(j) tx + rnorm(n, 0, sx))
  Y <- sapply(seq_len(m), function(j) 1 + 1.1 * tx + rnorm(n, 0, sy))
  list(wide = data.frame(X, Y),
       long = data.frame(id = rep(seq_len(n), m), x = as.vector(X),
                         y = as.vector(Y)))
}

test_that("BLS and CBLS reproduce BivRegBLS with replicates", {
  skip_if_not_installed("BivRegBLS")
  d <- bls_sim()
  fit <- cbls_reg(y ~ x, data = d$long, id = "id")
  expect_s3_class(fit, c("simple_bls", "simple_eiv"))
  b <- BivRegBLS::BLS(data = d$wide, xcol = 1:3, ycol = 4:6, npoints = 10,
                      xpred = c(6, 10, 14))
  est <- b$Estimate.BLS
  expect_equal(unname(fit$coefficients),
               as.numeric(est[1:2, "Estimate"]), tolerance = 1e-8)
  expect_equal(fit$model_table$se, as.numeric(est[1:2, "Std Error"]),
               tolerance = 1e-8)
  expect_equal(fit$model_table$lower.ci, as.numeric(est[1:2, 4]),
               tolerance = 1e-8)
  # CBLS: BivRegBLS uses D = y - x, cbls_reg() D = x - y
  cb <- BivRegBLS::CBLS(data = d$wide, xcol = 1:3, ycol = 4:6, npoints = 10)
  expect_equal(unname(fit$cbls$coefficients),
               -as.numeric(cb$Estimate.CBLS[1:2, "Estimate"]), tolerance = 1e-8)
  expect_equal(fit$cbls$model_table$se,
               as.numeric(cb$Estimate.CBLS[1:2, "Std Error"]), tolerance = 1e-8)
  # confidence and prediction intervals, single and mean of 3 replicates
  xp <- b$xpred.BLS
  ci <- predict(fit, c(6, 10, 14), interval = "confidence")
  pi <- predict(fit, c(6, 10, 14), interval = "prediction")
  expect_equal(ci$lwr, xp[["95% CI Lower"]], tolerance = 1e-8)
  expect_equal(ci$upr, xp[["95% CI Upper"]], tolerance = 1e-8)
  expect_equal(pi$lwr, xp[["95% PI Lower"]], tolerance = 1e-8)
  expect_equal(pi$upr, xp[["95% PI Upper"]], tolerance = 1e-8)
  b3 <- BivRegBLS::BLS(data = d$wide, xcol = 1:3, ycol = 4:6, npoints = 10,
                       xpred = c(6, 10, 14), qx = 3, qy = 3)
  gi <- predict(fit, c(6, 10, 14), interval = "prediction",
                reps_x = 3, reps_y = 3)
  expect_equal(gi$lwr, b3$xpred.BLS[["95% GI Lower"]], tolerance = 1e-8)
  expect_equal(gi$upr, b3$xpred.BLS[["95% GI Upper"]], tolerance = 1e-8)
})

test_that("known error variances reproduce BivRegBLS, including CBLS PIs", {
  skip_if_not_installed("BivRegBLS")
  d <- bls_sim(m = 1, n = 30, seed = 8)
  dat <- data.frame(x = d$wide[[1]], y = d$wide[[2]])
  fit <- cbls_reg(y ~ x, data = dat, var_x = 0.5, var_y = 0.5)
  expect_equal(fit$errors$source, "known")
  b <- BivRegBLS::BLS(data = dat, xcol = 1, ycol = 2, var.x = 0.5,
                      var.y = 0.5, npoints = 10, xpred = c(5, 12))
  expect_equal(unname(fit$coefficients),
               as.numeric(b$Estimate.BLS[1:2, "Estimate"]), tolerance = 1e-8)
  pi <- predict(fit, c(5, 12), interval = "prediction")
  expect_equal(pi$upr, b$xpred.BLS[["95% PI Upper"]], tolerance = 1e-8)
  # CBLS prediction intervals: BivRegBLS leaves out the covariance of the
  # errors of D and M, which is zero when var_x = var_y
  cb <- BivRegBLS::CBLS(data = dat, xcol = 1, ycol = 2, var.x = 0.5,
                        var.y = 0.5, npoints = 10, xpred = c(10, 14))
  pm <- predict(fit, c(10, 14), type = "md", interval = "prediction")
  expect_equal(pm$fit, -cb$xpred.CBLS$Ypred, tolerance = 1e-8)
  expect_equal(pm$upr, -cb$xpred.CBLS[["95% PI Lower"]], tolerance = 1e-8)
})

test_that("with constant variances BLS is Deming regression", {
  data("reps")
  fit <- cbls_reg(y ~ x, data = reps, var_x = 0.3, var_y = 0.6)
  dem <- dem_reg(y ~ x, data = reps, error.ratio = 0.5)
  expect_equal(unname(fit$coefficients), unname(dem$coefficients),
               tolerance = 1e-8)
  expect_equal(fit$error.ratio, 0.5)
})

test_that("unequal replicates: BLS minimises the weighted sum of squares", {
  set.seed(3)
  n <- 30
  tx <- rnorm(n, 20, 5)
  n_i <- sample(1:4, n, replace = TRUE)
  id <- rep(seq_len(n), n_i)
  long <- data.frame(id = id,
                     x = tx[id] + rnorm(length(id), 0, 1),
                     y = 2 + 0.9 * tx[id] + rnorm(length(id), 0, 1.5))
  fit <- cbls_reg(y ~ x, data = long, id = "id")
  expect_true(fit$converged)
  s <- fit$model
  expect_false(length(unique(s$var_x)) == 1)
  obj <- function(b) sum((s$y - b[1] - b[2] * s$x)^2 /
                           (s$var_y + b[2]^2 * s$var_x))
  b_fit <- unname(fit$coefficients)
  h <- 1e-6
  grad <- c(obj(b_fit + c(h, 0)) - obj(b_fit - c(h, 0)),
            obj(b_fit + c(0, h)) - obj(b_fit - c(0, h))) / (2 * h)
  expect_lt(max(abs(grad)), 1e-4)
  opt <- stats::nlminb(c(0, 1), obj)$par
  # a general optimiser stops close to, not exactly at, the minimum
  expect_equal(b_fit, opt, tolerance = 1e-4)
  # the CBLS line is the BLS line in the Bland-Altman plot
  a <- fit$coefficients[[1]]
  b <- fit$coefficients[[2]]
  expect_equal(unname(fit$cbls$coefficients),
               c(-2 * a / (1 + b), 2 * (1 - b) / (1 + b)), tolerance = 1e-6)
})

test_that("only the error ratio: estimates, but no prediction intervals", {
  data("ba1986")
  r <- cbls_reg(mini1 ~ wright1, data = ba1986, error.ratio = 2)
  k <- cbls_reg(mini1 ~ wright1, data = ba1986, var_x = 200, var_y = 100)
  expect_equal(r$errors$source, "ratio")
  expect_false(r$errors$known)
  expect_equal(r$coefficients, k$coefficients, tolerance = 1e-8)
  expect_equal(r$vcov, k$vcov, tolerance = 1e-8)
  expect_error(predict(r, 300, interval = "prediction"), "only their ratio")
  expect_error(cbls_reg(mini1 ~ wright1, data = ba1986),
               "cannot be estimated")
})

test_that("methods work", {
  data("reps")
  fit <- cbls_reg(y ~ x, data = reps, id = "id")
  expect_output(print(fit), "Bivariate Least Squares")
  expect_output(summary(fit), "CBLS")
  expect_equal(dim(confint(fit)), c(2, 2))
  expect_equal(length(fitted(fit)), nrow(fit$model))
  expect_equal(residuals(fit, resid_type = "raw"),
               fit$model$y - fitted(fit))
  expect_s3_class(plot(fit), "ggplot")
  expect_s3_class(plot(fit, type = "md", interval = "prediction"), "ggplot")
  expect_s3_class(plot(fit, interval = "confidence"), "ggplot")
  expect_s3_class(check(fit), "patchwork")
  p <- predict(fit, data.frame(x = c(4, 6)), interval = "confidence",
               se.fit = TRUE)
  expect_named(p, c("fit", "se.fit", "df"))
  expect_equal(predict(fit, c(4, 6)), unname(coef(fit)[1] + coef(fit)[2] * c(4, 6)))
  jt <- joint_test(fit)
  expect_s3_class(jt, "htest")
  expect_match(jt$method, "Wald, F-based")
  expect_error(joint_test(fit, method = "sadler"), "BLS fits")
  expect_s3_class(plot_joint(fit), "ggplot")
  nomod <- cbls_reg(y ~ x, data = reps, id = "id", model = FALSE)
  expect_error(plot(nomod), "model = TRUE")
  expect_equal(predict(nomod, c(4, 6)), predict(fit, c(4, 6)))
})

test_that("input validation", {
  data("reps")
  expect_error(cbls_reg(y ~ x, data = reps, id = "id", conf.level = 2),
               "between 0 and 1")
  expect_error(cbls_reg(y ~ x, data = reps, var_x = -1, var_y = 1),
               "positive")
  expect_error(cbls_reg(y ~ x, data = reps, var_x = 1, var_y = 1,
                        df_var_x = 0), "positive")
  expect_error(cbls_reg("y ~ x", data = reps), "formula")
  expect_message(cbls_reg(y ~ x, data = reps, id = "id", var_x = 0.3),
                 "var_x is given")
})
