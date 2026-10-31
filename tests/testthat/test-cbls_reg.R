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
  fit <- cbls_reg(y ~ x, data = d$long, id = "id", equation_error = FALSE)
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
  fit <- cbls_reg(y ~ x, data = dat, var_x = 0.5, var_y = 0.5,
                  equation_error = FALSE)
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
  # scale invariance (without equation error, which needs the variances)
  k <- cbls_reg(mini1 ~ wright1, data = ba1986, var_x = 200, var_y = 100,
                equation_error = FALSE)
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

test_that("the simultaneous band matches the joint test and BivRegBLS", {
  d <- bls_sim()
  fit <- cbls_reg(y ~ x, data = d$long, id = "id", equation_error = FALSE)
  n <- fit$df.residual + 2
  xs <- c(6, 10, 14)
  band <- predict(fit, xs, interval = "band")
  se <- predict(fit, xs, se.fit = TRUE)$se.fit
  expect_equal(band$upr - band$fit, sqrt(2 * qf(0.95, 2, n - 2)) * se)
  # wider than the pointwise intervals
  ci <- predict(fit, xs, interval = "confidence")
  expect_true(all(band$upr > ci$upr))
  # duality with joint_test(): a line parallel to the fit, shifted by delta,
  # stays inside the band (touching it where the SE of the line is smallest)
  # exactly when the joint test does not reject it
  vc <- vcov(fit)
  delta <- sqrt(2 * qf(0.95, 2, n - 2)) * sqrt(vc[1, 1] - vc[1, 2]^2 / vc[2, 2])
  p_at <- function(k) {
    joint_test(fit, ideal_intercept = coef(fit)[[1]] + k * delta,
               ideal_slope = coef(fit)[[2]])$p.value
  }
  expect_gt(p_at(0.999), 0.05)
  expect_lt(p_at(1.001), 0.05)
  skip_if_not_installed("BivRegBLS")
  b <- BivRegBLS::BLS(data = d$wide, xcol = 1:3, ycol = 4:6, npoints = 10,
                      xpred = xs)
  # BivRegBLS multiplies by sqrt((n - 1) / (n - 2)) in addition
  expect_equal(b$xpred.BLS[["95% CB Upper"]] - band$fit,
               (band$upr - band$fit) * sqrt((n - 1) / (n - 2)),
               tolerance = 1e-8)
  expect_s3_class(plot(fit, interval = "band"), "ggplot")
})

# proportional error: CV 5% for x, 8% for y
vf_sim <- function(n = 40, m = 3, seed = 21) {
  set.seed(seed)
  t <- runif(n, 10, 100)
  id <- rep(seq_len(n), each = m)
  data.frame(id = id, x = t[id] * (1 + rnorm(n * m, 0, 0.05)),
             y = (2 + 1.05 * t[id]) * (1 + rnorm(n * m, 0, 0.08)))
}

test_that("a constant variance function equals a constant variance", {
  data("reps")
  f1 <- cbls_reg(y ~ x, data = reps, var_x = 0.3, var_y = 0.6)
  f2 <- cbls_reg(y ~ x, data = reps, var_x = function(l) 0.3,
                 var_y = function(l) rep(0.6, length(l)))
  expect_equal(f1$coefficients, f2$coefficients)
  expect_equal(f1$vcov, f2$vcov)
  expect_equal(predict(f1, 5, interval = "prediction"),
               predict(f2, 5, interval = "prediction"))
})

test_that("variance functions are evaluated at the estimated true values", {
  dat <- vf_sim()
  fit <- suppressMessages(cbls_reg(y ~ x, data = dat, id = "id",
                                   var_x = function(l) (0.05 * l)^2,
                                   var_y = function(l) (0.08 * l)^2))
  expect_true(fit$converged)
  expect_gt(fit$iterations[["passes"]], 1)
  s <- fit$model
  b <- unname(fit$coefficients)
  e <- s$y - b[1] - b[2] * s$x
  s2 <- s$var_y + b[2]^2 * s$var_x
  lev_x <- s$x + b[2] * s$var_x * e / s2
  lev_y <- b[1] + b[2] * lev_x
  # a fixed point: the stored variances are the functions at these levels
  expect_equal(s$var_x, (0.05 * lev_x)^2 / s$n_x, tolerance = 1e-6)
  expect_equal(s$var_y, (0.08 * lev_y)^2 / s$n_y, tolerance = 1e-6)
  # prediction intervals widen with the level
  p <- predict(fit, c(20, 80), interval = "prediction")
  expect_gt(p$upr[2] - p$lwr[2], 3 * (p$upr[1] - p$lwr[1]))
  expect_output(summary(fit), "given function of the level")
})

test_that("proportional and power error models are estimated from replicates", {
  dat <- vf_sim()
  fp <- cbls_reg(y ~ x, data = dat, id = "id", var_model = "proportional")
  expect_equal(fp$errors$model_x, "proportional")
  expect_equal(unname(fp$errors$params_x), 0.05, tolerance = 0.15)
  expect_equal(unname(fp$errors$params_y), 0.08, tolerance = 0.15)
  expect_equal(fp$errors$df_var_x, 80)
  # the proportional CV is the pooled squared relative deviation
  d <- dat[dat$id %in% fp$model$id, ]
  mx <- ave(d$x, d$id)
  expect_equal(unname(fp$errors$params_x)^2,
               sum((d$x - mx)^2 / mx^2) / 80, tolerance = 1e-10)
  fw <- cbls_reg(y ~ x, data = dat, id = "id", var_model = "power")
  expect_named(fw$errors$params_x, c("sigma", "theta"))
  expect_equal(fw$errors$df_var_x, 79)
  expect_output(summary(fw), "power")
  # close to the fit with the true variance functions
  fk <- suppressMessages(cbls_reg(y ~ x, data = dat, id = "id",
                                  var_x = function(l) (0.05 * l)^2,
                                  var_y = function(l) (0.08 * l)^2))
  expect_equal(unname(fp$coefficients[2]), unname(fk$coefficients[2]),
               tolerance = 0.01)
})

test_that("variance function input validation", {
  dat <- vf_sim()
  neg <- dat
  neg$x[1] <- -5
  expect_error(cbls_reg(y ~ x, data = transform(neg, x = x - 50), id = "id",
                        var_model = "proportional"), "positive measurements")
  expect_error(cbls_reg(y ~ x, data = dat[!duplicated(dat$id), ],
                        error.ratio = 2, var_model = "proportional"),
               "must be constant")
  expect_error(cbls_reg(y ~ x, data = dat, var_x = function(l) -l,
                        var_y = 1), "positive value")
  expect_error(cbls_reg(y ~ x, data = dat, var_x = "a", var_y = 1),
               "number or a function")
})

test_that("dispersion and equation error", {
  # equation error: true values scattered about the line (SD 1.5)
  set.seed(9)
  n <- 40
  t <- rnorm(n, 20, 5)
  dev <- rnorm(n, 0, 1.5)
  id <- rep(seq_len(n), each = 3)
  long <- data.frame(id = id, x = t[id] + rnorm(3 * n, 0, 0.6),
                     y = 1 + 1.1 * t[id] + dev[id] + rnorm(3 * n, 0, 0.9))
  f1 <- cbls_reg(y ~ x, data = long, id = "id")
  f0 <- cbls_reg(y ~ x, data = long, id = "id", equation_error = FALSE)
  d <- f1$dispersion
  expect_gt(d$s2, 2)
  expect_lt(d$p.value, 0.001)
  expect_true(f1$converged)
  # common tau: the residual variance gains 2 b^2 tau2 (here all of it came
  # from y, SD 1.5)
  b <- unname(coef(f1))
  expect_equal(sqrt(2 * b[2]^2 * d$tau2), 1.5, tolerance = 0.3)
  # the moment equation holds at the final fit
  s <- f1$model
  e <- s$y - b[1] - b[2] * s$x
  expect_equal(sum(e^2 / (s$var_y + b[2]^2 * s$var_x + 2 * b[2]^2 * d$tau2)),
               n - 2, tolerance = 1e-6)
  # without equation error the fit is BLS (BivRegBLS); with it, the line
  # moves a little and the prediction intervals widen
  expect_equal(f0$dispersion$tau2, 0)
  expect_false(isTRUE(all.equal(coef(f1), coef(f0))))
  # symmetric: x on y gives the same line
  f_yx <- cbls_reg(x ~ y, data = long, id = "id")
  expect_equal(unname(coef(f_yx)[2]), 1 / b[2], tolerance = 1e-6)
  expect_equal(unname(coef(f_yx)[1]), -b[1] / b[2], tolerance = 1e-6)
  p1 <- predict(f1, 20, interval = "prediction")
  p0 <- predict(f0, 20, interval = "prediction")
  expect_gt(p1$upr - p1$lwr, 1.5 * (p0$upr - p0$lwr))
  m1 <- predict(f1, 20, type = "md", interval = "prediction")
  m0 <- predict(f0, 20, type = "md", interval = "prediction")
  expect_gt(m1$upr - m1$lwr, m0$upr - m0$lwr)
  expect_output(summary(f1), "Equation error: SD")
  # without equation error the estimate is usually 0 and nothing changes
  dd <- bls_sim(n = 40, seed = 2)
  g1 <- cbls_reg(y ~ x, data = dd$long, id = "id")
  if (g1$dispersion$tau2 == 0) {
    expect_equal(predict(g1, 10, interval = "prediction"),
                 predict(cbls_reg(y ~ x, data = dd$long, id = "id",
                                  equation_error = FALSE),
                         10, interval = "prediction"))
  }
  # only the ratio: no test
  data("ba1986")
  r <- cbls_reg(mini1 ~ wright1, data = ba1986, error.ratio = 2)
  expect_true(is.na(r$dispersion$p.value))
  expect_error(cbls_reg(y ~ x, data = long, id = "id", equation_error = NA),
               "TRUE or FALSE")
})
