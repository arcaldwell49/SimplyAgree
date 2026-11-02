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
  fit <- bls_reg(y ~ x, data = d$long, id = "id", equation_error = FALSE,
                 se_method = "bivregbls")
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
  # CBLS: BivRegBLS uses D = y - x, bls_reg() D = x - y
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
  fit <- bls_reg(y ~ x, data = dat, var_x = 0.5, var_y = 0.5,
                  se_method = "bivregbls",
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
  fit <- bls_reg(y ~ x, data = reps, var_x = 0.3, var_y = 0.6)
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
  fit <- bls_reg(y ~ x, data = long, id = "id")
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
  r <- bls_reg(mini1 ~ wright1, data = ba1986, error.ratio = 2)
  # scale invariance (without equation error, which needs the variances)
  k <- bls_reg(mini1 ~ wright1, data = ba1986, var_x = 200, var_y = 100,
                equation_error = FALSE)
  expect_equal(r$errors$source, "ratio")
  expect_false(r$errors$known)
  expect_equal(r$coefficients, k$coefficients, tolerance = 1e-8)
  rb <- bls_reg(mini1 ~ wright1, data = ba1986, error.ratio = 2,
                se_method = "bivregbls")
  kb <- bls_reg(mini1 ~ wright1, data = ba1986, var_x = 200, var_y = 100,
                equation_error = FALSE, se_method = "bivregbls")
  expect_equal(rb$vcov, kb$vcov, tolerance = 1e-8)
  # with only the ratio, the errors-in-variables term uses the variances
  # scaled by the dispersion: the same as giving those variances
  s2 <- r$dispersion$s2
  ks <- bls_reg(mini1 ~ wright1, data = ba1986, var_x = 2 * s2, var_y = s2,
                equation_error = FALSE)
  expect_equal(r$vcov, ks$vcov, tolerance = 1e-6)
  expect_error(predict(r, 300, interval = "prediction"), "only their ratio")
  expect_error(bls_reg(mini1 ~ wright1, data = ba1986),
               "cannot be estimated")
})

test_that("methods work", {
  data("reps")
  fit <- bls_reg(y ~ x, data = reps, id = "id")
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
  nomod <- bls_reg(y ~ x, data = reps, id = "id", model = FALSE)
  expect_error(plot(nomod), "model = TRUE")
  expect_equal(predict(nomod, c(4, 6)), predict(fit, c(4, 6)))
})

test_that("input validation", {
  data("reps")
  expect_error(bls_reg(y ~ x, data = reps, id = "id", conf.level = 2),
               "between 0 and 1")
  expect_error(bls_reg(y ~ x, data = reps, var_x = -1, var_y = 1),
               "positive")
  expect_error(bls_reg(y ~ x, data = reps, var_x = 1, var_y = 1,
                        df_var_x = 0), "positive")
  expect_error(bls_reg("y ~ x", data = reps), "formula")
  expect_message(bls_reg(y ~ x, data = reps, id = "id", var_x = 0.3),
                 "var_x is given")
})

test_that("the simultaneous band matches the joint test and BivRegBLS", {
  d <- bls_sim()
  fit <- bls_reg(y ~ x, data = d$long, id = "id", equation_error = FALSE,
                 se_method = "bivregbls")
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
  f1 <- bls_reg(y ~ x, data = reps, var_x = 0.3, var_y = 0.6)
  f2 <- bls_reg(y ~ x, data = reps, var_x = function(l) 0.3,
                 var_y = function(l) rep(0.6, length(l)))
  expect_equal(f1$coefficients, f2$coefficients)
  expect_equal(f1$vcov, f2$vcov)
  expect_equal(predict(f1, 5, interval = "prediction"),
               predict(f2, 5, interval = "prediction"))
})

test_that("variance functions are evaluated at the estimated true values", {
  dat <- vf_sim()
  fit <- suppressMessages(bls_reg(y ~ x, data = dat, id = "id",
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
  fp <- bls_reg(y ~ x, data = dat, id = "id", var_model = "proportional")
  expect_equal(fp$errors$model_x, "proportional")
  expect_equal(unname(fp$errors$params_x), 0.05, tolerance = 0.15)
  expect_equal(unname(fp$errors$params_y), 0.08, tolerance = 0.15)
  expect_equal(fp$errors$df_var_x, 80)
  # the proportional CV is the pooled squared relative deviation
  d <- dat[dat$id %in% fp$model$id, ]
  mx <- ave(d$x, d$id)
  expect_equal(unname(fp$errors$params_x)^2,
               sum((d$x - mx)^2 / mx^2) / 80, tolerance = 1e-10)
  fw <- bls_reg(y ~ x, data = dat, id = "id", var_model = "power")
  expect_named(fw$errors$params_x, c("sigma", "theta"))
  expect_equal(fw$errors$df_var_x, 79)
  expect_output(summary(fw), "power")
  # close to the fit with the true variance functions
  fk <- suppressMessages(bls_reg(y ~ x, data = dat, id = "id",
                                  var_x = function(l) (0.05 * l)^2,
                                  var_y = function(l) (0.08 * l)^2))
  expect_equal(unname(fp$coefficients[2]), unname(fk$coefficients[2]),
               tolerance = 0.01)
})

test_that("variance function input validation", {
  dat <- vf_sim()
  neg <- dat
  neg$x[1] <- -5
  expect_error(bls_reg(y ~ x, data = transform(neg, x = x - 50), id = "id",
                        var_model = "proportional"), "positive measurements")
  expect_error(bls_reg(y ~ x, data = dat[!duplicated(dat$id), ],
                        error.ratio = 2, var_model = "proportional"),
               "must be constant")
  expect_error(bls_reg(y ~ x, data = dat, var_x = function(l) -l,
                        var_y = 1), "positive value")
  expect_error(bls_reg(y ~ x, data = dat, var_x = "a", var_y = 1),
               "function of the level")
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
  f1 <- bls_reg(y ~ x, data = long, id = "id")
  f0 <- bls_reg(y ~ x, data = long, id = "id", equation_error = FALSE)
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
  f_yx <- bls_reg(x ~ y, data = long, id = "id")
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
  g1 <- bls_reg(y ~ x, data = dd$long, id = "id")
  if (g1$dispersion$tau2 == 0) {
    expect_equal(predict(g1, 10, interval = "prediction"),
                 predict(bls_reg(y ~ x, data = dd$long, id = "id",
                                  equation_error = FALSE),
                         10, interval = "prediction"))
  }
  # only the ratio: no test
  data("ba1986")
  r <- bls_reg(mini1 ~ wright1, data = ba1986, error.ratio = 2)
  expect_true(is.na(r$dispersion$p.value))
  expect_error(bls_reg(y ~ x, data = long, id = "id", equation_error = NA),
               "TRUE or FALSE")
})

test_that("tolerance limits (experimental)", {
  d <- bls_sim(n = 30, seed = 4)
  fit <- bls_reg(y ~ x, data = d$long, id = "id")
  xs <- c(6, 10, 14)
  pi <- predict(fit, xs, interval = "prediction")
  tj <- predict(fit, xs, interval = "tolerance")
  ti <- predict(fit, xs, interval = "tolerance", bound_type = "iu")
  expect_equal(tj$fit, pi$fit)
  expect_true(all(tj$upr > pi$upr & tj$lwr < pi$lwr))
  expect_true(all(ti$upr > pi$upr & ti$lwr < pi$lwr))
  # higher confidence or content gives wider limits
  t99 <- predict(fit, xs, interval = "tolerance", tol_level = 0.99)
  expect_true(all(t99$upr > tj$upr))
  t90 <- predict(fit, xs, interval = "tolerance", level = 0.90)
  expect_true(all(t90$upr < tj$upr))
  # known variances, no equation error: the spread s is known, so the joint
  # limits are the 95% quantile of |N(c, s^2)|, c a t bound for the line
  dk <- bls_sim(m = 1, n = 30, seed = 8)
  datk <- data.frame(x = dk$wide[[1]], y = dk$wide[[2]])
  fk <- bls_reg(y ~ x, data = datk, var_x = 0.36, var_y = 0.81,
                 equation_error = FALSE)
  se <- predict(fk, xs, se.fit = TRUE)$se.fit
  s2 <- 0.81 + coef(fk)[[2]]^2 * 0.36
  tk <- predict(fk, xs, interval = "tolerance")
  cl <- qt(0.975, fk$df.residual) * se
  expect_equal(tk$upr - tk$fit,
               sqrt(s2) * sqrt(qchisq(0.95, 1, ncp = cl^2 / s2)),
               tolerance = 1e-5)
  # Bland-Altman plot and plotting
  expect_named(predict(fit, 10, type = "md", interval = "tolerance"),
               c("fit", "lwr", "upr"))
  expect_s3_class(plot(fit, type = "md", interval = "tolerance"), "ggplot")
  expect_error(predict(fit, xs, interval = "tolerance", tol_level = 2),
               "tol_level")
  expect_error(bls_reg(y ~ x, data = d$long, id = "id", tol_level = 0),
               "tol_level")
  r <- bls_reg(y ~ x, data = d$long[!duplicated(d$long$id), ],
                error.ratio = 1)
  expect_error(predict(r, xs, interval = "tolerance"), "only their ratio")
})

test_that("with equation error and one measurement each, tolerance limits use Wallis' factor", {
  set.seed(14)
  n <- 30
  t <- rnorm(n, 20, 5)
  dat <- data.frame(x = t + rnorm(n, 0, 1) + rnorm(n, 0, 0.6),
                    y = 1 + 1.1 * (t + rnorm(n, 0, 1)) + rnorm(n, 0, 0.9))
  f <- bls_reg(y ~ x, data = dat, var_x = 0.36, var_y = 0.81)
  expect_gt(f$dispersion$tau2, 0)
  xs <- c(15, 25)
  se <- predict(f, xs, se.fit = TRUE)$se.fit
  b <- coef(f)[[2]]
  s2 <- 0.81 + b^2 * 0.36 + 2 * b^2 * f$dispersion$tau2
  r <- sqrt((n - 2) / qchisq(0.05, n - 2))
  tl <- predict(f, xs, interval = "tolerance")
  # half-width r s sqrt(qchisq(p, 1, ncp = se^2 / s^2))
  expect_equal(tl$upr - tl$fit,
               r * sqrt(s2) * sqrt(qchisq(0.95, 1, ncp = se^2 / s2)),
               tolerance = 1e-6)
})

test_that("per-subject error variances match BivRegBLS BLS.ht", {
  skip_if_not_installed("BivRegBLS")
  set.seed(3)
  n <- 25
  mu <- runif(n, 10, 100)
  vx <- runif(n, 1, 9)
  vy <- runif(n, 2, 16)
  d <- data.frame(id = 1:n, x = mu + rnorm(n, sd = sqrt(vx)),
                  y = 2 + 1.1 * mu + rnorm(n, sd = sqrt(vy)),
                  vx = vx, vy = vy, dfx = 4, dfy = 4)
  fit <- bls_reg(y ~ x, data = d, var_x = "vx", var_y = "vy",
                  df_var_x = "dfx", df_var_y = "dfy",
                  equation_error = FALSE, se_method = "bivregbls")
  ref <- BivRegBLS::BLS.ht(data = d, xcol = "x", ycol = "y",
                           var.x.col = "vx", var.y.col = "vy",
                           df.var.x.col = "dfx", df.var.y.col = "dfy")
  est <- ref$Estimate.BLS.ht
  expect_equal(unname(coef(fit)), as.numeric(est[1:2, 2]), tolerance = 1e-6)
  expect_equal(fit$model_table$lower.ci, as.numeric(est[1:2, 4]),
               tolerance = 1e-6)
  expect_equal(fit$model_table$upper.ci, as.numeric(est[1:2, 5]),
               tolerance = 1e-6)
  expect_equal(fit$errors$model_x, "subject")
  expect_equal(fit$model$var_x, vx)
  # a new measurement gets the mean variance, Satterthwaite df
  expect_equal(fit$errors$var_x, mean(vx))
  expect_equal(fit$errors$df_var_x, sum(vx)^2 / sum(vx^2 / 4))
  expect_output(summary(fit), "one per subject")
  pr <- predict(fit, newdata = data.frame(x = c(30, 60)),
                interval = "prediction")
  expect_true(all(pr[, "lwr"] < pr[, "fit"] & pr[, "fit"] < pr[, "upr"]))
})

test_that("per-subject variance columns with replicates and validation", {
  data("reps")
  r <- reps[stats::complete.cases(reps), ]
  r$vx <- 0.5 + as.numeric(factor(r$id)) / 10
  r$vy <- 1
  fit <- bls_reg(y ~ x, data = r, id = "id", var_x = "vx", var_y = "vy")
  sub_v <- tapply(r$vx, r$id, `[`, 1)
  n_x <- tapply(r$x, r$id, length)
  expect_equal(fit$model$var_x,
               as.vector(sub_v[fit$model$id] / n_x[fit$model$id]))
  expect_equal(fit$errors$df_var_x, Inf)
  r$vbad <- r$vx + seq_len(nrow(r)) / 100
  expect_error(bls_reg(y ~ x, data = r, id = "id", var_x = "vbad",
                        var_y = "vy"), "one value per subject")
  r$vneg <- -1
  expect_error(bls_reg(y ~ x, data = r, id = "id", var_x = "vneg",
                        var_y = "vy"), "positive value")
  expect_error(bls_reg(y ~ x, data = r, var_x = "nope", var_y = 1),
               "name of a column")
  r$dfx <- 3
  expect_error(bls_reg(y ~ x, data = r, id = "id", var_x = 1, var_y = 1,
                        df_var_x = "dfx"), "column name only when")
})

test_that("transform = 'log' fits the logs and back-transforms", {
  set.seed(5)
  n <- 30
  id <- rep(1:n, each = 2)
  mu <- exp(runif(n, log(5), log(200)))
  d <- data.frame(id = id, x = mu[id] * exp(rnorm(2 * n, sd = 0.08)),
                  y = 1.3 * mu[id]^0.95 * exp(rnorm(2 * n, sd = 0.06)))
  fit <- bls_reg(y ~ x, data = d, id = "id", transform = "log")
  dl <- transform(d, x = log(x), y = log(y))
  fl <- bls_reg(y ~ x, data = dl, id = "id")
  expect_equal(coef(fit), coef(fl))
  expect_equal(fit$transform, "log")
  pl <- predict(fl, newdata = log(c(10, 100)), interval = "prediction")
  pr <- predict(fit, newdata = c(10, 100), interval = "prediction")
  expect_equal(pr, exp(pl))
  expect_equal(predict(fit, newdata = log(c(10, 100)),
                       interval = "prediction", back_transform = FALSE), pl)
  # Bland-Altman plot: ratio x / y at the geometric mean
  expect_equal(predict(fit, newdata = data.frame(avg = 50), type = "md"),
               exp(predict(fl, newdata = data.frame(avg = log(50)),
                           type = "md")))
  se <- predict(fit, newdata = 10, se.fit = TRUE)
  expect_equal(se$fit, exp(predict(fl, newdata = log(10))))
  expect_output(print(fit), "Fitted on the log scale")
  expect_output(summary(fit), "Fitted on the log scale")
  expect_s3_class(plot(fit, interval = "prediction"), "ggplot")
  expect_s3_class(plot(fit, type = "md", interval = "confidence"), "ggplot")
  expect_error(predict(fit, newdata = -1), "positive")
  expect_error(bls_reg(y ~ x, data = transform(d, x = x - 10), id = "id",
                        transform = "log"), "positive measurements")
  expect_error(bls_reg(y ~ x, data = d, id = "id", transform = "log",
                        var_model = "proportional"), "constant on the log")
})

test_that("default SEs: errors-in-variables sandwich plus the delta term", {
  set.seed(11)
  n <- 30
  t <- rnorm(n, 10, 2)
  dat <- data.frame(x = t + rnorm(n, 0, 1), y = 1 + 1.1 * t + rnorm(n, 0, 1))
  # known variances: the sandwich only (no estimated parameters)
  fit <- bls_reg(y ~ x, data = dat, var_x = 1, var_y = 1,
                 equation_error = FALSE)
  fb <- bls_reg(y ~ x, data = dat, var_x = 1, var_y = 1,
                equation_error = FALSE, se_method = "bivregbls")
  expect_equal(coef(fit), coef(fb))
  b <- unname(coef(fit))
  e <- dat$y - b[1] - b[2] * dat$x
  w <- 1 / (1 + b[2]^2)
  s2 <- sum(w * e^2) / (n - 2)
  xh <- dat$x + b[2] * e * w
  R <- matrix(c(n * w, w * sum(xh), w * sum(xh), w * sum(xh^2)), 2)
  A <- R
  A[2, 2] <- A[2, 2] - n * w * (1 - b[2]^2 * w)
  expect_equal(unname(fit$vcov), unname(solve(A) %*% R %*% solve(A) * s2))
  expect_equal(unname(fb$vcov), unname(solve(matrix(
    c(n * w, w * sum(dat$x), w * sum(dat$x), w * sum(dat$x^2)), 2)) * s2))
  # larger than BivRegBLS's when x has non-negligible error
  expect_gt(fit$vcov[2, 2], fb$vcov[2, 2])
  expect_equal(fit$call$se_method, "delta")

  # estimated variances add the delta-method term
  d <- bls_sim()
  f_d <- bls_reg(y ~ x, data = d$long, id = "id", equation_error = FALSE)
  f_b <- bls_reg(y ~ x, data = d$long, id = "id", equation_error = FALSE,
                 se_method = "bivregbls")
  expect_gt(f_d$vcov[2, 2], f_b$vcov[2, 2])
  expect_gt(f_d$cbls$vcov[2, 2], f_b$cbls$vcov[2, 2])
  expect_error(bls_reg(y ~ x, data = dat, var_x = 1, var_y = 1,
                       se_method = "nope"))
})
