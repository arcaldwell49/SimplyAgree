test_that("error_ratio_profile matches BivRegBLS FullCIs.XY", {
  skip_if_not_installed("BivRegBLS")
  data("ba1986")
  fit <- dem_reg(mini1 ~ wright1, data = ba1986)
  d <- data.frame(x = ba1986$wright1, y = ba1986$mini1)
  ref <- suppressWarnings(BivRegBLS::FullCIs.XY(data = d, xcol = 1, ycol = 2))
  # BivRegBLS: lambda = var(y) / var(x), with 1e-6 in place of 0 and Inf
  lam <- ref$Slopes[, 1]
  keep <- lam > 0 & is.finite(lam)
  prof <- error_ratio_profile(fit, ratios = 1 / lam[keep])
  m <- match(signif(1 / lam[keep], 10), signif(prof$table$ratio, 10))
  tab <- prof$table[m, ]
  expect_equal(tab$slope, ref$Slopes[keep, 2], tolerance = 1e-6)
  expect_equal(tab$slope_exact_lower, ref$Slopes[keep, 3], tolerance = 1e-6)
  expect_equal(tab$slope_exact_upper, ref$Slopes[keep, 4], tolerance = 1e-6)
  expect_equal(tab$slope_lower, ref$Slopes[keep, 5], tolerance = 1e-6)
  expect_equal(tab$slope_upper, ref$Slopes[keep, 6], tolerance = 1e-6)
  expect_equal(tab$intercept, ref$Intercepts[keep, 2], tolerance = 1e-6)
  expect_equal(tab$intercept_lower, ref$Intercepts[keep, 3], tolerance = 1e-6)
  expect_equal(tab$intercept_upper, ref$Intercepts[keep, 4], tolerance = 1e-6)
})

test_that("the ends of the profile are the two OLS regressions", {
  data("ba1986")
  fit <- dem_reg(mini1 ~ wright1, data = ba1986)
  prof <- error_ratio_profile(fit, ratios = c(0, Inf))
  ols_yx <- lm(mini1 ~ wright1, data = ba1986)
  ols_xy <- lm(wright1 ~ mini1, data = ba1986)
  expect_equal(prof$table$slope[1], unname(coef(ols_yx)[2]), tolerance = 1e-8)
  expect_equal(prof$table$slope[2], 1 / unname(coef(ols_xy)[2]),
               tolerance = 1e-8)
  # OLS of y on x: the BLS interval is the usual exact t interval
  ci <- confint(ols_yx)[2, ]
  expect_equal(unname(unlist(prof$table[1, c("slope_lower", "slope_upper")])),
               unname(ci), tolerance = 1e-8)
  expect_equal(unname(unlist(prof$table[1, c("slope_exact_lower",
                                             "slope_exact_upper")])),
               unname(ci), tolerance = 1e-8)
  # OLS of x on y: the exact interval inverts its t interval
  ci2 <- confint(ols_xy)[2, ]
  expect_equal(unname(unlist(prof$table[2, c("slope_exact_lower",
                                             "slope_exact_upper")])),
               sort(1 / unname(ci2)), tolerance = 1e-8)
})

test_that("each ratio reproduces the fit at that ratio", {
  data("ba1986")
  fit <- dem_reg(mini1 ~ wright1, data = ba1986, error.ratio = 2)
  prof <- error_ratio_profile(fit, ratios = 2)
  expect_equal(prof$table$slope, unname(coef(fit)[2]), tolerance = 1e-8)
  expect_equal(prof$table$intercept, unname(coef(fit)[1]), tolerance = 1e-6)
  # the profile's BLS intervals are BivRegBLS's (FullCIs.XY)
  bls <- bls_reg(mini1 ~ wright1, data = ba1986, error.ratio = 2,
                 se_method = "bivregbls")
  pb <- error_ratio_profile(bls, ratios = 2)
  expect_equal(pb$table$slope_lower, bls$model_table$lower.ci[2],
               tolerance = 1e-8)
  expect_equal(pb$table$intercept_upper, bls$model_table$upper.ci[1],
               tolerance = 1e-8)
  # joint test as joint_test(method = "wald")
  d <- coef(bls) - c(0, 1)
  f <- as.numeric(t(d) %*% solve(vcov(bls), d)) / 2
  expect_equal(pb$table$joint_F, f, tolerance = 1e-8)
})

test_that("the default grid, full intervals and identity range", {
  data("ba1986")
  prof <- error_ratio_profile(dem_reg(mini1 ~ wright1, data = ba1986))
  expect_s3_class(prof, "eiv_ratio_profile")
  expect_equal(nrow(prof$table), 63) # 0, 61 ratios, Inf (fitted ratio 1 in grid)
  expect_equal(prof$full$lower[1], min(prof$table$slope_lower))
  expect_equal(prof$full$upper[2], max(prof$table$intercept_upper))
  expect_equal(prof$full$term, c("slope", "intercept", "slope (exact)"))
  expect_equal(prof$identity, c(0, Inf))
  expect_output(print(prof), "Full 95% confidence intervals")
  expect_s3_class(plot(prof, type = "slope"), "ggplot")
  expect_s3_class(plot(prof, type = "intercept"), "ggplot")
  expect_s3_class(plot(prof), "patchwork")
})

test_that("replicated, unbalanced data have no exact interval", {
  data("reps")
  fit <- bls_reg(y ~ x, data = reps, id = "id")
  prof <- error_ratio_profile(fit, ratios = c(0, 1, Inf))
  expect_false(prof$balanced)
  expect_true(all(is.na(prof$table$slope_exact_lower)))
  expect_equal(nrow(prof$full), 2)
  expect_output(print(prof), "Error-ratio profile")
})

test_that("error_ratio_profile validates its input", {
  data("ba1986")
  expect_error(error_ratio_profile(lm(mini1 ~ wright1, data = ba1986)),
               "simple_eiv")
  pb <- suppressWarnings(pb_reg(mini1 ~ wright1, data = ba1986))
  expect_error(error_ratio_profile(pb),
               "Passing-Bablok")
  expect_error(error_ratio_profile(dem_reg(mini1 ~ wright1, data = ba1986,
                                           weighted = TRUE)),
               "weighted")
  expect_error(error_ratio_profile(dem_reg(mini1 ~ wright1, data = ba1986,
                                           model = FALSE)),
               "model = TRUE")
  fit <- dem_reg(mini1 ~ wright1, data = ba1986)
  expect_error(error_ratio_profile(fit, ratios = -1), "non-negative")
  expect_error(error_ratio_profile(fit, conf.level = 2), "conf.level")
  bls <- bls_reg(mini1 ~ wright1, data = ba1986,
                  var_x = function(l) (0.05 * l)^2, var_y = 100)
  expect_error(error_ratio_profile(bls), "constant")
})
