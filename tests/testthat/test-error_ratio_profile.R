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
  pb <- error_ratio_profile(bls, ratios = 2, joint_method = "wald")
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

test_that("the Sadler joint test and regions match joint_test() and plot_joint()", {
  data("ba1986")
  fit <- dem_reg(mini1 ~ wright1, data = ba1986, error.ratio = 2)
  prof <- error_ratio_profile(fit, ratios = c(0.5, 2))
  expect_equal(prof$joint_method, "sadler")
  jt <- joint_test(fit)
  expect_equal(prof$table$joint_p[2], unname(jt$p.value), tolerance = 1e-10)
  expect_equal(prof$table$joint_F[2], unname(jt$statistic), tolerance = 1e-10)
  # the region at that ratio is plot_joint()'s
  f <- .erp_fit(prof$data, 2)
  rg <- .erp_region(prof$data, f, "sadler", 0.95)
  inp <- .joint_inputs(fit, 0.95, "sadler", "F")
  rg0 <- .sadler_region(inp$sadler, .joint_run(inp, "sadler", 0, 1)$s_hat,
                        0.95)
  expect_equal(range(rg$slope), range(rg0$slope), tolerance = 1e-8)
  expect_equal(range(rg$intercept), range(rg0$intercept), tolerance = 1e-6)
  # the Wald option keeps the ellipse test
  pw <- error_ratio_profile(fit, ratios = 2, joint_method = "wald")
  d <- f$coef - c(0, 1)
  expect_equal(pw$table$joint_F, as.numeric(t(d) %*% solve(f$vcov, d)) / 2,
               tolerance = 1e-10)
  expect_s3_class(.erp_region(prof$data, f, "wald", 0.95), "data.frame")
  expect_error(error_ratio_profile(fit, joint_method = "x"))
})

test_that("the Bland-Altman (CBLS) profile", {
  data("ba1986")
  fit <- dem_reg(mini1 ~ wright1, data = ba1986)
  prof <- error_ratio_profile(fit, ratios = c(0, 0.5, 3, Inf))
  bls <- bls_reg(mini1 ~ wright1, data = ba1986, error.ratio = 3,
                 se_method = "bivregbls")
  r3 <- prof$table[prof$table$ratio == 3, ]
  expect_equal(c(r3$md_intercept, r3$md_slope),
               unname(bls$cbls$coefficients), tolerance = 1e-8)
  q <- qt(0.975, prof$n - 2)
  expect_equal(c(r3$md_intercept_upper - r3$md_intercept,
                 r3$md_slope_upper - r3$md_slope),
               unname(q * sqrt(diag(bls$cbls$vcov))), tolerance = 1e-8)
  # the CBLS line is the BLS line in the Bland-Altman plot
  b <- prof$table$slope
  expect_equal(prof$table$md_slope, 2 * (1 - b) / (1 + b), tolerance = 1e-6)
  expect_equal(prof$full_md$lower[1], min(prof$table$md_slope_lower))
  # the Sadler criterion is the same for the two lines, so so is the test
  f <- .erp_fit(prof$data, 3)
  m <- (prof$data$x + prof$data$y) / 2
  d <- prof$data$x - prof$data$y
  S_md <- function(g0, g1) {
    sum((d - g0 - g1 * m)^2 /
          (f$vx * (1 - g1 / 2)^2 + f$vy * (1 + g1 / 2)^2))
  }
  s_hat <- S_md(f$cbls_coef[1], f$cbls_coef[2])
  F_md <- ((S_md(0, 0) - s_hat) / 2) / (s_hat / (prof$n - 2))
  expect_equal(F_md, r3$joint_F, tolerance = 1e-6)
  expect_output(print(prof, type = "md"), "Bland-Altman plot, CBLS")
  expect_output(print(prof, type = "md"), "same test")
  expect_s3_class(plot(prof, type = "md"), "patchwork")
})

test_that("plot(type = 'joint') draws the regions by ratio", {
  data("ba1986")
  prof <- error_ratio_profile(dem_reg(mini1 ~ wright1, data = ba1986))
  p <- plot(prof, type = "joint")
  expect_s3_class(p, "ggplot")
  reg <- ggplot2::layer_data(p, 2)
  expect_equal(length(unique(reg$group)), 7) # 0, 0.01, ..., Inf (1 fitted)
  p2 <- plot(prof, type = "joint", ratios = c(1, 4), n_points = 20)
  expect_equal(length(unique(ggplot2::layer_data(p2, 2)$group)), 2)
  expect_error(plot(prof, type = "joint", ratios = -1), "non-negative")
  # a slope the data cannot bound: the region is not drawn
  set.seed(3)
  noise <- data.frame(x = rnorm(6), y = rnorm(6))
  pn <- error_ratio_profile(dem_reg(y ~ x, data = noise), ratios = 1)
  f <- .erp_fit(pn$data, 1)
  expect_null(.erp_region(pn$data, f, "sadler", 0.95))
  expect_error(plot(pn, type = "joint", ratios = 1), "unbounded")
  pn2 <- suppressWarnings(plot(pn, type = "joint", ratios = c(0, 1)))
  expect_match(pn2$labels$caption, "not drawn: ratio 1")
})

test_that("the Bland-Altman profile matches BivRegBLS FullCIs.MD (sign of D)", {
  skip_if_not_installed("BivRegBLS")
  data("ba1986")
  d <- data.frame(x = ba1986$wright1, y = ba1986$mini1)
  ref <- suppressWarnings(BivRegBLS::FullCIs.MD(data = d, xcol = 1, ycol = 2))
  lam <- ref$Slopes[, 1]
  keep <- lam > 0 & is.finite(lam)
  prof <- error_ratio_profile(dem_reg(mini1 ~ wright1, data = ba1986),
                              ratios = 1 / lam[keep])
  m <- match(signif(1 / lam[keep], 10), signif(prof$table$ratio, 10))
  tab <- prof$table[m, ]
  # BivRegBLS: D = y - x; here D = x - y, as bls_reg()
  expect_equal(tab$md_slope, -ref$Slopes[keep, 3], tolerance = 1e-6)
  expect_equal(tab$md_slope_lower, -ref$Slopes[keep, 5], tolerance = 1e-6)
  expect_equal(tab$md_slope_upper, -ref$Slopes[keep, 4], tolerance = 1e-6)
  expect_equal(tab$md_intercept, -ref$Intercepts[keep, 3], tolerance = 1e-6)
  expect_equal(tab$md_intercept_lower, -ref$Intercepts[keep, 5],
               tolerance = 1e-6)
})
