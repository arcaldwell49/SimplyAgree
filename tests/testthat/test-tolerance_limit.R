context("tolerance_limit")

# Test that the function returns a tolerance_delta class
test_that("tolerance_limit returns a tolerance_delta class", {
  # generic
  expect_s3_class(tolerance_limit(data = mtcars, x = "mpg", y = "disp"), "tolerance_delta")

  # all iterations
  data(temps)
  temps2 = temps
  temps2$x = temps$trec_pre
  temps2$y = temps$teso_pre
  temps2$condition = temps$tod
  temps2$ts = as.numeric(temps$trial_num)
  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                  x = "x",
                                  y = "y",
                                  tol_method = "boot_cal",
                                  replicates = 20),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      keep_model = FALSE),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      cor_type = "ar1"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      cor_type = "car1"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      time = "ts",
                                      cor_type = "ar1"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      time = "ts",
                                      cor_type = "car1"),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      condition = "condition",
                                      correlation = nlme::corAR1(form=~1|id),
                                      weights = nlme::varIdent(form=~condition)),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      condition = "condition",
                                      tol_method = "boot_cal",
                                      replicates = 20),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                      x = "x",
                                      y = "y",
                                      id = "id",
                                      log_tf = TRUE,
                                      prop_bias = TRUE),
                  "tolerance_delta")

  expect_s3_class(tolerance_limit(data = temps2,
                                  x = "x",
                                  y = "y",
                                  id = "id",
                                  log_tf = TRUE,
                                  log_tf_display = "sym"),
                  "tolerance_delta")






})

test_that("check methods",{

  # all iterations
  data(temps)
  temps2 = temps
  temps2$x = temps$trec_pre
  temps2$y = temps$teso_pre
  temps2$condition = temps$tod
  temps2$ts = as.numeric(temps$trial_num)
  test1 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y")

  print(test1)
  check(test1)
  plot(test1,
       delta = 2)
  plot(test1, geom =  "geom_bin2d")
  plot(test1, geom =  "geom_density_2d")
  plot(test1, geom =  "geom_density_2d_filled")
  plot(test1, geom =  "stat_density_2d")

  expect_error(plot(test1, geom =  "geom_bar"))

  test1p = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          prop_bias =TRUE)

  print(test1p)
  check(test1p)
  plot(test1p,
       delta = 2)
  plot(test1p,
       delta = c(-1,2))
  plot(test1p, geom =  "geom_bin2d")
  plot(test1p, geom =  "geom_density_2d")
  plot(test1p, geom =  "geom_density_2d_filled")
  plot(test1p, geom =  "stat_density_2d")



  test2 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id")

  print(test2)
  check(test2)
  plot(test2)


  test3 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          cor_type = "ar1")

  print(test3)
  check(test3)
  plot(test3)

  test4 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          cor_type = "car1")

  print(test4)
  check(test4)
  plot(test4)

  test5 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          condition = "condition",
                          correlation = nlme::corAR1(form=~1|id),
                          weights = nlme::varIdent(form=~condition))

  print(test5)
  check(test5)
  plot(test5)

  test6 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          condition = "condition",
                          tol_method = "boot_cal",
                          replicates = 20)

  print(test6)
  check(test6)
  plot(test6)


  test7 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          log_tf = TRUE,
                          prop_bias = TRUE)

  testthat::expect_identical(class(test7), "tolerance_delta")
  print(test7)
  check(test7)
  plot(test7)

  test7_b = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          log_tf = TRUE,
                          log_tf_display = "sym",
                          prop_bias = TRUE)

  testthat::expect_identical(class(test7_b), "tolerance_delta")
  print(test7_b)
  check(test7_b)
  plot(test7_b)

  test8 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          condition = "condition",
                          log_tf = TRUE,
                          prop_bias = TRUE)

  testthat::expect_identical(class(test8), "tolerance_delta")
  print(test8)
  check(test8)
  plot(test8)

  test9 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                          condition = "condition",
                          log_tf = TRUE,
                          prop_bias = TRUE,
                          tol_method = "boot_cal",
                          replicates = 20,
                          )

  testthat::expect_identical(class(test8), "tolerance_delta")
  print(test9)
  check(test9)
  plot(test9)

  test10 = tolerance_limit(data = temps2,
                          x = "x",
                          y = "y",
                          id = "id",
                         # condition = "condition",
                          log_tf = TRUE,
                          prop_bias = TRUE,
                          tol_method = "boot_cal",
                          replicates = 20,
  )

  testthat::expect_identical(class(test10), "tolerance_delta")
  print(test10)
  check(test10)
  plot(test10)

  x = rnorm(5500)
  z = rnorm(5500,sd=.2)
  y = x+z

  df1 = data.frame(x,y,z)

  test_big = tolerance_limit(data = df1,
                          x = "x",
                          y = "y")

  check(test_big)

})


test_that("limits use the condition-specific residual SD", {
  set.seed(2)
  n = 40
  d1 = data.frame(id = rep(1:n, 2), condition = rep(c("A", "B"), each = n))
  d1$x = rnorm(2 * n, 100, 10)
  d1$y = d1$x + rnorm(2 * n, 0, ifelse(d1$condition == "A", 1, 5))

  t1 = tolerance_limit(d1, x = "x", y = "y",
                       condition = "condition",
                       cor_type = "none")
  lim = t1$limits[order(t1$limits$condition), ]

  # separate means and variances: REML SDs equal the per-condition sample SDs
  emp_sd = tapply(d1$x - d1$y, d1$condition, sd)
  expect_equal(lim$SD, as.vector(emp_sd), tolerance = 1e-4)
  expect_equal(lim$SEP, sqrt(lim$SD^2 + lim$SEM^2), tolerance = 1e-8)

  width = lim$upper.PL - lim$lower.PL
  expect_gt(width[2] / width[1], 3.5)
})

test_that("limits follow a varPower variance function with prop_bias", {
  set.seed(1)
  d2 = data.frame(id = 1:200)
  d2$x = runif(200, 10, 200)
  d2$y = d2$x + rnorm(200, 0, 0.05 * d2$x)

  t2 = tolerance_limit(d2, x = "x", y = "y",
                       prop_bias = TRUE,
                       weights = nlme::varPower(form = ~avg),
                       cor_type = "none")
  lim = t2$limits[order(t2$limits$avg), ]
  pow = coef(t2$model$modelStruct$varStruct)

  expect_equal(lim$SD, sigma(t2$model) * lim$avg^pow, tolerance = 1e-6)
  expect_true(all(diff(lim$SD) > 0))
})

test_that("an avg-dependent variance expands the grid without prop_bias", {
  set.seed(1)
  d2 = data.frame(id = 1:200)
  d2$x = runif(200, 10, 200)
  d2$y = d2$x + rnorm(200, 0, 0.05 * d2$x)
  avg = (d2$x + d2$y) / 2

  t3 = tolerance_limit(d2, x = "x", y = "y",
                       weights = nlme::varPower(form = ~avg),
                       cor_type = "none")
  lim = t3$limits[order(t3$limits$avg), ]

  expect_equal(nrow(lim), 3)
  expect_equal(lim$avg, c(min(avg), median(avg), max(avg)))
  expect_equal(length(unique(lim$bias)), 1)
  expect_true(all(diff(lim$SEP) > 0))

  expect_output(print(t3))
  expect_s3_class(plot(t3), "ggplot")
})

test_that("boot_cal limits use the condition-specific residual SD", {
  set.seed(2)
  n = 40
  d1 = data.frame(id = rep(1:n, 2), condition = rep(c("A", "B"), each = n))
  d1$x = rnorm(2 * n, 100, 10)
  d1$y = d1$x + rnorm(2 * n, 0, ifelse(d1$condition == "A", 1, 5))

  t1 = tolerance_limit(d1, x = "x", y = "y",
                       condition = "condition",
                       cor_type = "none",
                       tol_method = "boot_cal",
                       replicates = 20)
  lim = t1$limits[order(t1$limits$condition), ]

  expect_false(anyNA(lim$lower.TL))
  expect_false(anyNA(lim$upper.TL))
  width = lim$upper.TL - lim$lower.TL
  expect_gt(width[2] / width[1], 3)
})

test_that("returned model can be updated and re-queried", {
  set.seed(2)
  n = 40
  d1 = data.frame(id = rep(1:n, 2), condition = rep(c("A", "B"), each = n))
  d1$x = rnorm(2 * n, 100, 10)
  d1$y = d1$x + rnorm(2 * n, 0, ifelse(d1$condition == "A", 1, 5))

  t1 = tolerance_limit(d1, x = "x", y = "y", id = "id",
                       condition = "condition")

  # evaluate away from the environment tolerance_limit() ran in
  refit_elsewhere = function(m) {
    e = new.env(parent = baseenv())
    e$m = m
    eval(quote(stats::update(m, . ~ .)), e)
  }
  refit = refit_elsewhere(t1$model)
  expect_equal(coef(refit), coef(t1$model))
  expect_equal(logLik(refit), logLik(t1$model))
  expect_equal(nrow(nlme::getData(t1$model)), nrow(d1))
  expect_identical(t1$model$call[[1]], quote(nlme::gls))

  # print should not deparse the whole data frame
  expect_lt(length(capture.output(print(t1$model))), 30)

  # emmeans can find the data without it being passed explicitly
  expect_s4_class(emmeans::emmeans(t1$model, ~ condition), "emmGrid")

  # user-supplied weights and correlation are stored too
  t2 = tolerance_limit(d1, x = "x", y = "y", id = "id",
                       weights = nlme::varPower(form = ~avg),
                       correlation = nlme::corAR1(form = ~1 | id))
  refit2 = refit_elsewhere(t2$model)
  expect_equal(logLik(refit2), logLik(t2$model))
})

test_that("conf_level sets the level of the bias CI", {
  set.seed(2)
  n = 40
  d1 = data.frame(x = rnorm(n, 100, 10))
  d1$y = d1$x + rnorm(n, 0, 1)

  t95 = tolerance_limit(d1, x = "x", y = "y", cor_type = "none")
  t90 = tolerance_limit(d1, x = "x", y = "y", cor_type = "none",
                        conf_level = 0.90)

  expect_equal(t95$limits$upper.CL - t95$limits$lower.CL,
               2 * qt(0.975, t95$limits$df) * t95$limits$SEM,
               tolerance = 1e-8)
  expect_equal(t90$limits$upper.CL - t90$limits$lower.CL,
               2 * qt(0.95, t90$limits$df) * t90$limits$SEM,
               tolerance = 1e-8)

  # prediction and tolerance limits are unaffected
  expect_equal(t90$limits[, c("lower.PL", "upper.PL", "lower.TL", "upper.TL")],
               t95$limits[, c("lower.PL", "upper.PL", "lower.TL", "upper.TL")])
  expect_output(print(t90), "90% CI for Bias")
})

test_that("gls simulation reproduces the fitted marginal covariance", {
  set.seed(4)
  n = 30
  # ids are not contiguous, as in data stacked by condition
  d = data.frame(id = rep(1:n, 2), condition = rep(c("A", "B"), each = n))
  a = rnorm(n, 0, 2)
  d$delta = a[d$id] + rnorm(2 * n, 0, ifelse(d$condition == "A", 1, 3))
  g = nlme::gls(delta ~ condition, d,
                correlation = nlme::corCompSymm(form = ~1 | id),
                weights = nlme::varIdent(form = ~1 | condition))
  V = unclass(nlme::getVarCov(g, individual = "1"))

  st = gls_sim_setup(g, d)
  expect_equal(st$mu, as.vector(fitted(g)))

  sims = replicate(5000, gls_sim_draw(st) - st$mu)
  S = cov(t(sims))
  # rows 1 and 31 are subject 1 under conditions A and B
  expect_equal(c(S[1, 1], S[1, 31], S[31, 31]),
               c(V[1, 1], V[1, 2], V[2, 2]),
               tolerance = 0.1)
  # rows from different subjects are independent
  expect_lt(abs(S[1, 2]), 0.3)
})

test_that("parametric bootstrap does not inflate the SE of the bias", {
  set.seed(3)
  n = 20; k = 19
  df = data.frame(id = rep(1:n, each = k))
  a = rnorm(n, 0, 0.5)
  df$delta = 0.8 + a[df$id] + rnorm(n * k, 0, 1)
  g = nlme::gls(delta ~ 1, data = df,
                correlation = nlme::corCompSymm(form = ~1 | id))

  st = gls_sim_setup(g, df)
  cf = replicate(300, {
    d2 = df
    d2$delta = gls_sim_draw(st)
    coef(update(g, data = d2))
  })
  # simulating from the point estimates: ratio should be ~1, not ~1.41
  ratio = sd(cf) / sqrt(vcov(g)[1])
  expect_gt(ratio, 0.85)
  expect_lt(ratio, 1.15)
})

test_that("boot_cal is reproducible and matches the analytic prediction limits", {
  data(reps)
  set.seed(10)
  t1 = tolerance_limit(reps, x = "x", y = "y", id = "id",
                       tol_method = "boot_cal", replicates = 50)
  set.seed(10)
  t2 = tolerance_limit(reps, x = "x", y = "y", id = "id",
                       tol_method = "boot_cal", replicates = 50)
  ta = tolerance_limit(reps, x = "x", y = "y", id = "id")

  expect_equal(t1$limits, t2$limits)
  expect_equal(t1$limits[, c("bias", "SEM", "SD", "lower.PL", "upper.PL")],
               ta$limits[, c("bias", "SEM", "SD", "lower.PL", "upper.PL")])
  expect_true(all(t1$limits$lower.TL < t1$limits$lower.PL))
  expect_true(all(t1$limits$upper.TL > t1$limits$upper.PL))
})

test_that("analytic joint limits match Howe (1969) for independent data", {
  set.seed(5)
  n = 30
  d = data.frame(x = rnorm(n, 100, 10))
  d$y = d$x + rnorm(n, 0.5, 2)
  t1 = tolerance_limit(d, x = "x", y = "y", tol_method = "analytic")

  dd = d$x - d$y
  nu = n - 1
  k_howe = sqrt(nu * (1 + 1/n) * qnorm(0.975)^2 / qchisq(0.05, nu))
  expect_equal(c(t1$limits$lower.TL, t1$limits$upper.TL),
               mean(dd) + c(-1, 1) * k_howe * sd(dd),
               tolerance = 1e-6)
  expect_equal(t1$limits$SD.df, nu)
  expect_equal(c(t1$limits$lower.TL, t1$limits$upper.TL),
               c(-6.1724, 4.5375), tolerance = 1e-4)
})

test_that("analytic iu bounds use the exact noncentral t for independent data", {
  set.seed(5)
  n = 30
  d = data.frame(x = rnorm(n, 100, 10))
  d$y = d$x + rnorm(n, 0.5, 2)
  t1 = tolerance_limit(d, x = "x", y = "y", bound_type = "iu")

  dd = d$x - d$y
  k1 = qt(0.95, df = n - 1, ncp = qnorm(0.975) * sqrt(n)) / sqrt(n)
  expect_equal(c(t1$limits$lower.TL, t1$limits$upper.TL),
               mean(dd) + c(-1, 1) * k1 * sd(dd),
               tolerance = 1e-6)
  expect_output(print(t1), "not a joint 95% interval")
})

test_that("compound symmetry uses the MOVER bound for the SD", {
  set.seed(11)
  ng = 20; k = 19
  d = data.frame(id = rep(1:ng, each = k))
  d$x = rnorm(ng * k, 100, 10)
  d$y = d$x - 0.8 - rnorm(ng, 0, 0.5)[d$id] - rnorm(ng * k, 0, 1)
  t1 = tolerance_limit(d, x = "x", y = "y", id = "id")

  # balanced one-way ANOVA version of the same bound
  rho = unname(coef(t1$model$modelStruct$corStruct, unconstrained = FALSE))
  s2 = t1$limits$SD^2
  msw = (1 - rho) * s2
  msb = msw + k * rho * s2
  u_s2 = s2 + sqrt((1/k * (msb * (ng - 1) / qchisq(0.05, ng - 1) - msb))^2 +
                   ((1 - 1/k) * (msw * (ng * k - ng) / qchisq(0.05, ng * k - ng) - msw))^2)
  expect_equal(t1$limits$SD.upper, sqrt(u_s2), tolerance = 1e-8)

  # variance components
  expect_equal(t1$limits$SD.between^2 + t1$limits$SD.within^2,
               t1$limits$SD^2, tolerance = 1e-8)
  expect_equal(t1$limits$SD.between^2, rho * s2, tolerance = 1e-8)

  zp = qnorm(0.975)
  expect_equal(t1$limits$upper.TL,
               t1$limits$bias + zp * t1$limits$SEP * sqrt(u_s2) / t1$limits$SD,
               tolerance = 1e-8)
})

test_that("compound symmetry with condition uses per-condition cluster sizes", {
  set.seed(4)
  ng = 12; k = 4
  d = data.frame(id = rep(1:ng, each = 2 * k),
                 condition = rep(rep(c("A", "B"), each = k), ng))
  d$y = 0
  d$x = rnorm(ng, 0, 1)[d$id] +
    rnorm(nrow(d), ifelse(d$condition == "A", 0.5, -0.5),
          ifelse(d$condition == "A", 1, 2))
  t1 = tolerance_limit(d, x = "x", y = "y", id = "id", condition = "condition")
  lim = t1$limits

  rho = unname(coef(t1$model$modelStruct$corStruct, unconstrained = FALSE))
  # each subject has k measurements per condition
  mh = k; df_b = ng - 1; df_w = ng * k - ng
  s2 = lim$SD^2
  msw = (1 - rho) * s2
  msb = msw + mh * rho * s2
  u_s2 = s2 + sqrt((1/mh * (msb * df_b / qchisq(0.05, df_b) - msb))^2 +
                   ((1 - 1/mh) * (msw * df_w / qchisq(0.05, df_w) - msw))^2)
  expect_equal(lim$SD.upper, sqrt(u_s2), tolerance = 1e-8)
})

test_that("effective df is used for other correlation structures", {
  data(temps)
  temps2 = temps
  temps2$x = temps$trec_pre
  temps2$y = temps$teso_pre
  temps2$ts = as.numeric(temps$trial_num)
  t1 = tolerance_limit(temps2, x = "x", y = "y", id = "id",
                       time = "ts", cor_type = "ar1")
  expect_true(all(is.finite(t1$limits$SD.df)))
  expect_true(all(t1$limits$SD.upper > t1$limits$SD))
  expect_true(all(t1$limits$lower.TL < t1$limits$lower.PL))
  # variance components are only defined under compound symmetry
  expect_true(all(is.na(t1$limits$SD.between)))

  # independent data with a variance function: about n - 1 per condition
  set.seed(2)
  n = 40
  d1 = data.frame(id = rep(1:n, 2), condition = rep(c("A", "B"), each = n))
  d1$x = rnorm(2 * n, 100, 10)
  d1$y = d1$x + rnorm(2 * n, 0, ifelse(d1$condition == "A", 1, 5))
  t2 = tolerance_limit(d1, x = "x", y = "y", condition = "condition",
                       cor_type = "none")
  expect_equal(t2$limits$SD.df, c(n - 1, n - 1), tolerance = 0.5)
})

test_that("boot_cal limits agree with analytic limits where both are accurate", {
  set.seed(6)
  gd = data.frame(id = rep(1:20, length.out = 366))
  gd$x = rnorm(366, 100, 10)
  gd$y = gd$x - 0.8 - rnorm(20, 0, 0.5)[gd$id] + rnorm(366, 0, 1)

  for(bt in c("joint", "iu")){
    ta = tolerance_limit(gd, x = "x", y = "y", id = "id", bound_type = bt)
    set.seed(1)
    expect_silent(
      tp <- tolerance_limit(gd, x = "x", y = "y", id = "id", bound_type = bt,
                            tol_method = "boot_cal", replicates = 199)
    )
    expect_equal(c(tp$limits$lower.TL, tp$limits$upper.TL),
                 c(ta$limits$lower.TL, ta$limits$upper.TL),
                 tolerance = 0.05)
    expect_true(all(tp$limits$lower.TL.level > 0.5 & tp$limits$lower.TL.level < 1))
  }

  # independent data: the analytic limits are exact, so calibration keeps
  # the level close to tol_level
  set.seed(5)
  n = 30
  d = data.frame(x = rnorm(n, 100, 10))
  d$y = d$x + rnorm(n, 0.5, 2)
  for(bt in c("joint", "iu")){
    ta = tolerance_limit(d, x = "x", y = "y", bound_type = bt)
    set.seed(2)
    tp = tolerance_limit(d, x = "x", y = "y", bound_type = bt,
                         tol_method = "boot_cal", replicates = 999)
    expect_equal(c(tp$limits$lower.TL.level, tp$limits$upper.TL.level),
                 c(0.95, 0.95), tolerance = 0.02)
    expect_equal(c(tp$limits$lower.TL, tp$limits$upper.TL),
                 c(ta$limits$lower.TL, ta$limits$upper.TL),
                 tolerance = 0.03)
  }
})

test_that("calibration reproduces the analytic limits at the calibrated level", {
  set.seed(2)
  n = 40
  d1 = data.frame(id = rep(1:n, 2), condition = rep(c("A", "B"), each = n))
  d1$x = rnorm(2 * n, 100, 10)
  d1$y = d1$x + rnorm(2 * n, 0, ifelse(d1$condition == "A", 1, 5))
  set.seed(3)
  tp = tolerance_limit(d1, x = "x", y = "y", id = "id", condition = "condition",
                       tol_method = "boot_cal", replicates = 50)
  lim = tp$limits
  for(j in seq_len(nrow(lim))){
    ta = tolerance_limit(d1, x = "x", y = "y", id = "id", condition = "condition",
                         tol_level = lim$lower.TL.level[j])
    expect_equal(ta$limits$lower.TL[j], lim$lower.TL[j], tolerance = 1e-8)
  }
})

test_that("calibrate_level finds the smallest level reaching the target", {
  cov_fun = function(level) pnorm(100 * (level - 0.9))
  lev = calibrate_level(cov_fun, 0.95)
  expect_equal(lev, 0.9 + qnorm(0.95) / 100, tolerance = 1e-6)
  expect_equal(calibrate_level(function(level) 1, 0.95), 0.5)
  expect_warning(calibrate_level(function(level) 0, 0.95), "did not reach")
})

test_that("old tol_method names are deprecated aliases", {
  data(reps)
  old_opts = options(lifecycle_verbosity = "warning")
  on.exit(options(old_opts), add = TRUE)
  expect_warning(
    t_old <- tolerance_limit(reps, x = "x", y = "y", tol_method = "approx"),
    "deprecated"
  )
  t_new = tolerance_limit(reps, x = "x", y = "y", tol_method = "analytic")
  expect_equal(t_old$limits, t_new$limits)
  expect_identical(t_old$call$tol_method, "analytic")

  expect_warning(
    t_p <- tolerance_limit(reps, x = "x", y = "y", tol_method = "p",
                           replicates = 20),
    "boot_cal"
  )
  expect_identical(t_p$call$tol_method, "boot_cal")
  # "a" partially matches the new name
  expect_identical(tolerance_limit(reps, x = "x", y = "y",
                                   tol_method = "a")$call$tol_method,
                   "analytic")
})

test_that("Checked against BivRegBLS", {
  data(reps)
  # test2 = BivRegBLS::MD.horiz.lines(data = reps, xcol = "y", ycol = "x", pred.level = .95, .95)

  test1 = tolerance_limit(x = "x",
                          y = "y",
                          data = reps)

  expect_equal(test1$limits$bias, .4383,
               tolerance = .001)

  expect_equivalent(c(test1$limits$lower.PL,
                      test1$limits$upper.PL),
                    c(-2.199752,3.076419),
               tolerance = .001)

  expect_equivalent(test1$limits$bias, .4383,
               tolerance = .001)
} )
