test_that("iu is the default and joint uses alpha/2 per side", {
  data(reps)
  for(dt in c("simple", "reps", "nest")){
    for(lc in c("mover", "blandaltman")){
      a_def = agreement_limit(x = "x", y = "y", id = "id", data = reps,
                              data_type = dt, loa_calc = lc)
      a_iu = agreement_limit(x = "x", y = "y", id = "id", data = reps,
                             data_type = dt, loa_calc = lc, bound_type = "iu")
      a_joint = agreement_limit(x = "x", y = "y", id = "id", data = reps,
                                data_type = dt, loa_calc = lc,
                                bound_type = "joint")
      a_half = agreement_limit(x = "x", y = "y", id = "id", data = reps,
                               data_type = dt, loa_calc = lc,
                               bound_type = "iu", alpha = 0.025)

      expect_equal(a_def$loa, a_iu$loa)
      # joint at alpha = 0.05 is iu at alpha = 0.025 for the LoA bounds
      expect_equal(a_joint$loa[, c("lower_loa_ci", "upper_loa_ci")],
                   a_half$loa[, c("lower_loa_ci", "upper_loa_ci")])
      # the bias CI is unaffected
      expect_equal(a_joint$loa[, c("lower.CL", "upper.CL")],
                   a_iu$loa[, c("lower.CL", "upper.CL")])
      expect_true(all(a_joint$loa$lower_loa_ci < a_iu$loa$lower_loa_ci))
      expect_true(all(a_joint$loa$upper_loa_ci > a_iu$loa$upper_loa_ci))
    }
  }
})

test_that("iu bounds are unchanged from the previous one-sided calculation", {
  data(reps)
  a1 = agreement_limit(x = "x", y = "y", data = reps)
  d = reps$x - reps$y
  d = d[!is.na(d)]
  k = length(d)
  s = sd(d)
  zp = qnorm(0.975)
  lme = s * sqrt(qnorm(0.95)^2 / k + zp^2 * (sqrt((k - 1) / qchisq(0.05, k - 1)) - 1)^2)
  expect_equal(a1$loa$lower_loa_ci, mean(d) - zp * s - lme, tolerance = 1e-8)
  expect_equal(a1$loa$upper_loa_ci, mean(d) + zp * s + lme, tolerance = 1e-8)
})

test_that("print and plot label the bound type", {
  data(reps)
  a_iu = agreement_limit(x = "x", y = "y", data = reps)
  a_joint = agreement_limit(x = "x", y = "y", data = reps, bound_type = "joint")
  expect_output(print(a_iu), "not a joint 95% interval")
  expect_output(print(a_joint), "95% joint confidence")
  expect_match(plot(a_iu)$labels$caption, "90% \\(LoA\\)")
  expect_match(plot(a_joint)$labels$caption, "95% \\(LoA\\)")
})
