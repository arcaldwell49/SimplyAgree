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

# nested LoA bounds computed by hand from the complete pairs, as in the vignette
nest_loa_by_hand = function(dat, loa_calc, alpha = 0.05, agree = 0.95){
  dat = dat[!is.na(dat$x) & !is.na(dat$y), ]
  dat$d = dat$x - dat$y
  m = lme4::lmer(d ~ 1 + (1 | id), data = dat, REML = TRUE)
  vc = as.data.frame(lme4::VarCorr(m))
  sw2 = vc$vcov[vc$grp == "Residual"]
  sb2 = vc$vcov[vc$grp == "id"]
  sd2 = sb2 + sw2
  m_i = as.vector(table(dat$id))
  m_i = m_i[m_i > 0]
  n = length(m_i)
  N = nrow(dat)
  mh = n / sum(1 / m_i)
  zA = qnorm(1 - (1 - agree) / 2)
  za = qnorm(1 - alpha)
  if(loa_calc == "mover"){
    u = sd2 + sqrt((sb2 * ((n - 1) / qchisq(alpha, n - 1) - 1))^2 +
                   ((1 - 1 / mh) * sw2 * ((N - n) / qchisq(alpha, N - n) - 1))^2)
    lme = sqrt(za^2 * sb2 / n + zA^2 * (sqrt(u) - sqrt(sd2))^2)
  } else {
    lme = za * sqrt(sb2 / n + zA^2 / (2 * sd2) *
                      (sb2^2 / (n - 1) + (1 - 1 / mh)^2 * sw2^2 / (N - n)))
  }
  bias = unname(lme4::fixef(m))
  c(bias - zA * sqrt(sd2) - lme, bias + zA * sqrt(sd2) + lme)
}

test_that("nested LoA bounds use only the complete pairs", {
  data(reps)
  # reps has 2 rows with a missing measurement
  expect_equal(sum(is.na(reps$x - reps$y)), 2)
  for(lc in c("mover", "blandaltman")){
    for(bt in c("iu", "joint")){
      a = agreement_limit(x = "x", y = "y", id = "id", data = reps,
                          data_type = "nest", loa_calc = lc, bound_type = bt)
      alpha = if(bt == "iu") 0.05 else 0.025
      expect_equal(c(a$loa$lower_loa_ci, a$loa$upper_loa_ci),
                   nest_loa_by_hand(reps, lc, alpha = alpha),
                   tolerance = 1e-6)
    }
  }
})

test_that("rows with a missing measurement do not change the nested LoA", {
  data(reps)
  complete = reps[!is.na(reps$x) & !is.na(reps$y), ]
  padded = rbind(complete,
                 data.frame(id = complete$id[1:3], x = c(NA, 5, NA),
                            y = c(4, NA, NA)))
  a1 = agreement_limit(x = "x", y = "y", id = "id", data = complete,
                       data_type = "nest")
  a2 = agreement_limit(x = "x", y = "y", id = "id", data = padded,
                       data_type = "nest")
  expect_equal(a1$loa, a2$loa)
})

test_that("subjects with a single measurement count in the nested LoA", {
  data(reps)
  complete = reps[!is.na(reps$x) & !is.na(reps$y), ]
  single = rbind(complete, data.frame(id = 99, x = 6.2, y = 5.1))
  a = agreement_limit(x = "x", y = "y", id = "id", data = single,
                      data_type = "nest")
  expect_equal(c(a$loa$lower_loa_ci, a$loa$upper_loa_ci),
               nest_loa_by_hand(single, "mover"),
               tolerance = 1e-6)
})
