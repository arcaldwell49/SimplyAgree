context("quadrant and polar plots of changes")

test_that("successive changes within id, in order", {
  df = data.frame(id = c(1, 1, 1, 2, 2, 3), ord = c(2, 1, 3, 1, 2, 1),
                  x = c(12, 10, 15, 20, 18, 5), y = c(11, 10, 16, 20, 19, 5))
  ch = trend_changes(df)
  expect_equal(ch$id, c(1, 1, 2))
  expect_equal(ch$dx, c(2, 3, -2))
  expect_equal(ch$dy, c(1, 5, -1))
  expect_equal(ch$mean, c(1.5, 4, -1.5))
  cr = trend_changes(df, relative = TRUE)
  expect_equal(cr$dx, 100 * c(2 / 10, 3 / 12, -2 / 20))
  # log scale: percentage change from the log change
  cl = trend_changes(transform(df, x = log(x), y = log(y)), relative = TRUE,
                     log = TRUE)
  expect_equal(cl$dx, cr$dx)
  expect_error(trend_changes(df[df$id == 3, ]), "no changes to plot")
})

test_that("concordance, exclusion, angles and expected concordance", {
  ch = data.frame(dx = c(1, 2, -1, 0.1, 1, -1),
                  dy = c(1, 1, -2, -0.1, -1, 0))
  ch$mean = (ch$dx + ch$dy) / 2
  cc = trend_concordance(ch, exclusion = 0)
  # (0.1, -0.1) and (1, -1) have mean 0 but are still included at 0
  expect_equal(cc$n, 6)
  expect_equal(cc$rate, 3 / 6)
  cc = trend_concordance(ch, exclusion = 0.2)
  expect_equal(cc$n_excl, 2)
  expect_equal(cc$rate, 3 / 4)

  pa = trend_polar_angles(ch)
  expect_equal(pa$r, abs(ch$mean))
  # on the identity line: 0; x changed more: negative
  expect_equal(pa$theta[1], 0)
  expect_equal(pa$theta[2], atan2(1, 2) * 180 / pi - 45)
  # a fall is turned through 180 degrees: (-1, -2) is (1, 2)
  expect_equal(pa$theta[3], atan2(2, 1) * 180 / pi - 45)
  expect_equal(pa$theta[6], -45)
  expect_true(all(abs(pa$theta) <= 90 + 1e-8 | ch$mean == 0))

  # Enevoldsen et al.: change LoA 1.5 and a change of 1 give 0.904
  sd_w = 1.5 / (qnorm(0.975) * sqrt(2))
  ex = trend_expected(data.frame(mean = 1), TRUE, sd_w)
  expect_equal(ex, 0.904, tolerance = 1e-3)

  expect_error(trend_check_args(-1, FALSE, FALSE), "exclusion")
  expect_error(trend_check_args(0, NA, FALSE), "relative")
  expect_error(trend_check_args(0, TRUE, TRUE), "absolute changes")
})

test_that("plots for method_vc, agreement_limit and tolerance_limit", {
  dat = sim_methods(n_items = 15, n_reps = 4, methods = c("A", "B"),
                    alpha = c(0, 0.5), tau = 0.3, omega = 0.6,
                    sigma = c(0.3, 0.4), replicates = "linked", seed = 2)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked")
  pq = plot(fit, type = "quadrant", exclusion = 0.2, expected = TRUE)
  expect_s3_class(pq, "ggplot")
  # caption: concordance from the changes of the pair, in replicate order
  pp = mc_pair(fit$data, "A", "B", replicates = "linked")
  ch = trend_changes(data.frame(id = pp$id, ord = xtfrm(pp$repl),
                                x = pp$y1, y = pp$y2))
  cc = trend_concordance(ch, 0.2)
  expect_match(pq$labels$caption, trend_pct(cc$rate), fixed = TRUE)
  sd_w = sqrt(sum(fit$varcomp$var[fit$varcomp$component == "sigma"]))
  expect_match(pq$labels$caption,
               trend_pct(trend_expected(ch, cc$keep, sd_w)), fixed = TRUE)
  pp2 = plot(fit, type = "polar", pair = c("B", "A"))
  expect_s3_class(pp2, "ggplot")
  expect_match(pp2$labels$caption, "A changed more than B")

  fe = suppressWarnings(method_vc(dat, y = "y", method = "method",
                                  id = "id", replicate = "repl",
                                  replicates = "exchangeable"))
  expect_error(plot(fe, type = "quadrant"), "needs linked replicates")

  # agreement_limit(nest) and tolerance_limit(id) on the same data, wide
  wide = stats::reshape(as.data.frame(dat[, c("id", "repl", "method", "y")]),
                        idvar = c("id", "repl"), timevar = "method",
                        direction = "wide")
  wide = wide[order(wide$id, wide$repl), ]
  a = agreement_limit(x = "y.A", y = "y.B", id = "id", data = wide,
                      data_type = "nest")
  pa = plot(a, type = "quadrant", exclusion = 0.2)
  expect_s3_class(pa, "ggplot")
  # same changes as method_vc's pair
  expect_match(pa$labels$caption, trend_pct(cc$rate), fixed = TRUE)
  expect_s3_class(plot(a, type = "polar", relative = TRUE), "ggplot")
  expect_s3_class(plot(a), "ggplot")
  ar = agreement_limit(x = "y.A", y = "y.B", id = "id", data = wide,
                       data_type = "reps")
  expect_error(plot(ar, type = "quadrant"), "needs data_type")

  tl = tolerance_limit(x = "y.A", y = "y.B", id = "id", time = "repl",
                       data = wide, model = "lme")
  pt = plot(tl, type = "quadrant", exclusion = 0.2, expected = TRUE)
  expect_match(pt$labels$caption, trend_pct(cc$rate), fixed = TRUE)
  expect_s3_class(plot(tl, type = "polar"), "ggplot")
  t0 = tolerance_limit(x = "y.A", y = "y.B", data = wide)
  expect_error(plot(t0, type = "polar"), "needs repeated measurements")
})
