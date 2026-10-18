# Gwet (2014) example: 12 subjects, 4 raters, 5 categories, missing ratings.
# Reference values from irrCAC 1.0 (pa.coeff.raw, gwet.ac1.raw,
# fleiss.kappa.raw, krippen.alpha.raw), which rounds to 5 decimals.
gwet_ratings <- data.frame(
  R1 = c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
  R2 = c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, NA),
  R3 = c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, 3),
  R4 = c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA))
gwet_cols <- c("R1", "R2", "R3", "R4")

# rows: pa, gwet, fleiss, krippendorff; columns: estimate, se
irrcac_ref <- list(
  unweighted = c(0.8181818, 0.77544, 0.76117, 0.74342,
                 0.12561, 0.14295, 0.15302, 0.14548),
  quadratic = c(0.9753788, 0.91400, 0.86494, 0.84911,
                0.09062, 0.10396, 0.14603, 0.12905),
  linear = c(0.9393939, 0.85874, 0.81794, 0.80038,
             0.09368, 0.11733, 0.14850, 0.13538),
  ordinal = c(0.9681818, 0.89894, 0.85021, 0.83364,
              0.09086, 0.10690, 0.14704, 0.13099),
  radical = c(0.8972691, 0.81981, 0.78992, 0.77198,
              0.10155, 0.12836, 0.15004, 0.13951),
  ratio = c(0.9541149, 0.85737, 0.82134, 0.79740,
            0.09211, 0.12207, 0.15239, 0.14036),
  circular = c(0.9024592, 0.83020, 0.80720, 0.78998,
               0.10368, 0.13265, 0.14894, 0.14094),
  bipolar = c(0.9683622, 0.90037, 0.85307, 0.83499,
              0.09077, 0.10582, 0.14463, 0.12804))

test_that("all coefficients and weights match irrCAC", {
  coefs <- c("pa", "gwet", "fleiss", "krippendorff")
  for (w in names(irrcac_ref)) {
    ref <- matrix(irrcac_ref[[w]], ncol = 2)
    for (i in seq_along(coefs)) {
      res <- cac_test(col.names = gwet_cols, data = gwet_ratings,
                      coef = coefs[i], weights = w)
      expect_equal(unname(res$estimate), ref[i, 1], tolerance = 1e-4,
                   label = paste(w, coefs[i], "estimate"))
      expect_equal(res$stderr, ref[i, 2], tolerance = 1e-4,
                   label = paste(w, coefs[i], "se"))
    }
  }
})

test_that("unused categories change the weights and Gwet's chance agreement", {
  # irrCAC with categ.labels = 1:6
  res_q <- cac_test(col.names = gwet_cols, data = gwet_ratings,
                    weights = "quadratic", categories = 1:6)
  expect_equal(unname(res_q$estimate), 0.94741, tolerance = 1e-4)
  expect_equal(res_q$stderr, 0.09367, tolerance = 1e-4)
  res_u <- cac_test(col.names = gwet_cols, data = gwet_ratings,
                    categories = 1:6)
  expect_equal(unname(res_u$estimate), 0.78553, tolerance = 1e-4)
  # Fleiss' kappa does not depend on unused categories
  res_f <- cac_test(col.names = gwet_cols, data = gwet_ratings,
                    coef = "fleiss", weights = "quadratic", categories = 1:6)
  expect_equal(unname(res_f$estimate), 0.86494, tolerance = 1e-4)
  expect_error(cac_test(col.names = gwet_cols, data = gwet_ratings,
                        categories = 1:4),
               "not listed in 'categories': 5")
})

test_that("cac_test returns an htest with a working test", {
  res <- cac_test(col.names = gwet_cols, data = gwet_ratings)
  expect_s3_class(res, c("cac_htest", "htest"))
  expect_null(res$p.value)
  expect_equal(names(res$estimate), "Gwet's AC1")
  expect_equal(attr(res$conf.int, "conf.level"), 0.95)
  expect_equal(res$conf.int[2], 1)

  tst <- cac_test(col.names = gwet_cols, data = gwet_ratings,
                  null.value = 0.5, alternative = "greater")
  t_stat <- (unname(tst$estimate) - 0.5) / tst$stderr
  expect_equal(unname(tst$statistic), t_stat)
  expect_equal(unname(tst$parameter), 11)
  expect_equal(tst$p.value, stats::pt(t_stat, 11, lower.tail = FALSE))
  expect_equal(tst$conf.int[1],
               unname(tst$estimate) - stats::qt(0.95, 11) * tst$stderr)
  tst2 <- cac_test(col.names = gwet_cols, data = gwet_ratings,
                   null.value = 0.5)
  expect_equal(tst2$p.value, 2 * tst$p.value)
  expect_output(print(tst), "Gwet's AC1 for 4 raters")
})

test_that("formula, long and wide interfaces agree", {
  long <- data.frame(id = rep(seq_len(12), times = 4),
                     rater = rep(gwet_cols, each = 12),
                     score = unlist(gwet_ratings, use.names = FALSE))
  wide <- cac_test(col.names = gwet_cols, data = gwet_ratings,
                   coef = "krippendorff")
  frm <- cac_test(score ~ rater | id, data = long, coef = "krippendorff")
  lng <- cac_test(data = long, measure = "score", item = "rater", id = "id",
                  coef = "krippendorff")
  expect_equal(frm$estimate, wide$estimate)
  expect_equal(lng$stderr, wide$stderr)
})

test_that("factor and character ratings work", {
  lab <- c("none", "mild", "moderate", "severe", "extreme")
  fac <- as.data.frame(lapply(gwet_ratings, function(x)
    factor(lab[x], levels = lab)))
  res <- cac_test(col.names = gwet_cols, data = fac, weights = "quadratic")
  expect_equal(unname(res$estimate), 0.91400, tolerance = 1e-4)
  # factor levels define the scale, including unused ones
  fac6 <- as.data.frame(lapply(gwet_ratings, function(x)
    factor(lab[x], levels = c(lab, "fatal"))))
  res6 <- cac_test(col.names = gwet_cols, data = fac6, weights = "quadratic")
  expect_equal(unname(res6$estimate), 0.94741, tolerance = 1e-4)
  chr <- as.data.frame(lapply(gwet_ratings, function(x) lab[x]))
  res_chr <- cac_test(col.names = gwet_cols, data = chr)
  expect_equal(unname(res_chr$estimate), 0.77544, tolerance = 1e-4)
})

test_that("custom weight matrices are used and validated", {
  w <- SimplyAgree:::.cac_weights("linear", 1:5)
  res <- cac_test(col.names = gwet_cols, data = gwet_ratings, weights = w)
  expect_equal(unname(res$estimate), 0.85874, tolerance = 1e-4)
  expect_error(cac_test(col.names = gwet_cols, data = gwet_ratings,
                        weights = diag(3)),
               "custom weight matrix must be 5 x 5")
})

test_that("cac_test validates its input", {
  expect_error(cac_test(col.names = gwet_cols, data = gwet_ratings,
                        null.value = 1), "'null.value'")
  expect_error(cac_test(col.names = gwet_cols, data = gwet_ratings,
                        weights = "cubic"))
  same <- data.frame(a = rep(1, 5), b = rep(1, 5))
  expect_error(cac_test(col.names = c("a", "b"), data = same),
               "one category")
  expect_equal(unname(cac_test(col.names = c("a", "b"), data = same,
                               coef = "pa")$estimate), 1)
  dup <- data.frame(id = c(1, 1, 1, 2, 2), r = c("a", "a", "b", "a", "b"),
                    y = c(1, 2, 1, 2, 2))
  expect_error(cac_test(y ~ r | id, data = dup), "at most one measurement")
  one <- data.frame(a = c(1, NA, 2), b = c(NA, 1, NA))
  expect_error(cac_test(col.names = c("a", "b"), data = one),
               "2 or more ratings")
})

test_that("summary and plot methods work", {
  res <- cac_test(col.names = gwet_cols, data = gwet_ratings, coef = "fleiss",
                  null.value = 0.4, alternative = "greater")
  s <- summary(res)
  expect_s3_class(s, "summary.cac_htest")
  expect_equal(s$style, "hypothesis")
  expect_match(s$text, "Fleiss' kappa")
  expect_match(s$text, "11 degrees of freedom")
  expect_output(print(s), "statistically significant")
  expect_equal(summary(res, style = "estimation")$style, "estimation")
  expect_message(summary(cac_test(col.names = gwet_cols, data = gwet_ratings),
                         style = "hypothesis"), "estimation style")
  expect_s3_class(plot(res), "ggplot")
  expect_s3_class(plot(res, type = "subjects"), "ggplot")
  expect_error(plot(res, type = "effects"), "only available")
})
