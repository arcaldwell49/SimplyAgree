test_that("kappa_m matches Nelson, Mitani & Edwards (2017)", {
  # Table 1: C = 5, (sigma2_u, sigma2_v) = (1, 5) and (5, 1)
  expect_equal(SimplyAgree:::.kappa_m(1 / 7, 5), 0.035, tolerance = 0.5e-3 / 0.035)
  expect_equal(SimplyAgree:::.kappa_m(5 / 7, 5), 0.264, tolerance = 0.5e-3 / 0.264)
  # Beam mammography study, Table 2: model (i) (Figure 1: about 0.24) and
  # model (iii) for younger patients (0.333)
  expect_equal(SimplyAgree:::.kappa_m(2.442 / 3.600, 5), 0.2405, tolerance = 1e-3)
  expect_equal(SimplyAgree:::.kappa_m(4.615 / 5.773, 5), 0.333, tolerance = 0.5e-3 / 0.333)
  # limits, and the inverse
  expect_equal(SimplyAgree:::.kappa_m(0, 4), 0)
  expect_equal(SimplyAgree:::.kappa_m(1, 4), 1)
  expect_equal(SimplyAgree:::.kappa_m_inverse(SimplyAgree:::.kappa_m(0.4, 3), 3),
               0.4, tolerance = 1e-6)
})

test_that("observed agreement at the minimising thresholds gives kappa_m", {
  # with thresholds at qnorm(c/C) * sigma_T, p0 enters kappa_m directly
  s2u <- 2; s2v <- 0.5; C <- 4
  th <- stats::qnorm(seq_len(C - 1) / C) * sqrt(s2u + s2v + 1)
  p0 <- SimplyAgree:::.ordk_p0(th, s2u, s2v)
  expect_equal((p0 - 1 / C) / (1 - 1 / C),
               SimplyAgree:::.kappa_m(s2u / (s2u + s2v + 1), C))
})

ordk_data <- function(seed = 42, n = 40, k = 6) {
  set.seed(seed)
  d <- expand.grid(id = seq_len(n), rater = paste0("R", seq_len(k)))
  latent <- stats::rnorm(n, sd = 1.5)[d$id] +
    stats::rnorm(k, sd = 0.5)[d$rater] + stats::rnorm(nrow(d))
  d$rating <- cut(latent, c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), labels = FALSE)
  d
}

test_that("ord_kappa_test fits the model and returns an htest", {
  skip_if_not_installed("ordinal")
  d <- ordk_data()
  res <- ord_kappa_test(rating ~ rater | id, data = d)
  expect_s3_class(res, c("cac_htest", "htest"))
  fit <- res$fit
  expect_equal(fit$C, 5)
  expect_equal(fit$rho,
               fit$sigma2_subject / (fit$sigma2_subject + fit$sigma2_rater + 1))
  expect_equal(unname(res$estimate), SimplyAgree:::.kappa_m(fit$rho, 5))
  # the interval contains the estimate and stays within [0, 1]
  expect_true(res$conf.int[1] < res$estimate && res$estimate < res$conf.int[2])
  expect_true(res$conf.int[1] > 0 && res$conf.int[2] < 1)
  # variance components agree with a direct clmm fit
  df <- data.frame(values = factor(d$rating, ordered = TRUE),
                   items = factor(d$rater), id = factor(d$id))
  m <- ordinal::clmm(values ~ 1 + (1 | id) + (1 | items), data = df,
                     link = "probit")
  expect_equal(fit$sigma2_subject, unname(m$ST$id[1, 1])^2, tolerance = 1e-4)
  expect_equal(fit$sigma2_rater, unname(m$ST$items[1, 1])^2, tolerance = 1e-4)
  expect_equal(nrow(fit$rater_effects), 6)

  # the test inverts the interval
  at_lower <- ord_kappa_test(rating ~ rater | id, data = d,
                             null.value = res$conf.int[1])
  expect_equal(at_lower$p.value, 0.05, tolerance = 1e-4)
  gr <- ord_kappa_test(rating ~ rater | id, data = d, null.value = 0.1,
                       alternative = "greater")
  expect_equal(gr$conf.int[2], 1)
  expect_lt(gr$p.value, 0.05)
  expect_output(print(gr), "true kappa_m is greater than 0.1")
})

test_that("ord_kappa_test methods work", {
  skip_if_not_installed("ordinal")
  skip_on_cran()
  res <- ord_kappa_test(rating ~ rater | id, data = ordk_data())
  s <- summary(res)
  expect_equal(s$style, "estimation")
  expect_match(s$text, "Nelson & Edwards")
  expect_s3_class(plot(res), "ggplot")
  expect_s3_class(plot(res, type = "subjects"), "ggplot")
  expect_s3_class(plot(res, type = "effects"), "ggplot")
})

test_that("ord_kappa_test validates its input", {
  skip_if_not_installed("ordinal")
  d <- ordk_data()
  expect_error(ord_kappa_test(rating ~ rater | id, data = d, null.value = 1),
               "'null.value'")
  d$chr <- c("a", "b", "c", "d", "e")[d$rating]
  expect_error(ord_kappa_test(chr ~ rater | id, data = d), "order")
  expect_error(ord_kappa_test(rating ~ rater | id, data = d,
                              categories = 1:4), "not listed")
})

test_that("subgroup kappas match Nelson, Mitani & Edwards (2017) Table 1", {
  km <- SimplyAgree:::.kappa_m
  kb <- SimplyAgree:::.kappa_m_between
  # (a): sigma2_u0 = 1, sigma2_v0 = 5, extra variance 0.5 for x = 1
  expect_equal(round(km(1 / 7.5, 5), 3), 0.032)            # x_j = 1
  expect_equal(round(kb(5, 1, 5, 5.5), 3), 0.033)          # x_j = 0 vs 1
  expect_equal(round(km(1.5 / 7.5, 5), 3), 0.050)          # x_i = 1
  expect_equal(round(km(1.5 / 8, 5), 3), 0.046)            # x_i = 1, x_j = 1
  # (b): sigma2_u0 = 5, sigma2_v0 = 1
  expect_equal(round(km(5 / 7.5, 5), 3), 0.233)
  expect_equal(round(kb(5, 5, 1, 1.5), 3), 0.248)
  expect_equal(round(km(5.5 / 7.5, 5), 3), 0.277)
  expect_equal(round(km(5.5 / 8, 5), 3), 0.246)
  # between two raters from the same group it reduces to kappa_m
  expect_equal(kb(4, 2, 0.5, 0.5), km(2 / 3.5, 4), tolerance = 1e-6)
  # a shift between the groups lowers agreement
  expect_lt(kb(4, 2, 0.5, 0.5, 0, 1), kb(4, 2, 0.5, 0.5))
})

test_that("random-effect covariances are rebuilt from the clmm parameters", {
  ST <- list(id = matrix(1), items = matrix(0, 2, 2))
  S <- SimplyAgree:::.ordk_cov_from_par(c(2, 0.8, 0.84, -0.12), ST)
  expect_equal(S$id[1, 1], 4)
  L <- matrix(c(0.8, -0.12, 0, 0.84), 2)
  expect_equal(S$items, L %*% t(L))
})

ordk_group_data <- function(seed = 3, n = 60, k = 16) {
  set.seed(seed)
  d <- expand.grid(id = seq_len(n), rater = paste0("R", seq_len(k)))
  d$exp <- ifelse(as.integer(d$rater) <= k / 2, "exp", "inexp")
  d$age <- c("young", "old")[stats::rbinom(n, 1, 0.5)[d$id] + 1]
  x <- d$exp == "inexp"
  latent <- stats::rnorm(n, sd = 2)[d$id] +
    stats::rnorm(k, sd = 0.4)[d$rater] +
    x * stats::rnorm(k, sd = 1)[d$rater] + stats::rnorm(nrow(d))
  d$rating <- cut(latent, c(-Inf, -2, -0.5, 0.5, 2, Inf), labels = FALSE)
  d
}

test_that("ord_kappa_test compares rater groups", {
  skip_if_not_installed("ordinal")
  skip_on_cran()
  d <- ordk_group_data()
  res <- ord_kappa_test(rating ~ rater | id, data = d, group = "exp")
  expect_s3_class(res, c("cac_htest", "htest"))
  fit <- res$fit
  expect_equal(names(res$estimate), c("kappa_m (exp)", "kappa_m (inexp)"))
  s2u <- fit$sigma2_subject
  s2v <- fit$sigma2_rater
  expect_equal(unname(res$estimate),
               c(SimplyAgree:::.kappa_m(s2u / (s2u + s2v[1] + 1), 5),
                 SimplyAgree:::.kappa_m(s2u / (s2u + s2v[2] + 1), 5)))
  expect_equal(fit$subgroups$kappa_m[3],
               SimplyAgree:::.kappa_m_between(5, s2u, s2v[1], s2v[2], 0,
                                              fit$beta[["estimate"]]))
  # LRT against the model with a common rater variance
  ll <- as.numeric(stats::logLik(fit$model)) -
    as.numeric(stats::logLik(fit$null_model))
  expect_equal(unname(res$statistic), 2 * ll)
  expect_equal(res$p.value, stats::pchisq(2 * ll, 1, lower.tail = FALSE))
  expect_lt(res$p.value, 0.05)
  # same likelihood as the paper's random-slope model (1 + x | rater)
  df <- data.frame(values = factor(d$rating, ordered = TRUE),
                   items = factor(d$rater), id = factor(d$id),
                   x = as.integer(d$exp == "inexp"))
  slope <- ordinal::clmm(values ~ x + (1 | id) + (1 + x | items), data = df,
                         link = "probit")
  expect_equal(as.numeric(stats::logLik(fit$model)),
               as.numeric(stats::logLik(slope)), tolerance = 1e-4)

  s <- summary(res)
  expect_match(s$text, "likelihood ratio test is statistically significant")
  expect_match(s$text, "Between a rater from each group")
  expect_match(summary(res, style = "estimation")$text, "S-value")
  expect_s3_class(plot(res, type = "effects"), "ggplot")
  expect_equal(levels(fit$rater_effects$group), c("exp", "inexp"))
})

test_that("ord_kappa_test compares subject groups", {
  skip_if_not_installed("ordinal")
  skip_on_cran()
  d <- ordk_group_data()
  d$age <- factor(d$age, levels = c("young", "old"))
  res <- ord_kappa_test(data = d, measure = "rating", item = "rater",
                        id = "id", group = "age", group_type = "subject")
  expect_equal(names(res$estimate), c("kappa_m (young)", "kappa_m (old)"))
  expect_equal(nrow(res$fit$subgroups), 2)
  expect_length(res$fit$sigma2_subject, 2)
  expect_match(summary(res)$text, "subject variances")
})

test_that("ord_kappa_test validates the group", {
  skip_if_not_installed("ordinal")
  d <- ordk_group_data()
  expect_error(ord_kappa_test(rating ~ rater | id, data = d, group = "exp",
                              null.value = 0.2), "leave 'null.value'")
  expect_error(ord_kappa_test(rating ~ rater | id, data = d, group = "nope"),
               "not found")
  d$three <- c("a", "b", "c")[as.integer(d$rater) %% 3 + 1]
  expect_error(ord_kappa_test(rating ~ rater | id, data = d, group = "three"),
               "exactly 2 levels")
  expect_error(ord_kappa_test(rating ~ rater | id, data = d, group = "age"),
               "constant within each rater")
  wide <- data.frame(a = 1:4, b = 1:4, c = 1:4, g = c(1, 1, 2, 2))
  expect_error(ord_kappa_test(col.names = c("a", "b", "c"), data = wide,
                              group = "g"), "requires long data")
})
