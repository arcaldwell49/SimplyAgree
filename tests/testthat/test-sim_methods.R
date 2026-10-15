test_that("sim_methods returns the documented structure", {
  dat = sim_methods(n_items = 10, n_reps = 2, methods = c("A", "B", "C"),
                    seed = 1)
  expect_s3_class(dat, "tbl_df")
  expect_named(dat, c("id", "method", "repl", "y"))
  expect_equal(nrow(dat), 10 * 2 * 3)
  expect_equal(levels(dat$method), c("A", "B", "C"))

  truth = attr(dat, "truth")
  expect_named(truth, c("params", "mu_mean", "mu_sd", "omega", "pairs",
                        "repeatability", "conversion", "design"))
  expect_equal(nrow(truth$params), 3)
  expect_equal(nrow(truth$pairs), 3)
  expect_equal(truth$pairs$method1, c("A", "A", "B"))
  expect_equal(truth$pairs$method2, c("B", "C", "C"))
  expect_equal(nrow(truth$conversion), 9)
  expect_true(is.na(truth$omega))
  expect_false("rpc" %in% names(truth$repeatability))
})

test_that("sim_methods is reproducible and keeps the global RNG state", {
  set.seed(99)
  seed_before = .Random.seed
  d1 = sim_methods(n_items = 5, seed = 10)
  d2 = sim_methods(n_items = 5, seed = 10)
  expect_identical(d1, d2)
  expect_identical(.Random.seed, seed_before)
})

test_that("sim_methods truth follows the model formulas", {
  dat = sim_methods(n_items = 5, n_reps = 2, methods = c("A", "B"),
                    alpha = c(1, 3), beta = c(1, 2), mu_mean = 10,
                    tau = c(0.5, 0.2), omega = 0.3, sigma = c(1, 2),
                    replicates = "linked", seed = 1)
  truth = attr(dat, "truth")
  z = qnorm(0.975)
  expect_equal(truth$params$tau_y, c(0.5, 0.4))
  expect_equal(truth$params$omega_y, c(0.3, 0.6))
  # bias at mu_mean: (1 + 10) - (3 + 2 * 10)
  expect_equal(truth$pairs$bias, -12)
  expect_false(truth$pairs$constant_bias)
  sd_ab = sqrt(0.5^2 + 0.4^2 + 1 + 4 + (1 - 2)^2 * 0.3^2)
  expect_equal(truth$pairs$sd, sd_ab)
  expect_equal(truth$pairs$upper_loa, -12 + z * sd_ab)
  expect_equal(truth$repeatability$rc, z * sqrt(2) * c(1, 2))
  expect_equal(truth$repeatability$rpc,
               z * sqrt(2) * sqrt(c(1, 4) + c(0.3, 0.6)^2))

  conv = truth$conversion
  b_to_a = conv[conv$to == "B" & conv$from == "A", ]
  expect_equal(b_to_a$beta, 2)
  expect_equal(b_to_a$alpha, 3 - 1 * 2)
  expect_equal(b_to_a$sd_pred, sqrt(4 * (0.5^2 + 1) + 0.4^2 + 4))
  a_to_a = conv[conv$to == "A" & conv$from == "A", ]
  expect_equal(a_to_a$alpha, 0)
  expect_equal(a_to_a$beta, 1)
  expect_equal(a_to_a$sd_pred, sqrt(2) * 1)
})

test_that("sim_methods sample moments match the truth (large sample)", {
  dat = sim_methods(n_items = 2000, n_reps = 3,
                    methods = c("A", "B", "C"),
                    alpha = c(0, 1, -0.5), mu_mean = 5, mu_sd = 2,
                    tau = c(0.3, 0.4, 0.5), omega = 0.6,
                    sigma = c(0.2, 0.4, 0.6),
                    replicates = "linked", seed = 2024)
  truth = attr(dat, "truth")
  wide = tidyr::pivot_wider(dat, names_from = method, values_from = y)

  # differences on the same occasion: bias and SD of the difference
  d_ab = wide$A - wide$B
  d_bc = wide$B - wide$C
  expect_equal(mean(d_ab), truth$pairs$bias[1], tolerance = 0.02)
  expect_equal(sd(d_ab), truth$pairs$sd[1], tolerance = 0.03)
  expect_equal(mean(d_bc), truth$pairs$bias[3], tolerance = 0.03)
  expect_equal(sd(d_bc), truth$pairs$sd[3], tolerance = 0.03)

  # replicates of one method within item: sigma^2 + omega^2 (linked)
  within_var = dat %>%
    dplyr::group_by(method, id) %>%
    dplyr::summarize(v = stats::var(y), .groups = "drop") %>%
    dplyr::group_by(method) %>%
    dplyr::summarize(v = mean(v))
  expect_equal(sqrt(within_var$v),
               sqrt(truth$params$sigma^2 + truth$params$omega_y^2),
               tolerance = 0.03)

  # item means
  expect_equal(mean(dat$y[dat$method == "A"]), 5, tolerance = 0.05)
})

test_that("sim_methods exchangeable replicates estimate sigma", {
  dat = sim_methods(n_items = 2000, n_reps = 3, methods = c("A", "B"),
                    tau = 0.5, sigma = c(0.3, 0.7), seed = 7)
  within_sd = dat %>%
    dplyr::group_by(method, id) %>%
    dplyr::summarize(v = stats::var(y), .groups = "drop") %>%
    dplyr::group_by(method) %>%
    dplyr::summarize(s = sqrt(mean(v)))
  expect_equal(within_sd$s, c(0.3, 0.7), tolerance = 0.03)
})

test_that("sim_methods missingness keeps every item-method combination", {
  dat = sim_methods(n_items = 50, n_reps = 3, methods = c("A", "B", "C"),
                    p_missing = 0.6, seed = 3)
  expect_lt(nrow(dat), 50 * 3 * 3)
  counts = table(dat$method, dat$id)
  expect_true(all(counts >= 1))
})

test_that("sim_methods back-transforms simulated values", {
  dat = sim_methods(n_items = 200, methods = c("A", "B"), mu_mean = 0,
                    mu_sd = 1, sigma = 0.2, transform = "pctlogit", seed = 4)
  expect_true(all(dat$y > 0 & dat$y < 100))
  expect_equal(attr(dat, "truth")$design$transform, "pctlogit")
  dat_log = sim_methods(n_items = 20, transform = "log", seed = 4)
  expect_true(all(dat_log$y > 0))
})

test_that("sim_methods input validation", {
  expect_error(sim_methods(methods = "A"), "at least two")
  expect_error(sim_methods(methods = c("A", "A")), "at least two distinct")
  expect_error(sim_methods(n_items = 1), "at least 2")
  expect_error(sim_methods(n_reps = 1.5), "whole number")
  expect_error(sim_methods(alpha = c(1, 2, 3)), "one number per method")
  expect_error(sim_methods(sigma = -1), "must not be negative")
  expect_error(sim_methods(beta = 0), "must not be zero")
  expect_error(sim_methods(p_missing = 1), "p_missing")
  expect_error(sim_methods(mu_sd = -1), "must not be negative")
  expect_warning(sim_methods(omega = 1, seed = 1), "ignored")
})
