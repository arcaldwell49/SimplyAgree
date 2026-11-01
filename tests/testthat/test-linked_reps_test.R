test_that("linked_reps_test returns a permutation htest", {
  dat <- sim_methods(n_items = 12, n_reps = 2, methods = c("A", "B"),
                     alpha = c(0, 0.5), tau = 0.3, omega = 0.5,
                     sigma = 0.3, replicates = "linked", seed = 1)
  fit <- method_vc(dat, y = "y", method = "method", id = "id",
                   replicate = "repl", replicates = "linked")
  res <- linked_reps_test(fit, n_perm = 5, seed = 1)
  expect_s3_class(res, c("mvc_perm_htest", "htest"))
  vc <- fit$varcomp
  expect_equal(unname(res$statistic), vc$sd[vc$component == "omega"])
  expect_equal(unname(res$parameter), 5)
  expect_length(res$omega_perm, 5)
  expect_equal(res$p.value,
               (1 + sum(res$omega_perm >= res$statistic)) / 6)
  expect_equal(unname(res$estimate), mean(res$omega_perm))
  expect_equal(res$limits$observed[res$limits$quantity == "sd"],
               fit$limits$sd)
  expect_true(all(c("sd", "lower_loa", "upper_loa") %in% res$limits$quantity))
  # reproducible, and the global RNG is untouched
  set.seed(10)
  before <- .Random.seed
  res2 <- linked_reps_test(fit, n_perm = 5, seed = 1)
  expect_identical(.Random.seed, before)
  expect_equal(res2$omega_perm, res$omega_perm)
  expect_output(print(res), "Permutation test of linked replicates")
})

test_that("linked_reps_test validates its input", {
  dat <- sim_methods(n_items = 12, n_reps = 2, methods = c("A", "B"),
                     alpha = c(0, 0.5), sigma = 0.3, seed = 1)
  fe <- suppressWarnings(method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "exchangeable"))
  expect_error(linked_reps_test(fe), "linked")
  expect_error(linked_reps_test(lm(y ~ 1, data = dat)), "simple_mvc")
  fl <- suppressWarnings(method_vc(dat, y = "y", method = "method", id = "id",
                                   replicate = "repl", replicates = "linked"))
  expect_error(linked_reps_test(fl, n_perm = 0), "n_perm")
})
