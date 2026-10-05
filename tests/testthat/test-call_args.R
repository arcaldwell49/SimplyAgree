context("call_args")

# GitHub issue #50: the print and plot methods read settings (delta,
# conf.level, TOST, ...) back out of the stored call. Arguments supplied as
# expressions (e.g., delta = log(1.10)) or as variables from another
# environment (e.g., inside a wrapper function) were stored unevaluated, so
# plot() failed with "non-numeric argument to binary operator" or
# "object not found". The stored call now holds the evaluated values.

df50 <- data.frame(
  x1 = c(0, 0, 0, 0, -0.15, 0, 0, 0, 0, 0, -0.185714286, 0, 0, 0, 0, -0.05,
         0.028571429, 0, 0.028571429, 0, 0, 0, 0, 0, 0, 0, -0.05, 0, -0.3,
         0, 0),
  x2 = c(-0.255255255, -0.092972973, -0.421428571, -0.001228501,
         -0.027777778, -0.103070175, 0.042857143, -0.005555556, 0.048178613,
         -0.236842105, -0.092436975, 0.117777778, 0.08168643, -0.090909091,
         -0.082309582, -0.038461538, 0.058823529, -0.03968254, -0.008009153,
         0.052287582, -0.001349528, 0, -0.016746411, 0.078947368,
         -0.031400966, -0.161616162, 0.057142857, 0.051578947, -0.117647059,
         -0.169642857, -0.057017544)
)

render <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  print(p)
  invisible(TRUE)
}

test_that("issue #50 example: agree_np plots with delta = log(1.10)", {
  expect_warning(
    res <- agree_np(x = "x1", y = "x2", data = df50, delta = log(1.10),
                    TOST = TRUE, prop_bias = TRUE),
    NA
  )
  expect_equal(res$call$delta, log(1.10))
  p <- plot(res)
  expect_s3_class(p, "ggplot")
  expect_true(render(p))
})

test_that("settings passed from a wrapper function are stored as values", {
  data("reps")
  f_test <- function(d, cl) suppressWarnings(
    agree_test(x = reps$x, y = reps$y, delta = d, conf.level = cl))
  f_reps <- function(d, cl) agree_reps(x = "x", y = "y", id = "id",
                                       data = reps, delta = d,
                                       conf.level = cl)
  f_nest <- function(d, cl) agree_nest(x = "x", y = "y", id = "id",
                                       data = reps, delta = d,
                                       conf.level = cl)
  f_np <- function(d, cl) agree_np(x = "x", y = "y", data = reps,
                                   delta = d, conf.level = cl)

  for (res in list(f_test(2, 0.9), f_reps(2, 0.9), f_nest(2, 0.9),
                   suppressWarnings(f_np(2, 0.9)))) {
    expect_equal(res$call$delta, 2)
    expect_equal(res$call$conf.level, 0.9)
    expect_output(print(res))
    expect_true(render(plot(res)))
    expect_true(render(plot(res, type = 2)))
  }
})

test_that("settings supplied as expressions are stored as values", {
  data("reps")
  res <- suppressWarnings(
    agree_test(x = reps$x, y = reps$y, delta = sqrt(4),
               conf.level = 1 - 0.1, agree.level = 1 - 0.05,
               TOST = !FALSE, prop_bias = !TRUE))
  expect_equal(res$call$delta, 2)
  expect_equal(res$call$conf.level, 0.9)
  expect_equal(res$call$agree.level, 0.95)
  expect_true(res$call$TOST)
  expect_false(res$call$prop_bias)
  expect_output(print(res))
  expect_true(render(plot(res)))
})

test_that("delta is absent from the call when not supplied", {
  data("reps")
  expect_null(suppressWarnings(agree_test(x = reps$x, y = reps$y))$call$delta)
  expect_null(agree_reps(x = "x", y = "y", id = "id",
                         data = reps)$call$delta)
  expect_null(agree_nest(x = "x", y = "y", id = "id",
                         data = reps)$call$delta)
})

test_that("stored call does not keep a copy of the data arguments", {
  data("reps")
  res <- agree_reps(x = "x", y = "y", id = "id", data = reps, delta = 2)
  expect_true(is.name(res$call$data))
  res2 <- suppressWarnings(agree_test(x = reps$x, y = reps$y, delta = 2))
  expect_true(is.call(res2$call$x))
  expect_true(is.call(res2$call$y))
})
