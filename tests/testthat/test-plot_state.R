context("plot_state")

# GitHub issue #76: in jamovi the analysis object is stored with setState() and
# restored before the plots are rendered. The environment the analysis was run
# in is not restored, so any object that carried its plotting data as a formula
# pointing at that environment failed with "object 'y' not found".
# strip_env() emulates that restore by replacing every environment in the
# object with the global environment.

strip_env <- function(x) {
  unserialize(serialize(x, NULL, refhook = function(e) ""),
              refhook = function(s) globalenv())
}

test_that("plotting data is stored as a self-contained model frame", {
  data("reps")
  data("temps")

  x1 = c(1, 2, 3, 4, 5)
  y1 = c(1.2, 1.3, 3, 4, 5)

  mods = list(
    agree_test = suppressWarnings(agree_test(x = x1, y = y1, delta = 2)),
    agree_reps = suppressWarnings(agree_reps(x = "x", y = "y", id = "id",
                                             data = reps)),
    agree_nest = suppressWarnings(agree_nest(x = "x", y = "y", id = "id",
                                             data = reps)),
    agreement_limit = agreement_limit(x = "x", y = "y", id = "id",
                                      data = reps, data_type = "reps"),
    tolerance_limit = tolerance_limit(x = "x", y = "y", id = "id",
                                      data = reps),
    reli_stats = reli_stats(data = reps, wide = TRUE, col.names = c("x","y"))
  )

  for (nm in names(mods)) {
    mf = mods[[nm]]$call$lm_mod
    expect_true(is.data.frame(mf$model), info = nm)
    expect_null(mf$call, info = nm)
    expect_equal(model.frame(mf), mf$model, info = nm)
  }
})

test_that("plots render after the analysis environment is lost (#76)", {
  x1 = c(1, 2, 3, 4, 5)
  y1 = c(1.2, 1.3, 3, 4, 5)

  res = suppressWarnings(agree_test(x = x1, y = y1, delta = 2))
  res2 = strip_env(res)

  expect_s3_class(plot(res2, x_name = "Method 1", y_name = "Method 2"),
                  "ggplot")
  expect_s3_class(plot(res2, type = 2), "ggplot")
  expect_s3_class(check(res2), "patchwork")
})

test_that("repeated measures plots render after the environment is lost (#76)", {
  data("reps")

  reps_test = suppressWarnings(agree_reps(x = "x", y = "y", id = "id",
                                          data = reps))
  nest_test = suppressWarnings(agree_nest(x = "x", y = "y", id = "id",
                                          data = reps))
  loa_test = agreement_limit(x = "x", y = "y", id = "id",
                             data = reps, data_type = "reps")

  expect_s3_class(plot(strip_env(reps_test)), "ggplot")
  expect_s3_class(plot(strip_env(nest_test)), "ggplot")
  expect_s3_class(plot(strip_env(loa_test)), "ggplot")
})

test_that("reliability plots render after the environment is lost (#76)", {
  data("reps")
  reli_test = reli_stats(data = reps, wide = TRUE, col.names = c("x","y"))

  expect_s3_class(plot(strip_env(reli_test)), "ggplot")
})
