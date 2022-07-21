context("agree_np")


testthat::test_that("Simple Use Run Through", {

    data("reps")
    agree1 = suppressWarnings(agree_np(x = "x",
                        y = "y",
                      data = reps,
                        delta = 2.5))

    agree2 = agree_np(x = "x",
                      y = "y",
                      prop_bias = TRUE,
                      data = reps,
                      delta = 2.5)

    expect_equivalent(agree1$h0_test, "don't reject h0")
    expect_equivalent(agree2$h0_test, "don't reject h0")

    p1 = plot(agree1)
    p1 = plot(agree1, smooth_method = "lm")
    p1 = plot(agree1, smooth_method = "loess")
    p1 = plot(agree1, smooth_method = "gam")
    p2 = plot(agree2)
    p1 = plot(agree1, 2)
    p2 = plot(agree2, 2)

    agree1 = suppressWarnings(agree_np(x = "x",
                                       y = "y",
                                       data = reps,
                                       delta = 2.5,
                                       TOST = FALSE))

    agree2 = agree_np(x = "x",
                      y = "y",
                      prop_bias = TRUE,
                      data = reps,
                      delta = 2.5,
                      TOST = FALSE)

    expect_equivalent(agree1$h0_test, "don't reject h0")
    expect_equivalent(agree2$h0_test, "don't reject h0")

    p1 = plot(agree1)
    p1 = plot(agree1, smooth_method = "lm")
    p1 = plot(agree1, smooth_method = "loess")
    p1 = plot(agree1, smooth_method = "gam")
    p2 = plot(agree2)
    p1 = plot(agree1, 2)
    p2 = plot(agree2, 2)

    expect_error(agree_np(x = "x",
                                       y = "y",
                                       data = reps,
                                       TOST = FALSE))


    agree1 = suppressWarnings(agree_np(x = "x",
                                       y = "y",
                                       id = "id",
                                       data = reps,
                                       delta = 2.5,
                                       TOST = FALSE))

    agree2 = agree_np(x = "x",
                      y = "y",
                      id = "id",
                      prop_bias = TRUE,
                      data = reps,
                      delta = 2.5,
                      TOST = FALSE)

    agree1 = suppressWarnings(agree_np(x = "x",
                                       y = "y",
                                       id = "id",
                                       data = reps,
                                       delta = 2.5,
                                       TOST = TRUE))

    agree2 = agree_np(x = "x",
                      y = "y",
                      id = "id",
                      prop_bias = TRUE,
                      data = reps,
                      delta = 2.5,
                      TOST = TRUE)

    expect_message(agree_np(x = "x",
                            y = "y",
                            id = "id",
                            prop_bias = TRUE,
                            data = reps,
                            delta = 10,
                            TOST = TRUE))

    expect_equivalent(agree1$h0_test, "don't reject h0")
    expect_equivalent(agree2$h0_test, "don't reject h0")

    p1 = plot(agree1)
    p1 = plot(agree1, smooth_method = "lm")
    p1 = plot(agree1, smooth_method = "loess")
    p1 = plot(agree1, smooth_method = "gam")
    p2 = plot(agree2)
    p1 = plot(agree1, 2)
    p2 = plot(agree2, 2)
    print(agree1)
    print(agree2)
    expect_error(check(agree1))


    p1 = plot(agree1,
              geom = "geom_point")

    p1 = plot(agree1,
              geom = "geom_bin2d")

    p1 = plot(agree1,
              geom = "geom_density_2d")

    p1 = plot(agree1,
              geom = "geom_density_2d_filled")

    p1 = plot(agree1,
              geom = "stat_density_2d")


})
