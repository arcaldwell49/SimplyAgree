context("dem_reg")


testthat::test_that("Compare to NCSS 303-26", {
  X=c(7,8.3,10.5,9,5.1,8.2,10.2,10.3,7.1,5.9)
  Y=c(7.9,8.2,9.6,9,6.5,7.3,10.2,10.6,6.3,5.2)
  df1 = data.frame(x = X,
         y = Y)
  dm_u = dem_reg('x',
                'y',
                data = df1,
                weighted = FALSE,
                error.ratio = 4)
  dm_u = dem_reg('x',
                 'y',
                 data = df1,
                 weighted = FALSE,
                 error.ratio = 4,
                 keep_data = TRUE)
  dm_u_jam = jmvdeming(data = df1,
                       method1 = 'x',
                       method2 = 'y')
  dm_u_jam2 = jmvdeming(data = df1,
                       method1 = 'x',
                       method2 = 'y',
                       plotcon = TRUE,
                       plotcheck = TRUE,
                       weighted = TRUE)
  expect_equivalent(round(dm_u$model$coef,4),
                    c(-0.0897, 1.0012))

  expect_equivalent(round(dm_u$model$se,4),
                    c(1.7220, 0.1872))
  p1 = plot(dm_u)
  c1 = check(dm_u)

  dm_w = dem_reg('x',
                  'y',
                  data = df1,
                  weighted = TRUE,
                 error.ratio = 4)

  expect_equivalent(round(dm_w$model$coef,4),
                    c(-0.3251, 1.0309))

  expect_equivalent(round(dm_w$model$se,3),
                    c(1.961, 0.219))

  p1 = plot(dm_w)
  c1 = check(dm_w)
  print(dm_w)
  })

testthat::test_that("Simple Run Through with Nested", {

  data('reps')

  dm1 = dem_reg(data = reps,
                x = "x",
                y = "y",
                weighted = TRUE,
                id = "id")

  dm2 = dem_reg(data = reps,
                x = "x",
                y = "y",
                weighted = FALSE,
                id = "id")

  expect_warning( dem_reg(data = reps,
                        x = "x",
                        y = "y",
                        weighted = TRUE,
                        weights = c(1,2),
                        id = "id"))
  p1 = plot(dm1)
  p2 = plot(dm2)
  c1 = check(dm1)
  c2 = check(dm2)
  print(dm1)
})
