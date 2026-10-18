# summary describes the CCC in both styles

    Code
      summary(res)
    Output
      The test is not statistically significant (Z = 1.59, p = 0.056; CCC =
      0.479, 95% lower bound 0.189) at the 0.05 alpha level. The null
      hypothesis cannot be rejected: at the desired error rate, it cannot be
      stated that the true CCC is greater than 0.2. The estimate is Lin's
      concordance correlation coefficient between x and y: how closely the
      pairs fall on the line of identity. It combines precision (Pearson r =
      0.577) and accuracy (bias correction factor C_b = 0.831), CCC = r x
      C_b. The data had 18 complete pairs. The interval and z test use
      Fisher's z transformation with Lin's (1989) delta-method standard
      error, which assumes the two measurements are bivariate normal.

---

    Code
      summary(ccc_test(x = "x", y = "y", id = "id", data = reps, data_type = "reps"))
    Output
      The estimated CCC was 0.107 (95% compatibility interval [-0.160,
      0.359]). Values in this interval are the most compatible with the data,
      given the model and its assumptions. The estimate is the concordance
      correlation coefficient between x and y for replicate measurements,
      estimated by U-statistics (King, Chinchilli & Carrasco, 2007): how
      closely the measurements fall on the line of identity. The data had 4
      subjects (20 measurements of x and 18 of y). The interval uses Fisher's
      z transformation with the U-statistic standard error.

