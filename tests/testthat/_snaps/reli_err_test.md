# summary styles for error statistics

    Code
      summary(res)
    Output
      The test is not statistically significant (X-squared(7) = 4.00, p =
      0.220; SEM = 7.56, 95% upper bound 13.6) at the 0.05 alpha level. The
      null hypothesis cannot be rejected: at the desired error rate, it
      cannot be stated that the true SEM is less than 10. The estimate is the
      standard error of measurement (two-way, consistency), in the units of
      the measurement: the typical error of a single measurement, excluding
      systematic differences between items. The data had 8 subjects and 2
      items (16 measurements, none missing); variance components were
      estimated by analysis of variance. The interval and chi-square test are
      exact under normality (chi-square distribution of the error variance).

---

    Code
      est
    Output
      The estimated SEM was 7.56 (95% upper compatibility bound 13.6). Values
      below this bound are the most compatible with the data, given the model
      and its assumptions. Against the hypothesis that the true SEM is at
      least 10, p = 0.220, an S-value of 2.18 bits of information (S = -log2
      p): about as surprising as getting 2 heads in a row from 2 tosses of a
      fair coin. The estimate is the standard error of measurement (two-way,
      consistency), in the units of the measurement: the typical error of a
      single measurement, excluding systematic differences between items. The
      data had 8 subjects and 2 items (16 measurements, none missing);
      variance components were estimated by analysis of variance. The
      interval and chi-square test are exact under normality (chi-square
      distribution of the error variance).

---

    Code
      summary(reli_err_test(col.names = wcols, data = weir_a, stat = "SEE"))
    Output
      The estimated SEE was 7.35 (95% compatibility interval [4.48, 12.1]).
      Values in this interval are the most compatible with the data, given
      the model and its assumptions. The estimate is the standard error of
      estimate (two-way, consistency), in the units of the measurement: the
      standard error of a subject's estimated true score, SD * sqrt(ICC * (1
      - ICC)) with ICC(C,1) and the SD matched to its denominator, excluding
      systematic differences between items. The data had 8 subjects and 2
      items (16 measurements, none missing); variance components were
      estimated by analysis of variance. The interval uses the delta method
      on the log scale with normal-theory variances of the mean squares.

