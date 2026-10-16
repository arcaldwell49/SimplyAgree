# summary paragraphs are stable

    Code
      summary(icc_test(col.names = sf_cols, data = sf_wide))
    Output
      The estimated ICC(C,1) was 0.715 (95% compatibility interval [0.342,
      0.946]). Values in this interval are the most compatible with the data,
      given the model and its assumptions. The estimate is the two-way,
      consistency, single-measure ICC (McGraw & Wong ICC(C,1); Shrout &
      Fleiss ICC(3,1)): the reliability of a single measurement, excluding
      systematic differences between items. The data had 6 subjects and 4
      items (24 measurements, none missing); variance components were
      estimated by analysis of variance. The F-based interval is exact
      (McGraw & Wong, 1996).

---

    Code
      summary(res)
    Output
      The test is not statistically significant (F(5, 4.3) = 0.431, p =
      0.810; ICC(A,1) = 0.290, 95% lower bound 0.0429) at the 0.05 alpha
      level. The null hypothesis cannot be rejected: at the desired error
      rate, it cannot be stated that the true ICC is greater than 0.5. The
      estimate is the two-way, absolute agreement, single-measure ICC (McGraw
      & Wong ICC(A,1); Shrout & Fleiss ICC(2,1)): the reliability of a single
      measurement, including systematic differences between items. The data
      had 6 subjects and 4 items (24 measurements, none missing); variance
      components were estimated by analysis of variance. The interval and F
      test use Satterthwaite approximations (McGraw & Wong, 1996).

---

    Code
      summary(res, style = "estimation")
    Output
      The estimated ICC(A,1) was 0.290 (95% lower compatibility bound
      0.0429). Values above this bound are the most compatible with the data,
      given the model and its assumptions. Against the hypothesis that the
      true ICC(A,1) is at most 0.5, p = 0.810, an S-value of 0.304 bits of
      information (S = -log2 p): less surprising than a single head from one
      toss of a fair coin. The estimate is the two-way, absolute agreement,
      single-measure ICC (McGraw & Wong ICC(A,1); Shrout & Fleiss ICC(2,1)):
      the reliability of a single measurement, including systematic
      differences between items. The data had 6 subjects and 4 items (24
      measurements, none missing); variance components were estimated by
      analysis of variance. The interval and F test use Satterthwaite
      approximations (McGraw & Wong, 1996).

