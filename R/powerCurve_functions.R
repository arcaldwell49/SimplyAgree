#' @importFrom stats qnorm qt pt
NULL

# Power of the Bland-Altman agreement procedure (Lu et al. 2016, eq. 5):
# agreement is concluded when the lower CI limit of the lower LoA is above
# -delta and the upper CI limit of the upper LoA is below delta. Vectorized
# over n. With clustered data (m > 1) the SE of the limits uses the effective
# sample size for the mean and a Satterthwaite df for the SD; m = 1 gives the
# original method.
.lu_power <- function(n, delta, mu, sigma, conf.level = 0.95,
                      agree.level = 0.95, m = 1, icc = 0) {
  zgamma <- qnorm(1 - (1 - agree.level) / 2)
  n_eff <- .n_eff(n, m, icc)
  df <- .s2_df(n, m, icc)

  # SE of the limits of agreement (Lu eq 1 and 2)
  se <- sigma * sqrt(1 / n_eff + zgamma^2 / (2 * df))
  talpha <- qt(1 - (1 - conf.level) / 2, df = df)

  # non-centrality parameters; type II errors (Lu eq 3 and 4)
  tau1 <- (delta - mu - zgamma * sigma) / se
  tau2 <- (delta + mu - zgamma * sigma) / se
  beta1 <- pt(talpha, df = df, ncp = tau1)
  beta2 <- pt(talpha, df = df, ncp = tau2)

  pmax(1 - (beta1 + beta2), 0)
}
