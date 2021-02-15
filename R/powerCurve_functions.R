#' @importFrom stats pf qf qnorm qt pt
#' @importFrom utils stack

estimateLimitsOfAgreement <- function(mu, SD, agree.level = 0.95){

  gamma = 1-agree.level
  # compute limits of agreement
  zgamma = qnorm(1 - gamma/2, lower.tail = TRUE)
  upperLOA = mu + zgamma * SD
  lowerLOA = mu - zgamma * SD

  result <- list(mu = mu,
                 SD = SD,
                 gamma = gamma,
                 zgamma = zgamma,
                 upperLOA = upperLOA,
                 lowerLOA = lowerLOA)

  class(result) <- "LOA"
  return(result)
}

estimateConfidenceIntervals <- function(LOA, n, conf.level = 0.95){
  alpha = 1-conf.level

  # get from LOA object
  if(!"LOA" %in% class(LOA)) stop("input must be an LOA object produced by estimateLimitsOfAgreement")
  lowerLOA <- LOA$lowerLOA
  upperLOA <- LOA$upperLOA
  zgamma <- LOA$zgamma
  SD <- LOA$SD
  mu <- LOA$mu

  # compute standard error on LOA
  se = SD * sqrt( (1/n) + (zgamma^2)/(2*(n - 1)) )

  # compute quantiles of t-distribution
  talpha = qt(1 - alpha/2, df = n - 1, lower.tail = TRUE)

  # compute CI of mean
  mu_upperCI <- mu + talpha * SD * sqrt(1/n)
  mu_lowerCI <- mu - talpha * SD * sqrt(1/n)

  # CI on lower LOA (Lu eq 1)
  lowerLOA_upperCI = lowerLOA + talpha * se
  lowerLOA_lowerCI = lowerLOA - talpha * se

  # CI on upper LOA (Lu eq 2)
  upperLOA_lowerCI = upperLOA - talpha * se
  upperLOA_upperCI = upperLOA + talpha * se

  result <- list(
    n = n,
    se = se,
    alpha = alpha,
    talpha = talpha,
    mu_upperCI = mu_upperCI,
    mu_lowerCI = mu_lowerCI,
    lowerLOA_upperCI = lowerLOA_upperCI,
    lowerLOA_lowerCI = lowerLOA_lowerCI,
    upperLOA_lowerCI = upperLOA_lowerCI,
    upperLOA_upperCI = upperLOA_upperCI
  )
  class(result) <- "CI"

  joined <- list(LOA = LOA, CI = result)
  class(joined) <- list("LOA", "CI")
  return(joined)

}

estimateTypeIIerror <- function(CI, delta){

  # get from CI object
  if(!"CI" %in% class(CI)) stop("input CI must include a CI object produced by estimateConfidenceIntervals")
  if(!"LOA" %in% class(CI)) stop("input CI must include a LOA object produced by estimateLimitsOfAgreement")
  mu = CI$LOA$mu
  SD = CI$LOA$SD
  zgamma = CI$LOA$zgamma
  n = CI$CI$n
  se = CI$CI$se
  alpha = CI$CI$alpha
  talpha = CI$CI$talpha
  upperLOA = CI$CI$upperLOA
  lowerLOA = CI$CI$lowerLOA

  # estimate type-II error using non-central t-distribution

  tau1 = (delta - mu - zgamma * SD) / se # non-centrality parameter
  tau2 = (delta + mu - zgamma * SD) / se # non-centrality parameter
  beta1 = 1 - pt(talpha,
                 df = n - 1,
                 ncp = tau1,
                 lower.tail = FALSE) # Lu eq 3
  beta2 = 1 - pt(talpha,
                 df = n - 1,
                 ncp = tau2,
                 lower.tail = FALSE) # Lu eq 4


  result <- list(
    delta = delta,
    beta1 = beta1,
    beta2 = beta2
  )
  class(result) <- "beta"

  joined <- list(LOA = CI$LOA, CI = CI$CI, beta = result)
  class(joined) <- list("LOA", "CI", "beta")
  return(joined)
}

estimatePowerFromBeta <- function(beta){

  # get from beta object
  if(!"beta" %in% class(beta)) stop("input beta must include a beta object produced by estimateTypeIIerror")
  if(!"CI"   %in% class(beta)) stop("input beta must include a CI object produced by estimateConfidenceIntervals")
  if(!"LOA"  %in% class(beta)) stop("input beta must include a LOA object produced by estimateLimitsOfAgreement")
  beta1 = beta$beta$beta1
  beta2 = beta$beta$beta2

  power = 1 - (beta1 + beta2) # Lu eq 5


  result <- list(
    power = power
  )
  class(result) <- "power"

  joined <- list(LOA = beta$LOA, CI = beta$CI, beta = beta$beta, power = result)
  class(joined) <- list("beta", "power")
  return(joined)
}
