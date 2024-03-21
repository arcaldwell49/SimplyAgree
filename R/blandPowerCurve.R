#' @title Power Curve for Bland-Altman Limits of Agreement
#'
#' @description
#' `r lifecycle::badge('maturing')`
#' This function calculates the power for the Bland-Altman method under varying parameter settings and for a range of sample sizes.
#'
#' @param samplesizes vector of samples sizes at which to estimate power.
#' @param mu mean of differences
#' @param SD standard deviation of differences
#' @param delta The threshold below which methods agree/can be considered equivalent, can be in any units. Equivalence Bound for Agreement. More than one delta can be provided.
#' @param conf.level the confidence level(s) required. Default is 95%. More than one confidence level can be provided.
#' @param agree.level the agreement level(s) required. Default is 95%. The proportion of data that should lie between the thresholds, for 95% limits of agreement this should be 0.95. More than one confidence level can be provided.

#' @return A dataframe is returned containing the power analysis results. The results can then be plotted with the plot.powerCurve function.
#' @references
#' Lu, M. J., et al. (2016). Sample Size for Assessing Agreement between Two Methods of Measurement by Bland-Altman Method. The international journal of biostatistics, 12(2),
#' \doi{10.1515/ijb-2015-0039}
#' @examples
#' \donttest{
#' powerCurve <- blandPowerCurve(samplesizes = seq(10, 200, 1),
#' mu = 0,
#' SD = 3.3,
#' delta = 8,
#' conf.level = .95,
#' agree.level = .95)
#' # Plot the power curve
#' plot(powerCurve, type = 1)
#' # Find at what N power of .8 is achieved
#' find_n(powerCurve, power = .8)
#' }
#' # If the desired power is not found then
#' ## Sample size range must be expanded
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @export
blandPowerCurve <- function(samplesizes = seq(10, 100, 1),
                            mu = 0,
                            SD,
                            delta,
                            conf.level = .95,
                            agree.level = .95){
  if(length(mu) > 1) {
    stop("aLength of mu cannot be greater than 1")
  }
  if(length(SD) > 1) {
    stop("Length of SD cannot be greater than 1")
  }
  alpha = 1-conf.level
  gamma = 1-agree.level
  final_dat = data.frame()
  dat_grid = expand.grid(conf.level,agree.level,delta)
  colnames(dat_grid) = c("conf.level","agree.level","delta")
  for(i in 1:nrow(dat_grid)){
  LOA <- estimateLimitsOfAgreement(mu = mu,
                                   SD = SD,
                                   agree.level = dat_grid$agree.level[i])

  df <- lapply(samplesizes, function(this_n) {
    LOA %>%
      estimateConfidenceIntervals(n = this_n,
                                  conf.level = dat_grid$conf.level[i]) %>%
      estimateTypeIIerror(delta = dat_grid$delta[i]) %>%
      estimatePowerFromBeta() %>%
      unlist(recursive = FALSE) %>%
      as.data.frame()
  })
  result <- do.call(rbind, df) %>%
    select(CI.n,LOA.mu,LOA.SD,beta.delta,
           power.power) %>%
    rename(mu = LOA.mu,
           SD = LOA.SD,
           N = CI.n,
           delta = beta.delta,
           power = power.power) %>%
    mutate(agree.level = dat_grid$agree.level[i],
           conf.level = dat_grid$conf.level[i])
  final_dat = rbind(final_dat,result)
  }
  final_dat = final_dat %>%
    mutate(power = ifelse(power < 0, 0,power))
  class(final_dat) <- list("powerCurve","data.frame")
  return(final_dat)
}
