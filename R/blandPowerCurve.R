#' @title Power Curve for Bland-Altman Limits of Agreement
#'
#' @description
#' `r lifecycle::badge('maturing')`
#' This function calculates the power for the Bland-Altman method under varying parameter settings and for a range of sample sizes.
#'
#' @param samplesizes vector of samples sizes at which to estimate power: the
#'   number of pairs, or the number of subjects when `m > 1`.
#' @param mu mean of differences
#' @param SD standard deviation of differences (the *total* SD when `m > 1`)
#' @param delta The threshold below which methods agree/can be considered equivalent, can be in any units. Equivalence Bound for Agreement. More than one delta can be provided.
#' @param conf.level the confidence level(s) required. Default is 95%. More than one confidence level can be provided.
#' @param agree.level the agreement level(s) required. Default is 95%. The proportion of data that should lie between the thresholds, for 95% limits of agreement this should be 0.95. More than one confidence level can be provided.

#' @param m number of paired measurements per subject (default 1, independent
#'   pairs).
#' @param icc intraclass correlation of the paired *differences*, in \[0, 1).
#'   Ignored when `m = 1`.
#'
#' @details
#' Power follows the approximate method of Lu et al. (2016). With clustered
#' data (`m > 1`), the standard error of the limits uses the effective sample
#' size n \* m / (1 + (m - 1) \* icc) for the mean difference and a
#' Satterthwaite df for the SD, under a balanced compound symmetry model (see
#' [agree_expected_half()] for the model). Like the original method, this is
#' an approximation; `m = 1` gives the original results.
#'
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
                            agree.level = .95,
                            m = 1,
                            icc = 0){
  if(length(mu) > 1) {
    stop("Length of mu cannot be greater than 1")
  }
  if(length(SD) > 1) {
    stop("Length of SD cannot be greater than 1")
  }
  if(SD <= 0) {
    stop("'SD' must be positive")
  }
  if(any(conf.level <= 0 | conf.level >= 1)) {
    stop("'conf.level' must be between 0 and 1")
  }
  if(any(agree.level <= 0 | agree.level >= 1)) {
    stop("'agree.level' must be between 0 and 1")
  }
  if(any(samplesizes < 2 | samplesizes != round(samplesizes))) {
    stop("'samplesizes' must be integers >= 2")
  }
  .check_cluster_args(m, icc)
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
                                  conf.level = dat_grid$conf.level[i],
                                  m = m,
                                  icc = icc) %>%
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
           conf.level = dat_grid$conf.level[i],
           m = m,
           icc = icc)
  final_dat = rbind(final_dat,result)
  }
  final_dat = final_dat %>%
    mutate(power = ifelse(power < 0, 0,power))
  class(final_dat) <- list("powerCurve","data.frame")
  return(final_dat)
}
