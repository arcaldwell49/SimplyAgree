#' Estimate power curve for Bland-Altman limits of agreement test
#'
#' @param samplesizes vector of samples sizes to
#' @param mu mean of differences
#' @param SD standard deviation of differences
#' @param delta pre-determined clinical agreement interval
#' @param conf.level the confidence level required. Default is 95\%.
#' @param agree.level the agreement level required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95.

#' @return a dataframe containing the power analysis results
#' @examples
#' \dontrun{
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
#' @importFrom magrittr "%>%"
#' @export
blandPowerCurve <- function(samplesizes = seq(10, 100, 1),
                            mu = 0,
                            SD,
                            delta,
                            conf.level = .95,
                            agree.level = .95){
  alpha = 1-conf.level
  gamma = 1-agree.level
  LOA <- estimateLimitsOfAgreement(mu = mu, SD = SD, agree.level = agree.level)

  df <- lapply(samplesizes, function(this_n) {
    LOA %>%
      estimateConfidenceIntervals(n = this_n, conf.level = conf.level) %>%
      estimateTypeIIerror(delta = delta) %>%
      estimatePowerFromBeta() %>%
      unlist(recursive = FALSE) %>%
      as.data.frame()
  })


  result <- do.call(rbind, df)
  class(result) <- list("powerCurve","data.frame")
  return(result)
}
