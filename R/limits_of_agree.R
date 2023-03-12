#' Calculate the Limits of Agreement
#' @description A function for calculating for limits of agreement.
#' @param x Name of column with first measurement
#' @param y Name of other column with the other measurement to compare to the first.
#' @param id Column with subject identifier. Default is "id" if no entry is provided.
#' @param data Data frame with all data.
#' @param agree.level the agreement level required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95.
#' @param prop_bias Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the limits of agreement calculations.
#' @param alpha The alpha-level for confidence levels.
#' @param TOST Logical indicator (TRUE/FALSE) of whether to use two one-tailed tests for the limits of agreement. Default is TRUE.
#' @param log Calculate limits of agreement using log-transformed data.
#' @return Returns single simple_agree class object with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"loa"}}{A.}
#' }

#' @examples
#' data('reps')
#'
#' @section References:
#' Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman limits of agreement with multiple observations per individual. Statistical methods in medical research, 22(6), 630-642.
#' @importFrom stats pnorm qnorm lm dchisq qchisq sd var
#' @importFrom tidyselect all_of
#' @importFrom tidyr drop_na pivot_longer
#' @import dplyr
#' @import ggplot2
#' @export

agreement_limit = function(x,
                           y,
                           id = NULL,
                           data,
                           data_type = c("simple","nest","reps"),
                           loa_calc = c("mover","blandaltman"),
                           agree.level = 0.95,
                           alpha = 0.5,
                           prop_bias = FALSE,
                           TOST = TRUE,
                           log = FALSE){
  data_type = match.arg(data_type)
  loa_calc = match.arg(loa_calc)
  conf.level = 1- alpha


  df = loa_data_org(data = data,
                    x = x,
                    y = y,
                    id = id,
                    data_type=data_type)

}

loa_data_org = function(data,
                        x,
                        y,
                        id,
                        data_type){
  if(data_type == "simple"){
    df = data %>%
      select(all_of(x),all_of(y)) %>%
      rename(x = all_of(x),
             y = all_of(y)) %>%
      select(x,y) %>%
      drop_na()
    df$id = 1:nrow
  } else {
    if(is.null(id)){
      stop("id must be provided if data_type != \'simple\'.")
    }
    df = data %>%
      select(all_of(id),all_of(x),all_of(y)) %>%
      rename(id = all_of(id),
             x = all_of(x),
             y = all_of(y)) %>%
      select(id,x,y) %>%
      drop_na()
  }

  return(df)
}

calc_loa_sumstats_simple = function(df,
                                    conf.level = .95,
                                    agree.level = .95,
                                    TOST = TRUE,
                                    prop_bias = FALSE){

  agreeq = qnorm(1 - (1 - agree.level) / 2)
  agree_l = 1 - (1 - agree.level) / 2
  agree_u = (1 - agree.level) / 2
  confq = qnorm(1 - (1 - conf.level) / 2)
  conf1 = conf.level
  if(TOST == TRUE){
    confq2 = qnorm(1 - (1 - conf.level) )
    alpha.l = 1 - (1 - conf.level)
    alpha.u = (1 - conf.level)
    conf2 = 1 - (1 - conf.level) * 2
  } else {
    confq2 = qnorm(1 - (1 - conf.level) / 2)
    alpha.l = 1 - (1 - conf.level) / 2
    alpha.u = (1 - conf.level) / 2
    conf2 = conf.level
  }

  k <- nrow(df)
  yb <- mean(df$y)
  sy2 <- var(df$y) * (k - 1) / k
  sd1 <- sd(df$y)
  xb <- mean(df$x)
  sx2 <- var(df$x) * (k - 1) / k
  sd2 <- sd(df$x)
  r <- cor(df$x, df$y)
  sl <- r * sd1 / sd2
  sxy <- r * sqrt(sx2 * sy2)

  df = df %>%
    mutate(delta = x - y,
           avg = (x+y)/2)

  if(prop_bias == FALSE){
    # sqrt(var(delta, na.rm = TRUE))
    delta.sd <- sigma(lm(formula = delta ~ 1,
                         data=df))
    dfs = df.residual(lm(formula = delta ~ 1,
                         data=df))
  } else {
    delta.sd <- sigma(lm(formula = delta ~ avg,
                         data=df))
    dfs = df.residual(lm(formula = delta ~ avg,
                         data=df))
  }
  var.d = (delta.sd)^2/k
  var.dlim = (1/k+zv2/(2*(k-1)))*(delta.sd)^2

  bias <- mean(df$delta)
  bias_ci = (bias + c(-1,1) * qt(conf1,dfs)*sqrt(var.d))
  lower_loa = bias - agreeq*delta.sd
  lower_loa_ci = (lower_loa - c(-1,1) *qt(conf2,dfs)*sqrt(var.dlim))
  upper_loa = bias + agreeq*delta.sd
  upper_loa_ci = (upper_loa  - c(-1,1) *qt(conf2,dfs)*sqrt(var.dlim))
  df_loa = data.frame(
    estimate = c(
      upper_loa,
      bias,
      lower_loa),
    lower.ci = c(
      upper_loa_ci[2],
      bias_ci[2],
      lower_loa_ci[2]
                 ),
    upper.ci = c(
      upper_loa_ci[1],
      bias_ci[1],
      lower_loa_ci[1]
                 ),
    ci.level = c(conf2, conf1, conf2),
    row.names = c("Upper LoA","Bias","Lower LoA")
  )

  var_comp = list(
    method = "blandaltman",
    type = "simple",
    n = k,
    total_variance = delta.sd^2
  )

return(var_comp)
}

calc_loa_sumstats_reps = function(df){

}

calc_loa_sumstats_nest = function(df){

}
