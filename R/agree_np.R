#' Nonparametric Test for Limits of Agreement
#' @description agree_np produces an agreement analysis based on quantiles and
#' @param x Name of column with first measurement.
#' @param y Name of other column with first measurement.
#' @param id Column with subject identifier with samples are taken in replicates.
#' @param data Data frame with all data.
#' @param conf.level the confidence level required. Default is 95\%.
#' @param agree.level the agreement level required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95.
#' @param delta The threshold below which methods agree/can be considered equivalent and this argument is required. Equivalence Bound for Agreement or Maximal Allowable Difference.
#' @param prop_bias Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the limits of agreement calculations.
#' @return Returns adv-agree object with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"loa"}}{a data frame of the limits of agreement.}
#'   \item{\code{"h0_test"}}{Decision from hypothesis test.}
#'   \item{\code{"call"}}{the matched call}
#'
#' }

#' @examples
#' # TBA
#' @section References:
#' TBA
#' @importFrom stats pnorm qnorm lm anova dchisq qchisq sd var model.frame
#' @import quantreg
#' @importFrom tidyselect all_of
#' @importFrom tidyr drop_na pivot_longer
#' @import dplyr
#' @import ggplot2
#' @export

agree_np <- function(x,
                     y,
                     id = NULL,
                     data,
                     delta = NULL,
                     prop_bias = FALSE,
                     agree.level = .95,
                     conf.level = .95){

  agreeq = qnorm(1 - (1 - agree.level) / 2)
  agree.l = 1 - (1 - agree.level) / 2
  agree.u = (1 - agree.level) / 2
  confq = qnorm(1 - (1 - conf.level) / 2)
  alpha.l = 1 - (1 - conf.level) / 2
  alpha.u = (1 - conf.level) / 2

  if(!is.null(id)){
    df = data %>%
      select(all_of(id),all_of(x),all_of(y)) %>%
      rename(id = all_of(id),
             x = all_of(x),
             y = all_of(y)) %>%
      select(id,x,y)
  } else {
    df = data %>%
      select(all_of(x),all_of(y)) %>%
      rename(x = all_of(x),
             y = all_of(y)) %>%
      select(x,y)
    df$id = as.factor(1)
    df = df %>%
      drop_na()
  }

  df$mean = (df$x+dfy)/2
  df$delta = (df$x-df$y)

  quan_mod2 = rq(formula =  delta ~ x,
                data = df,
                tau = c(agree.u,.5,agree.l))

  quan_mod1 = SuppressWarnings({rq(formula =  delta ~ 1,
                 data = df,
                 tau = c(agree.u,.5,agree.l))})

  ## Save LoA ----
  df_loa = data.frame(
    estimate = c(d_bar, loa_l, loa_u),
    lower.ci = c(d_lo, loa_l.l, loa_u.l),
    upper.ci = c(d_hi, loa_l.u, loa_u.u),
    row.names = c("Difference","Lower LoA","Upper LoA")
  )

  if (!missing(delta)) {
    rej <- (-delta < loa_l.l) * (loa_u.l < delta)
    rej_text = "don't reject h0"
    if (rej == 1) {
      rej_text = "reject h0"
    }
  } else {
    rej_text = "No Hypothesis Test"
  }

  # Save call----
  # function name will be: as.character(call2[[1]])
  call2 = match.call()
  if(is.null(call2$agree.level)){
    call2$agree.level = agree.level
  }

  if(is.null(call2$conf.level)){
    call2$conf.level = conf.level
  }
  # Return Results ----

  structure(list(loa = df_loa,
                 h0_test = rej_text,
                 call = call2),
            class = "adv_agree")

}
