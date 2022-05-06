#' Mixed Effects Limits of Agreement
#' @description This function allows for the calculation of bootstrapped limits of agreement when there are multiple observations per subject.
#' @param data A data frame containing the variables within the model.
#' @param diff Column name of the data frame that includes the difference between the 2 measurements of interest.
#' @param avg Column name of the data frame that includes the difference between the 2 measurements of interest.
#' @param condition Column name indicating different conditions subjects were tested under. This can be left missing if there are no differing conditions to be tested.
#' @param formula Optional argument: a two-sided linear formula object describing both the fixed-effects and random-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right. Random-effects terms are distinguished by vertical bars (|) separating expressions for design matrices from grouping factors. Two vertical bars (||) can be used to specify multiple uncorrelated random effects for the same grouping variable
#' @param id Column name indicating the subject/participant identifier
#' @param delta The threshold below which methods agree/can be considered equivalent, can be in any units. Equivalence Bound for Agreement.
#' @param conf.level the confidence level required. Default is 95\%.
#' @param agree.level the agreement level required. Default is 95\%.
#' @param replicates 	the number of bootstrap replicates. Passed on to the boot function. Default is 500.
#' @param type A character string representing the type of bootstrap confidence intervals. Only "norm", "basic", "bca", and "perc" currently supported. Bias-corrected and accelerated, bca, is the default. See ?boot::boot.ci for more details.
#' @param prop_bias Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the limits of agreement calculations.
#' @return Returns single list with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"var_comp"}}{Table of variance components}
#'   \item{\code{"loa"}}{a data frame of the limits of agreement including the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements.}
#'   \item{\code{"h0_test"}}{Decision from hypothesis test.}
#'   \item{\code{"bland_alt.plot"}}{Simple Bland-Altman plot. Red line are the upper and lower bounds for shieh test; grey box is the acceptable limits (delta). If the red lines are within the grey box then the shieh test should indicate 'reject h0', or to reject the null hypothesis that this not acceptable agreement between x & y.}
#'   \item{\code{"conf.level"}}{Returned as input.}
#'   \item{\code{"agree.level"}}{Returned as input.}
#' }
#'
#' @section References:
#' Parker, R. A., Weir, C. J., Rubio, N., Rabinovich, R., Pinnock, H., Hanley, J., McLoughan, L., Drost, E.M., Mantoani, L.C., MacNee, W., & McKinstry, B. (2016). "Application of mixed effects limits of agreement in the presence of multiple sources of variability: exemplar from the comparison of several devices to measure respiratory rate in COPD patients". PLOS One, 11(12), e0168321. <https://doi.org/10.1371/journal.pone.0168321>
#' @importFrom stats qnorm as.formula na.omit
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename
#' @importFrom tidyselect all_of
#' @import lme4
#' @import ggplot2
#' @import boot
#' @import emmeans
#' @export
#'

loa_mixed = function(diff,
                     avg,
                     condition = NULL,
                     id,
                     formula = NULL,
                     data,
                     delta,
                     conf.level = .95,
                     agree.level = .95,
                     replicates = 1999,
                     type = "bca",
                     prop_bias = FALSE){
  message("loa_mixed is deprecated: please use loa_lmer or loa_nlme")
  if (conf.level >= 1 || conf.level <= 0) {
    stop("conf.level must be a value between 0 and 1")
  }
  if(is.null(condition) || missing(condition)){
    condition = 1
  }

  if (agree.level >= 1 || agree.level <= 0) {
    stop("agree.level must be a value between 0 and 1")
  }

  if(is.null(formula)){
    if(prop_bias == FALSE){
      formula1 = as.formula(paste0(diff,"~",condition,"+(1|",id,")"))
    } else {
      formula1 = as.formula(paste0(diff,"~",avg, "+", condition,"+(1|",id,")"))
    }
  } else {
    formula1 = formula
  }

  res_lmer = lmer(
    formula = formula1,
    data = data,
    weights = NULL,
    subset = NULL,
    offset = NULL,
    na.action = na.omit
  )

  boot_index = list(
    bias = 1,
    low_loa = 2,
    upper_loa = 3,
    within_sd = 4,
    between_sd = 5,
    total_sd = 6
  )
  boot_res = boot(
    statistic = loa_bs,
    R = replicates,
    diff = diff,
    condition = condition,
    id = id,
    data = data,
    formula = formula1,
    conf.level = conf.level,
    agree.level = agree.level
  )
  boot_bias = boot.ci(boot_res,
                      index = boot_index$bias,
                      conf = conf.level,
                      type = c("norm", "basic", "perc", "bca"))
  boot_within_sd = boot.ci(boot_res,
                           index = boot_index$within_sd,
                           conf = conf.level,
                           type = c("norm", "basic", "perc", "bca"))
  boot_between_sd = boot.ci(boot_res,
                            index = boot_index$between_sd,
                            conf = conf.level,
                            type = c("norm", "basic", "perc", "bca"))
  boot_total_sd = boot.ci(boot_res,
                          index = boot_index$total_sd,
                          conf = conf.level,
                          type = c("norm", "basic", "perc", "bca"))
  boot_low_loa = boot.ci(boot_res,
                         index = boot_index$low_loa,
                         conf = conf.level,
                         type = c("norm", "basic", "perc", "bca"))
  boot_upper_loa = boot.ci(boot_res,
                           index = boot_index$upper_loa,
                           conf = conf.level,
                           type = c("norm", "basic", "perc", "bca"))
  bsls = list(boot_bias = boot_bias,
              boot_low_loa = boot_low_loa,
              boot_upper_loa = boot_upper_loa,
              boot_within_sd = boot_within_sd,
              boot_between_sd = boot_between_sd,
              boot_total_sd = boot_total_sd)
  res_tab = loa_bstab(bsls = bsls,
                      type = type,
                      conf.level = conf.level)

  rej_text = "No Hypothesis Test"

  if (!missing(delta)) {
    rej <- (-delta < res_tab$lower.ci[2]) * (res_tab$upper.ci[3] < delta)
    rej_text = "don't reject h0"
    if (rej == 1) {
      rej_text = "reject h0"
    }
  }

  df_loa = res_tab[1:3,]
  var_comp = res_tab[4:6,]

  mc = match.call()

  mc$agree.level = agree.level
  mc$conf.level = conf.level
  if (condition != 1) {
    df_plt = data %>%
      select(all_of(diff),
             all_of(id),
             all_of(condition),
             all_of(avg)) %>%
      rename(
        diff = all_of(diff),
        id = all_of(id),
        Condition = all_of(condition),
        avg = all_of(avg)
      )
  } else{
    df_plt = data %>%
      select(all_of(diff),
             all_of(id),
             all_of(avg)) %>%
      rename(
        diff = all_of(diff),
        id = all_of(id),
        avg = all_of(avg)
      )
  }
  lm_mod = list(call = list(formula = as.formula(df_plt$diff~df_plt$avg+df_plt$id)))
  mc$lm_mod = lm_mod

  structure(list(loa = df_loa,
                 var_comp = var_comp,
                 h0_test = rej_text,
                 lmer = res_lmer,
                 agree.level = agree.level,
                 conf.level = conf.level,
                 type = type,
                 call = mc),
            class = "loa_mixed_bs")
}
