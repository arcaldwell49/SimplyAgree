#' Mixed Effects Limits of Agreement
#' @param data A data frame containing the variables within the model.
#' @param diff column name of the data frame that includes the continuous measurement of interest.
#' @param condition column name indicating different conditions subjects were tested under.
#' @param id column name indicating the subject/participant identifier
#' @param conf.level the confidence level required. Default is 95\%.
#' @param agree.level the agreement level required. Default is 95\%.
#' @param replicates 	the number of bootstrap replicates. Passed on to the boot function. Default is 500.
#' @param type A character string representing the type of bootstrap confidence intervals. Only "norm", "basic", "bca", and "perc" currently supported. Bias-corrected and accelerated, bca, is the default. See ?boot::boot.ci for more details.
#' @return Returns single list with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"vartab"}}{Table of variance components}
#'
#' }
#'@examples
#'\dontrun{
#' df_rec.pre = temps %>%
#' mutate(id_spec = paste0(id,"_",trial_condition)) %>%
#' select(id,id_spec,trec_pre,tod,trial_condition) %>%
#' pivot_wider(id_cols = c(id,id_spec,trial_condition),
#' names_from = tod,alues_from = trec_pre) %>%
#' mutate(diff = PM - AM)
#' }
#' @section References:
#' Parker, R. A., Weir, C. J., Rubio, N., Rabinovich, R., Pinnock, H., Hanley, J., McLoughan, L., Drost, E.M., Mantoani, L.C., MacNee, W., & McKinstry, B. (2016). Application of mixed effects limits of agreement in the presence of multiple sources of variability: exemplar from the comparison of several devices to measure respiratory rate in COPD patients. Plos one, 11(12), e0168321. <https://doi.org/10.1371/journal.pone.0168321>
#' @importFrom stats qnorm as.formula na.omit
#' @import lme4
#' @import ggplot2
#' @import boot
#' @import emmeans
#' @export
#'

loa_mixed = function(diff,
                     condition,
                     id,
                     data,
                     conf.level = .95,
                     agree.level = .95,
                     replicates = 500,
                     type = "bca"){

  boot_index = list(
    bias = 1,
    within_sd = 2,
    between_sd = 3,
    total_sd = 4,
    low_loa = 5,
    upper_loa = 6
  )
  boot_res = boot(
    statistic = loa_bs,
    R = replicates,
    diff = diff,
    condition = condition,
    id = id,
    data = data,
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
              boot_within_sd = boot_within_sd,
              boot_between_sd = boot_between_sd,
              boot_total_sd = boot_total_sd,
              boot_low_loa = boot_low_loa,
              boot_upper_loa = boot_upper_loa)
  res_tab = loa_bstab(bsls = bsls,
                      type = type,
                      conf.level = conf.level)
  # need to create method for print and plot
  structure(list(bs_tab = res_tab,
                 conf.level = conf.level,
                 type = type),
            class = "loa_mixed_bs")
}
