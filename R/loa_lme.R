#' Limits of Agreement with Linear Mixed Effects
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function allows for the calculation of (parametric) bootstrapped limits of agreement when there are multiple observations per subject.
#' The package author recommends using `tolerance_limit` as an alternative to this function.
#'
#' @param data A data frame containing the variables within the model.
#' @param diff Column name of the data frame that includes the difference between the 2 measurements of interest.
#' @param avg Column name of the data frame that includes the average of the 2 measurements of interest.
#' @param condition Column name indicating different conditions subjects were tested under. This can be left missing if there are no differing conditions to be tested.
#' @param id Column name indicating the subject/participant identifier
#' @param conf.level The confidence level required. Default is 95%.
#' @param agree.level The agreement level required. Default is 95%.
#' @param replicates 	The number of bootstrap replicates. Passed on to the boot function. Default is 999.
#' @param type A character string representing the type of bootstrap confidence intervals. Only "norm", "basic", and "perc" currently supported. Bias-corrected and accelerated, bca, is the default. See ?boot::boot.ci for more details.
#' @param prop_bias Logical indicator (default is FALSE) of whether proportional bias should be considered for the limits of agreement calculations.
#' @param het_var Logical indicator (default is FALSE) of whether to assume homogeneity of variance in each condition.
#' @return Returns single list with the results of the agreement analysis.
#'
#'   - `var_comp`: Table of variance components
#'   - `loa`: A data frame of the limits of agreement including the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements.
#'   - `call`: The matched call.
#'
#' @references
#' Parker, R. A., Weir, C. J., Rubio, N., Rabinovich, R., Pinnock, H., Hanley, J., McLoughan, L., Drost, E.M., Mantoani, L.C., MacNee, W., & McKinstry, B. (2016).
#' "Application of mixed effects limits of agreement in the presence of multiple sources of variability: exemplar from the comparison of several devices to measure respiratory rate in COPD patients".
#' PLOS One, 11(12), e0168321.
#' \doi{10.1371/journal.pone.0168321}
#' @importFrom stats qnorm as.formula na.omit coef df predict rnorm runif
#' @importFrom magrittr %>%
#' @importFrom dplyr select rename
#' @importFrom tidyselect all_of
#' @importFrom purrr map map_df keep
#' @importFrom nlme lme gls
#' @import lme4
#' @import ggplot2
#' @import boot
#' @import emmeans
#' @export
#'

loa_lme = function(diff,
                   avg,
                   condition = NULL,
                   id,
                   data,
                   type = c("perc","norm", "basic"),
                   conf.level = .95,
                   agree.level = .95,
                   replicates = 999,
                   prop_bias = FALSE,
                   het_var = FALSE){
  type = match.arg(type)
  if(is.null(condition) && het_var){
    stop("If het_var TRUE, then condition column must be provided.")
  }
  if(het_var && type != "perc"){
    stop("type must be \"perc\" if het_var TRUE.")
  }
  if(het_var){
    loa_hetvar(
      diff = diff,
      avg = avg,
      condition = condition,
      id = id,
      data = data,
      conf.level = conf.level,
      agree.level = agree.level,
      replicates = replicates,
      prop_bias = prop_bias
    )
  } else{
    loa_lmer(
      diff = diff,
      avg = avg,
      condition = condition,
      id = id,
      data = data,
      type = type,
      conf.level = conf.level,
      agree.level = agree.level,
      replicates = replicates,
      prop_bias = prop_bias
    )
  }
}
