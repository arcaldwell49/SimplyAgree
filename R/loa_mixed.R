#' @title Mixed Effects Limits of Agreement
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#'`loa_mixed()` is outdated, and for new code we recommend
#' switching to `loa_lme()` or `tolerance_limit`, which are easier to use, have more features,
#' and are still under active development.
#'
#' This function allows for the calculation of bootstrapped limits of agreement when there are multiple observations per subject.
#' @param data A data frame containing the variables within the model.
#' @param diff column name of the data frame that includes the continuous measurement of interest.
#' @param condition column name indicating different conditions subjects were tested under.
#' @param id column name indicating the subject/participant identifier
#' @param plot.xaxis column name indicating what to plot on the x.axis for the Bland-Altman plots. If this argument is missing or set to NULL then no plot will be produced.
#' @param delta The threshold below which methods agree/can be considered equivalent, can be in any units. Equivalence Bound for Agreement.
#' @param conf.level the confidence level required. Default is 95%.
#' @param agree.level the agreement level required. Default is 95%.
#' @param replicates 	the number of bootstrap replicates. Passed on to the boot function. Default is 1999.
#' @param type A character string representing the type of bootstrap confidence intervals. Only "norm", "basic", "bca", and "perc" currently supported. Bias-corrected and accelerated, bca, is the default. See ?boot::boot.ci for more details.
#' @return Returns single list with the results of the agreement analysis.
#'
#'   - `var_comp`: Table of variance components
#'   - `loa`: a data frame of the limits of agreement including the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements.
#'   - `h0_test`: Decision from hypothesis test.
#'   - `bland_alt.plot`: Simple Bland-Altman plot. Red line are the upper and lower bounds for shieh test; grey box is the acceptable limits (delta). If the red lines are within the grey box then the shieh test should indicate 'reject h0', or to reject the null hypothesis that this not acceptable agreement between x & y.
#'   - `conf.level`: Returned as input.
#'   - `agree.level`: Returned as input.
#'
#' @references
#' Parker, R. A., Weir, C. J., Rubio, N., Rabinovich, R., Pinnock, H., Hanley, J., McLoughan, L., Drost, E.M., Mantoani, L.C., MacNee, W., & McKinstry, B. (2016). "Application of mixed effects limits of agreement in the presence of multiple sources of variability: exemplar from the comparison of several devices to measure respiratory rate in COPD patients". Plos One, 11(12), e0168321.
#' \doi{10.1371/journal.pone.0168321}
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
                     condition,
                     id,
                     data,
                     plot.xaxis = NULL,
                     delta,
                     conf.level = .95,
                     agree.level = .95,
                     replicates = 1999,
                     type = "bca"){
  lifecycle::deprecate_warn("0.2.0", "loa_mixed()", "loa_lme()")
  if (conf.level >= 1 || conf.level <= 0) {
    stop("conf.level must be a value between 0 and 1")
  }
  if(is.null(condition) || missing(condition)){
    condition = 1
  }

  if (agree.level >= 1 || agree.level <= 0) {
    stop("agree.level must be a value between 0 and 1")
  }

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

  if(!missing(plot.xaxis) || !is.null(plot.xaxis)){
    if (condition != 1) {
      df_plt = data %>%
        select(all_of(diff),
               all_of(id),
               all_of(condition),
               all_of(plot.xaxis)) %>%
        rename(
          diff = all_of(diff),
          id = all_of(id),
          Condition = all_of(condition),
          X = all_of(plot.xaxis)
        )
    } else{
      df_plt = data %>%
        select(all_of(diff),
               all_of(id),
               all_of(plot.xaxis)) %>%
        rename(
          diff = all_of(diff),
          id = all_of(id),
          X = all_of(plot.xaxis)
        )
    }


    p = ggplot(data=df_plt,
               aes(x=X,
                   y=diff)) + # color = Condition
      # Mean Bias
      geom_hline(yintercept=res_tab$estimate[1], alpha=.75) +
      annotate("rect",
               xmin = -Inf, xmax = Inf,
               ymin = res_tab$lower.ci[1],
               ymax = res_tab$upper.ci[1],
               alpha = .5,
               fill = "gray") +
      # lower limit
      geom_hline(yintercept=res_tab$estimate[2],
                 alpha=.75,
                 linetype="dotdash") +
      annotate("rect",
               xmin = -Inf, xmax = Inf,
               ymin = res_tab$lower.ci[2],
               ymax = res_tab$upper.ci[2],
               alpha = .5,
               fill = "#D55E00") +
      # upper limit
      geom_hline(yintercept=res_tab$estimate[3],
                 alpha=.75,
                 linetype="dotdash") +
      annotate("rect",
               xmin = -Inf, xmax = Inf,
               ymin = res_tab$lower.ci[3],
               ymax = res_tab$upper.ci[3],
               alpha = .5,
               fill = "#D55E00") +
      labs(x = "",
           y = "Difference between Measurements",
           color = "Conditions") +
      theme_bw()

    if(condition == 1){
      p = p + geom_point()
    } else{
      p = p +
        geom_point(aes(color=Condition)) +
        scale_color_viridis_d()
    }

  } else {
    p = NULL
  }

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

  structure(list(loa = df_loa,
                 var_comp = var_comp,
                 h0_test = rej_text,
                 bland_alt.plot = p,
                 agree.level = agree.level,
                 conf.level = conf.level,
                 type = type),
            class = "loa_mixed_bs")
}
