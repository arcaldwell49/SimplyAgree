#' Mixed Effects Limits of Agreement
#' @param data A data frame containing the variables within the model.
#' @param diff column name of the data frame that includes the continuous measurement of interest.
#' @param condition column name indicating different conditions subjects were tested under.
#' @param id column name indicating the subject/participant identifier
#' @param plot.xaxis column name indicating what to plot on the x.axis for the Bland-Altman plots. If this argument is missing or set to NULL then no plot will be produced.
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
#' Parker, R. A., Weir, C. J., Rubio, N., Rabinovich, R., Pinnock, H., Hanley, J., McLoughan, L., Drost, E.M., Mantoani, L.C., MacNee, W., & McKinstry, B. (2016). "Application of mixed effects limits of agreement in the presence of multiple sources of variability: exemplar from the comparison of several devices to measure respiratory rate in COPD patients". Plos One, 11(12), e0168321. <https://doi.org/10.1371/journal.pone.0168321>
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
                     conf.level = .95,
                     agree.level = .95,
                     replicates = 1999,
                     type = "bca"){
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
  # need to create method for print and plot
  structure(list(bs_tab = res_tab,
                 plot = p,
                 agree.level = agree.level,
                 conf.level = conf.level,
                 type = type),
            class = "loa_mixed_bs")
}
