#' Limits of Agreement with Random Effects using lme4
#' @description This function allows for the calculation of bootstrapped limits of agreement when there are multiple observations per subject.
#' @param data A data frame containing the variables within the model.
#' @param diff Column name of the data frame that includes the difference between the 2 measurements of interest.
#' @param avg Column name of the data frame that includes the difference between the 2 measurements of interest.
#' @param condition Column name indicating different conditions subjects were tested under. This can be left missing if there are no differing conditions to be tested.
#' @param id Column name indicating the subject/participant identifier
#' @param conf.level The confidence level required. Default is 95\%.
#' @param agree.level The agreement level required. Default is 95\%.
#' @param replicates 	The number of bootstrap replicates. Passed on to the boot function. Default is 999.
#' @param type A character string representing the type of bootstrap confidence intervals. Only "norm", "basic", and "perc" currently supported. Bias-corrected and accelerated, bca, is the default. See ?boot::boot.ci for more details.
#' @param prop_bias Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the limits of agreement calculations.
#' @return Returns single list with the results of the agreement analysis.
#'
#' \describe{
#'   \item{\code{"var_comp"}}{Table of variance components}
#'   \item{\code{"loa"}}{A data frame of the limits of agreement including the average difference between the two sets of measurements, the standard deviation of the difference between the two sets of measurements and the lower and upper confidence limits of the difference between the two sets of measurements.}
#'   \item{\code{"call"}}{The matched call.}
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

loa_lmer = function(diff,
                    avg,
                    condition = NULL,
                    id,
                    data,
                    type = "perc",
                    conf.level = .95,
                    agree.level = .95,
                    replicates = 999,
                    prop_bias = FALSE
                    ){
  if (conf.level >= 1 || conf.level <= 0) {
    stop("conf.level must be a value between 0 and 1")
  }
  if(is.null(condition) || missing(condition)){
    condition = 1
    df = data[c(diff,avg,id)]
    colnames(df) = c("diff", "avg", "id")
  } else {
    df = data[c(diff,avg,id,condition)]
    colnames(df) = c("diff", "avg", "id", "condition")
  }

  if (agree.level >= 1 || agree.level <= 0) {
    stop("agree.level must be a value between 0 and 1")
  }

  avg_vals = c(min(df$avg, na.rm = TRUE),
               median(df$avg, na.rm = TRUE),
               max(df$avg, na.rm = TRUE))

  if(is.null(condition) || condition == 1){
    if (prop_bias == FALSE) {
      formula1 = as.formula("diff ~ 1 +(1| id )")
      newdat = expand.grid(1) %>%
        as.data.frame() %>%
        rename(condition = Var1)
      newdat2 = expand.grid(c("Bias", "Lower LoA", "Upper LoA")) %>%
        as.data.frame() %>%
        rename(value = Var1) %>%
        select(value)
    } else {
      formula1 = as.formula("diff~avg+(1| id )")
      newdat = expand.grid(avg_vals) %>%
        as.data.frame() %>%
        rename(avg = Var1)
      newdat2 = expand.grid(avg_vals, c("Bias", "Lower LoA", "Upper LoA")) %>%
        as.data.frame() %>%
        rename(avg = Var1,
               value = Var2) %>%
        select(value, avg)
    }
  } else{
    if (prop_bias == FALSE) {
      formula1 = as.formula("diff ~ condition +(1| id )")
      newdat = expand.grid(unique(df$condition)) %>%
        as.data.frame() %>%
        rename(condition = Var1)
      newdat2 = expand.grid(unique(df$condition),c("Bias", "Lower LoA", "Upper LoA")) %>%
        as.data.frame() %>%
        rename(condition = Var1,
               value = Var2) %>%
        select(value, condition)
    } else {
      formula1 = as.formula("diff~avg+condition+(1| id )")
      newdat = expand.grid(avg_vals, unique(df$condition)) %>%
        as.data.frame() %>%
        rename(condition = Var2,
               avg = Var1)
      newdat2 = expand.grid(avg_vals, unique(df$condition),c("Bias", "Lower LoA", "Upper LoA")) %>%
        as.data.frame() %>%
        rename(condition = Var2,
               avg = Var1,
               value = Var3) %>%
        select(value, avg, condition)
    }
  }

  res_lmer = lmer(
    formula = formula1,
    data = df,
    weights = NULL,
    subset = NULL,
    offset = NULL,
    na.action = na.omit
  )

  #boot_sd <- function(.) {
  #  c(sd_within = sigma(.),
  #    sd_between = sqrt(unlist(VarCorr(.))),
  #    sd_total = sigma(.) + sqrt(unlist(VarCorr(.))))
  #}

  boot_loa <- function(.) {
    c(bias = pred_bias(.,newdata = newdat),
      lower = pred_lloa(.,newdata = newdat, agree.level = agree.level),
      upper = pred_uloa(.,newdata = newdat, agree.level = agree.level))
  }

  boo1_tab = data.frame(
    term = c("SD within", "SD between", "SD total"),
    estimate = c(sigma(res_lmer),
                 sqrt(unlist(VarCorr(res_lmer))),
                 sqrt(sigma(res_lmer)^2 + (unlist(VarCorr(res_lmer)))))
  )
  #boo1 <- bootMer(res_lmer, boot_sd, nsim = replicates,
  #                type = "parametric", use.u = FALSE)
  #boo1_tab = tidy_boot(boo1,
  #                     conf.int = TRUE,
  #                     conf.level = conf.level,
  #                     conf.method = type) %>%
  #  mutate(term = c("SD within", "SD between", "SD total")) %>%
  #  rename(estimate = statistic,
  #         se = std.error,
  #         lower.ci = conf.low,
  #         upper.ci = conf.high)
  boo2 <- bootMer(res_lmer, boot_loa, nsim = replicates,
                  type = "parametric", use.u = FALSE)
  boo2_tab = tidy_boot(boo2,
                       conf.int = TRUE,
                       conf.level = conf.level,
                       conf.method = type) %>%
    bind_cols(newdat2 ,.) %>%
    select(-term) %>%
    rename(term = value,
           estimate = statistic,
           se = std.error,
           lower.ci = conf.low,
           upper.ci = conf.high)

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
        condition = all_of(condition),
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
  if(condition == 1){
    lm_mod = list(call = list(formula = as.formula(df_plt$diff~df_plt$avg+df_plt$id)))
  } else {
    lm_mod = list(call = list(formula = as.formula(df_plt$diff~df_plt$avg+df_plt$id+df_plt$condition)))
  }

  mc$lm_mod = lm_mod
  mc$condition = condition
  mc$agree.level = agree.level
  mc$conf.level = conf.level
  mc$prop_bias = prop_bias
  mc$type = type

  structure(list(loa = boo2_tab,
                 var_comp = boo1_tab,
                 model = res_lmer,
                 call = mc),
            class = "loa_mermod")
}
