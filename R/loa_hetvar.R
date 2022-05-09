#' Limits of Agreement with Random Effects with Heterogenous Variance
#' @description This function allows for the calculation of bootstrapped limits of agreement when there are multiple observations per subject and the variance is heterogenous.
#' @param data A data frame containing the variables within the model.
#' @param diff Column name of the data frame that includes the difference between the 2 measurements of interest.
#' @param avg Column name of the data frame that includes the difference between the 2 measurements of interest.
#' @param condition Column name indicating different conditions subjects were tested under. This can be left missing if there are no differing conditions to be tested.
#' @param id Column name indicating the subject/participant identifier
#' @param conf.level The confidence level required. Default is 95\%.
#' @param agree.level The agreement level required. Default is 95\%.
#' @param replicates 	The number of bootstrap replicates. Passed on to the boot function. Default is 999.
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
#' @importFrom purrr map map_df keep
#' @import nlme
#' @import ggplot2
#' @import boot
#' @import emmeans
#' @export
#'

loa_hetvar = function(diff,
                    avg,
                    condition,
                    id,
                    data,
                    conf.level = .95,
                    agree.level = .95,
                    replicates = 999,
                    prop_bias = FALSE
){
  if (conf.level >= 1 || conf.level <= 0) {
    stop("conf.level must be a value between 0 and 1")
  }
  df = data[c(diff,avg,id,condition)]
  colnames(df) = c("diff", "avg", "id", "condition")


  if (agree.level >= 1 || agree.level <= 0) {
    stop("agree.level must be a value between 0 and 1")
  }

  specs1 = c("condition")
  avg_vals = c(min(df$avg, na.rm = TRUE),
               median(df$avg, na.rm = TRUE),
               max(df$avg, na.rm = TRUE))

  if (prop_bias == FALSE) {
    formula1 = as.formula("diff ~ condition")
    newdat = expand.grid(unique(df$condition)) %>%
      as.data.frame() %>%
      rename(condition = Var1)
    newdat2 = expand.grid(unique(df$condition), c("Bias", "Lower LoA", "Upper LoA")) %>%
      as.data.frame() %>%
      rename(condition = Var1,
             value = Var2) %>%
      select(value, condition)
  } else {
    formula1 = as.formula("diff~avg+condition")
    newdat = expand.grid(avg_vals, unique(df$condition)) %>%
      as.data.frame() %>%
      rename(condition = Var2,
             avg = Var1)
    newdat2 = expand.grid(avg_vals,
                          unique(df$condition),
                          c("Bias", "Lower LoA", "Upper LoA")) %>%
      as.data.frame() %>%
      rename(condition = Var2,
             avg = Var1,
             value = Var3) %>%
      select(value, avg, condition)
  }


  res_lmer = lme(
    fixed = all_of(formula1),
    data = df,
    random = ~1|id,
    weights = varIdent(form = ~ 1 | factor(condition)),
    na.action = na.omit
  )

  ystar <- para_boot1(res_lmer, B=replicates)
  #ystar <- lmeresampler::parametric_bootstrap(res3,B=100, .refit = FALSE)
  refits <-
    purrr::map(ystar, function(y)
      update_mod(model = res_lmer,
                 new.y = y))
  #test = refits[class(refits) == "lme"]
  refits  = purrr::keep(refits, function(x) class(x)== "lme" )
  vals <- refits %>%
    purrr::map(para_boot2,
               specs1 = specs1,
               at_list = avg_vals,
               prop_bias = prop_bias)

  df_boot = bind_rows(vals, .id = "nboot")

  var_comp1 = res_lmer$modelStruct$varStruct %>%
    coef(unconstrained = FALSE, allCoef = TRUE) %>%
    enframe(name = "condition", value = "structure") %>%
    mutate(sigma = res_lmer$sigma) %>%
    mutate(sd_within = sigma * structure) %>%
    mutate(sd_between = as.numeric(VarCorr(res_lmer)[1,2])) %>%
    mutate(sd_total = sqrt(sd_within^2 + sd_between^2)) %>%
    select(condition, structure, sd_within, sd_between, sd_total)
  if(prop_bias) {
    emm_tab = emmeans(res3,
                      specs=specs1,
                      at=at_list) %>%
      as.data.frame()
    colnames(emm_tab) = c("avg", "condition", "mean", "se", "df", "lower.CL", "upper.CL")
    emm_tab$avg = avg_vals
    emm_tab = emm_tab %>%
      select(avg, condition, mean) %>%
      merge(var_comp1) %>%
      mutate(
        low = mean - agree.lim * sd_total,
        high = mean + agree.lim * sd_total)
  } else {
    emm_tab = emmeans(res3,
                      specs=specs1) %>%
      as.data.frame()
    colnames(emm_tab) = c("condition", "mean", "se", "df", "lower.CL", "upper.CL")

    emm_tab = emm_tab %>%
      select(condition, mean) %>%
      merge(var_comp1) %>%
      mutate(low = mean - agree.lim * sd_total,
             high = mean + agree.lim * sd_total)
  }

  lconf = (1 - conf.level)/2
  uconf = 1-(1 - conf.level)/2

  if(prop_bias){
    df_loa = df_boot %>%
      select(nboot, avg, condition, mean, low, high)

    df_loa_bias = df_loa %>%
      group_by(avg, condition) %>%
      summarize(boot_est = quantile(mean, .5, na.rm = TRUE),
                se = sd(mean, na.rm = TRUE),
                lower.ci = quantile(mean, lconf, na.rm = TRUE),
                upper.ci = quantile(mean, uconf, na.rm = TRUE),
                .groups = 'drop')  %>%
      mutate(term = "Bias") %>%
      merge(x= ., y = emm_tab %>% select(condition, mean),
            by = c("condition")) %>%
      rename(estimate = mean) %>%
      mutate(bias = estimate - boot_est) %>%
      select(term,
             avg,
             condition,
             estimate,
             bias,
             se,
             lower.ci,
             upper.ci)

    df_loa_low = df_loa %>%
      group_by(avg, condition) %>%
      summarize(
        boot_est = quantile(low, .5, na.rm = TRUE),
        se = sd(low, na.rm = TRUE),
        lower.ci = quantile(low, lconf, na.rm = TRUE),
        upper.ci = quantile(low, uconf, na.rm = TRUE),
        .groups = 'drop')%>%
    mutate(term = "Lower LoA") %>%
      merge(x= ., y = emm_tab %>% select(condition,low),
            by = c("condition"))%>%
      rename(estimate = low) %>%
      mutate(bias = estimate - boot_est) %>%
      select(term,
             condition,
             estimate,
             bias,
             se,
             lower.ci,
             upper.ci)

    df_loa_hi = df_loa %>%
      group_by(avg, condition) %>%
      summarize(
        boot_est = quantile(high, .5, na.rm = TRUE),
        se = sd(high, na.rm = TRUE),
        lower.ci = quantile(high, lconf, na.rm = TRUE),
        upper.ci = quantile(high, uconf, na.rm = TRUE),
        .groups = 'drop')%>%
      mutate(term = "Upper LoA")%>%
      merge(x= ., y = emm_tab %>% select(condition,high),
            by = c("condition")) %>%
      rename(estimate = high) %>%
      mutate(bias = estimate - boot_est) %>%
      select(term,
             condition,
             estimate,
             bias,
             se,
             lower.ci,
             upper.ci)

    df_loa_all = bind_rows(df_loa_bias,
                           df_loa_low,
                           df_loa_hi) %>%
      select(avg, condition, value, est, boot_est, lower.ci, upper.ci) %>%
      arrange(avg, condition)
  } else {
    df_loa = df_boot %>%
      select(nboot, condition, mean, low, high)

    df_loa_bias = df_loa %>%
      group_by( condition) %>%
      summarize(boot_est = quantile(mean, .5, na.rm = TRUE),
                se = sd(mean, na.rm = TRUE),
                lower.ci = quantile(mean, lconf, na.rm = TRUE),
                upper.ci = quantile(mean, uconf, na.rm = TRUE),
                .groups = 'drop') %>%
      mutate(term = "Bias") %>%
      merge(x= ., y = emm_tab %>% select(condition, mean),
            by = c("condition")) %>%
      rename(estimate = mean) %>%
      mutate(bias = estimate - boot_est) %>%
      select(term,
             condition,
             estimate,
             bias,
             se,
             lower.ci,
             upper.ci)

    df_loa_low = df_loa %>%
      group_by(condition) %>%
      summarize(
        boot_est = quantile(low, .5, na.rm = TRUE),
        se = sd(low, na.rm = TRUE),
        lower.ci = quantile(low, lconf, na.rm = TRUE),
        upper.ci = quantile(low, uconf, na.rm = TRUE),
        .groups = 'drop')%>%
      mutate(term = "Lower LoA") %>%
      merge(x= ., y = emm_tab %>% select(condition,low),
            by = c("condition"))%>%
      rename(estimate = low) %>%
      mutate(bias = estimate - boot_est) %>%
      select(term,
             condition,
             estimate,
             bias,
             se,
             lower.ci,
             upper.ci)

    df_loa_hi = df_loa %>%
      group_by(condition) %>%
      summarize(
        boot_est = quantile(high, .5, na.rm = TRUE),
        se = sd(high, na.rm =TRUE),
        lower.ci = quantile(high, lconf, na.rm = TRUE),
        upper.ci = quantile(high, uconf, na.rm = TRUE),
        .groups = 'drop')%>%
      mutate(term = "Upper LoA")%>%
      merge(x= ., y = emm_tab %>% select(condition,high),
            by = c("condition")) %>%
      rename(estimate = high) %>%
      mutate(bias = estimate - boot_est) %>%
      select(term,
             condition,
             estimate,
             bias,
             se,
             lower.ci,
             upper.ci)

    df_loa_all = bind_rows(df_loa_bias,
                           df_loa_low,
                           df_loa_hi) %>%
      arrange(condition)
  }

  mc = match.call()

  mc$agree.level = agree.level
  mc$conf.level = conf.level

  df_plt = data %>%
    select(all_of(diff),
           all_of(id),
           all_of(avg),
           all_of(condition)) %>%
    rename(diff = all_of(diff),
           id = all_of(id),
           avg = all_of(avg),
           condition = all_of(condition))

  lm_mod = list(call = list(
    formula = as.formula(df_plt$diff ~ df_plt$avg + df_plt$id + df_plt$condition)
  ))


  mc$lm_mod = lm_mod
  mc$condition = condition
  mc$agree.level = agree.level
  mc$conf.level = conf.level
  mc$prop_bias = prop_bias
  #mc$type = type

  structure(list(loa = df_loa_all,
                 var_comp = var_comp1,
                 model = res_lmer,
                 call = mc),
            class = "loa_mermod")
}
