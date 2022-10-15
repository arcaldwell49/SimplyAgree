#' Methods for loa_mermod objects
#'
#' Methods defined for objects returned from the loa_lme.
#'
#' @param x object of class \code{loa_mermod}.
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{loa_mixed}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the Limits of Agreement}
#'   \item{\code{plot}}{Returns a plot of the limits of agreement}
#' }
#'
#' @name loa_mermod-methods


### methods for loa_mermod (created by loa_mixed)

#' @rdname loa_mermod-methods
#' @method print loa_mermod
#' @export

print.loa_mermod <- function(x,...){
  agree = paste0(x$call$agree.level*100)
  conf = paste0(x$call$conf.level*100)
  title = paste0(agree,"% Limits of Agreement with Parametric Bootstrap ", conf, "% Confidence Intervals \n")
  cat(title)
  df = x$loa
  if("avg" %in% colnames(df) && "condition" %in% colnames(df) ){
    df = df %>%
      select(avg, condition, term, estimate, lower.ci, upper.ci)%>%
      rename(Average = avg,
             Condition = condition,
             Measures = term,
             Estimate = estimate,
             `Lower CI` = lower.ci,
             `Upper CI` = upper.ci)
  } else if("avg" %in% colnames(df)){
    df = df %>%
      select(avg, term, estimate, lower.ci, upper.ci)%>%
      rename(Average = avg,
             Measures = term,
             Estimate = estimate,
             `Lower CI` = lower.ci,
             `Upper CI` = upper.ci)
  }else if("condition" %in% colnames(df)){
    df = df %>%
      select(condition, term, estimate, lower.ci, upper.ci)%>%
      rename(Condition = condition,
             Measures = term,
             Estimate = estimate,
             `Lower CI` = lower.ci,
             `Upper CI` = upper.ci)
  } else {
    df = df %>%
      select(term, estimate, lower.ci, upper.ci)%>%
      rename(Measures = term,
             Estimate = estimate,
             `Lower CI` = lower.ci,
             `Upper CI` = upper.ci)
  }
  #colnames(df) = c("Measures", "Estimate", "Lower CI", "Upper CI")
  print(df, digits = 4)
}

#' @rdname loa_mermod-methods
#' @param x_label Label for x-axis.
#' @param y_label Label for y-axis.
#' @param geom String naming the type of geometry to display the data points. Default is "geom_point". Other options include: "geom_bin2d", "geom_density_2d", "geom_density_2d_filled", and "stat_density_2d".
#' @param smooth_method Smoothing method (function) to use, accepts either NULL or a character vector, e.g. "lm", "glm", "gam", "loess" or a function. Default is NULL, which will not include a trend line.
#' @param smooth_se Display confidence interval around smooth?
#' @method plot loa_mermod
#' @import ggplot2
#' @export

plot.loa_mermod <- function(x,
                            x_label = "Average of Both Methods",
                            y_label = "Difference Between Methods",
                            geom = "geom_point",
                            smooth_method = NULL,
                            smooth_se = TRUE,
                            ...){
  if(x$call$prop_bias != TRUE){
    simple_mix_plot(x,
                   x_label,
                   y_label,
                   geom,
                   smooth_method,
                   smooth_se)
  } else {
    bias_mix_plot(x,
                 x_label,
                 y_label,
                 geom,
                 smooth_se)
  }

}


#' @rdname loa_mermod-methods
#' @method check loa_mermod
#' @importFrom stats residuals lm na.omit pchisq shapiro.test ks.test rstudent df.residual anova rstandard sigma resid
#' @importFrom insight get_df get_residuals
#' @importFrom patchwork wrap_plots
#' @export

check.loa_mermod <- function(x) {
  if(x$call$condition == 1){
    df_plt = model.frame(x$call$lm_mod)
    colnames(df_plt) = c("diff", "avg", "id")
    df_loa = x$loa
  } else {
    df_plt = model.frame(x$call$lm_mod)
    colnames(df_plt) = c("diff", "avg", "id", "condition")
    df_loa = x$loa
  }
  res_mod = x$model

  # Heterosckad ------

  stan_res = get_residuals(res_mod, type = "pearson")
  df_het = get_df(res_mod, type = "residual")
  if(length(df_het) > 1) {
    df_het = get_df(res_mod, type = "residual")[1]
  }
  sum_het_res = sum(!is.na(stan_res))
  sigma_het = sigma(res_mod)

  s_sq = df_het * sigma_het^2 / sum_het_res
  u_het = stan_res^2 / s_sq

  mod <- lm(u_het ~ na.omit(df_plt$avg))

  SS <- anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS / 2
  ### Breusch-Pagan Test
  p_val_het <- pchisq(Chisq, df = 1, lower.tail = FALSE)

  rstan_het =  residuals(res_mod, scaled = TRUE)
  dat_het <- data.frame(
    x = na.omit(df_plt$avg),
    y = na.omit(sqrt(abs(rstan_het)))
  )
  p_het = plot_het(dat_het) +
    labs(caption = paste0("Heteroskedasticity", " \n",
                          "Breusch-Pagan Test: p = ",
                          signif(p_val_het,4)))+
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )

  # Normality ------------
  mod_resid = residuals(res_mod)
  if(length(res_mod) < 5000){
    norm_test = shapiro.test(mod_resid)
    norm_text = "Shapiro-Wilk Test"
  } else {
    norm_test = ks.test(mod_resid, y = "pnorm",
                        alternative = "two.sided")
    norm_text = "Kolmogorov-Smirnov Test"
  }

  rstan_norm <- suppressMessages(sort(stats::residuals(res_mod), na.last = NA))
  dat_norm <- na.omit(data.frame(y = rstan_norm))
  p_norm = plot_qq(
    x = dat_norm
  ) +
    labs(caption = paste0("Normality", " \n",
                          norm_text, ": p = ",
                          signif(norm_test$p.value,4))) +
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )

  # Prop Bias ----

  if ("condition" %in% colnames(df_plt)) {
    #form1 = as.formula(diff ~ condition)
    #form2 =  as.formula(diff ~ avg + condition)
    mod1 = lme(
      data = df_plt,
      fixed = diff ~ condition,
      random = ~ 1 | id,
      method = "ML"
    )
    mod2 = lme(
      data = df_plt,
      fixed = diff ~ avg + condition,
      random = ~ 1 | id,
      method = "ML"
    )
  } else {
    #form1 = as.formula(diff ~ 1)
    #form2 =  as.formula(diff ~ avg)
    mod1 = lme(
      data = df_plt,
      fixed = diff ~ 1,
      random = ~ 1 | id,
      method = "ML"
    )
    mod2 = lme(
      data = df_plt,
      fixed = diff ~ avg,
      random = ~ 1 | id,
      method = "ML"
    )
  }

  aov2 = suppressWarnings(as.data.frame(nlme::anova.lme(mod1, mod2)))
  colnames(aov2) = c("call", "model", "df", "aic", "bic", "loglik", "test", "l.ratio", "p.value")
  lin_pval = aov2$p.value[2]

  dat2 = data.frame(resid = residuals(mod1),
                    mean = na.omit(df_plt$avg))
  p_bias = plot_bias(dat2) +
    labs(caption = paste0("Proportional Bias", " \n",
                          "Test for Linear Bias", ": p = ",
                          signif(lin_pval,4))) +
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )

  # All plots ----

  wrap_plots(p_norm, p_het,
             p_bias, ncol = 2) & plot_annotation(
               theme = theme(
                 panel.background = element_rect(fill='transparent'), #transparent panel bg
                 plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
                 panel.grid.major = element_blank(), #remove major gridlines
                 panel.grid.minor = element_blank(), #remove minor gridlines
                 legend.background = element_rect(fill='transparent'), #transparent legend bg
                 legend.box.background = element_rect(fill='transparent') #transparent legend panel
               ))
}


