#' Methods for loa objects
#'
#' Methods defined for objects returned from the agreement_limit function.
#'
#' @param x object of class \code{loa} as returned from a agreement_limit function.
#' @param delta The maximal allowable difference.
#' @param digits The number of digits to print.
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{agreement_limit}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the Limits of Agreement.}
#'   \item{\code{plot}}{Returns a plot of the limits of agreement.}
#'   \item{\code{check}}{Returns plots testing the assumptions of a Bland-Altman analysis. P-values for the normality and heteroskedascity tests are provided as captions to the plot.}
#' }
#'
#' @name loa-methods


### methods for loa objects

#' @rdname loa-methods
#' @method print loa
#' @export

print.loa <- function(x,
                      digits = 4,
                      ...){

  df_loa = x$loa

  call2 = x$call
  pr_table = df_loa %>%
    select(avg,
           bias,
           lower.CL,
           upper.CL,
           lower_loa,
           upper_loa,
           lower_loa_ci,
           upper_loa_ci)
  if(call2$log_tf){
    pr_table = pr_table %>% mutate_if(is.numeric,exp)
    if(call2$prop_bias){
      pr_table = pr_table %>%
        mutate(avg = log(avg))
    }
  }

  pr_table2 = pr_table %>%
    mutate(
      `Bias CI` = paste0(
        "[",
        round(lower.CL, digits = digits),
        ", ",
        round(upper.CL, digits = digits),
        "]"
      ),
      `LoA CI` = paste0(
        "[",
        round(lower_loa_ci,digits = digits),
        ", ",
        round(upper_loa_ci,digits = digits),
        "]"
      )
    ) %>%
    rename(
      `Average of Both Methods` = avg,
      Bias = bias,
      `Lower LoA` = lower_loa,
      `Upper LoA` = upper_loa
    )
  if(call2$prop_bias){
    pr_table3 = pr_table2[,c(
      "Average of Both Methods",
      "Bias",
      "Bias CI",
      "Lower LoA",
      "Upper LoA",
      "LoA CI"
    )]
  } else{
    pr_table3 = pr_table2[,c(
      "Bias",
      "Bias CI",
      "Lower LoA",
      "Upper LoA",
      "LoA CI"
    )]
  }

  title1 = paste0(
    ifelse(x$call$loa_calc == "mover",
           "MOVER",
           "Bland-Altman"),
    " Limits of Agreement (LoA)"
  )
  subtitle1 = paste0(
    x$call$agree.level*100,
    "% LoA @ ",
    x$call$alpha*100,
    "% Alpha-Level"
  )
  dat_type = switch(
    call2$data_type,
    simple = "Independent Data Points",
    nest = "Nested Data",
    reps = "Data with Replicates"
  )
  if(call2$log_tf){
    dat_type = paste0("Log-transformed ", dat_type)
  }
  var_print = switch(ifelse(call2$log_tf,"log","norm"),
                     "log" = paste0(
                       "Coefficient of Variation (%) = ",
                       round((exp(x$loa$sd_delta[1])-1)*100,digits=digits)
                     ),
                     "norm" =  paste0(
                       "SD of Differences = ",
                       round(x$loa$sd_delta[1],digits=digits)
                     ))



  cat(title1, sep = "")
  cat("\n")
  cat(subtitle1, sep = "")
  cat("\n")
  cat(dat_type, sep = "")
  cat("\n")
  cat("\n")
  print(pr_table3, digits = digits, row.names = FALSE)
  cat("\n")
  cat(var_print, sep = "")
  cat("\n")
  if(call2$data_type == "reps"){
    var_print2 = paste0(
      "Within-Subject Variances of X & Y = ",
      round(x$loa$within_variance_x[1],digits=digits),
      " & ",
      round(x$loa$within_variance_y[1],digits=digits)
    )
  }

}

#' @rdname loa-methods
#' @method plot loa
#' @param geom String naming the type of geometry to display the data points. Default is "geom_point". Other options include: "geom_bin2d", "geom_density_2d", "geom_density_2d_filled", and "stat_density_2d".
#' @import ggplot2
#' @export

plot.loa <- function(x,
                     geom = c(
                       "geom_point",
                       "geom_bin2d",
                       "geom_density_2d",
                       "geom_density_2d_filled",
                       "stat_density_2d"
                     ),
                     delta = NULL,
                     ...) {

  geom = match.arg(geom)


  #return(x$bland_alt.plot)
  if (get_call(x$call$prop_bias) != TRUE) {
    simple_loa_plot(x,
                   geom,
                   delta)
  } else {
    bias_loa_plot(x,
                  geom,
                  delta)
  }


}

#' @rdname loa-methods
#' @method check loa
#' @importFrom stats residuals lm na.omit pchisq shapiro.test ks.test rstudent df.residual anova rstandard sigma resid
#' @export

check.loa <- function(x) {


  call2 = x$call
  df = model.frame(x$call$lm_mod)
  colnames(df) = c("y","x","id","mean","delta")

  if(x$call$prop_bias == TRUE){
    form_lm1 = as.formula(delta ~ mean)
    form_lmer1 = as.formula(delta ~ mean + (1|id))
  } else {
    form_lm1 = as.formula(delta ~ 1)
    form_lmer1 = as.formula(delta ~ 1 + (1|id))
  }


  dat = df
  ## Heteroskedasticity -------
  mod_check = if (call2$data_type != "simple") {
    lme4::lmer(data = dat,
               form_lmer1)
  } else {
    lm(data = dat,
       form_lm1)
  }

  stan_res = residuals(mod_check, type = "pearson")
  df_het = df.residual(mod_check)
  sum_het_res = sum(!is.na(stan_res))
  sigma_het = sigma(mod_check)

  s_sq = df_het * sigma_het^2 / sum_het_res

  u_het = stan_res^2 / s_sq

  mod <- lm(u_het ~ na.omit(dat$mean))

  SS <- anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS / 2
  ### Breusch-Pagan Test
  p_val_het <- pchisq(Chisq, df = 1, lower.tail = FALSE)

  rstan_het =  residuals(mod_check, scaled = TRUE)
  dat_het <- data.frame(
    x = na.omit(dat$mean),
    y = na.omit(sqrt(abs(rstan_het)))
  )
  p_het = plot_het(dat_het) +
    labs(caption = paste0("Heteroskedasticity", " \n",
                          "Breusch-Pagan Test: p = ",
                          signif(p_val_het,4))) +
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )


  ## Normality ------------

  mod_res = residuals(mod_check)
  if(length(mod_res) < 5000){
    norm_test = shapiro.test(mod_res)
    norm_text = "Shapiro-Wilk Test"
  } else {
    norm_test = ks.test(mod_res, y = "pnorm",
                        alternative = "two.sided")
    norm_text = "Kolmogorov-Smirnov Test"
  }

  rstan_norm = sort(rstudent(mod_check), na.last = NA)
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

  # Proportional Bias -----
  if(call2$data_type == "simple"){
    mod2 = lm(delta ~ mean,
              data = dat)
    aov2 = as.data.frame(anova(mod_check, mod2))
    colnames(aov2) = c("df1","RSS","df2","SS","f","p")
    lin_pval = aov2$p[2]
  } else {
    mod2 = lmer(data = dat,
                delta ~ mean + (1 | id))
    aov2 = suppressMessages(as.data.frame(anova(mod_check, mod2)))
    colnames(aov2) = c("npar","AIC","BIC","log_lik","dev","chisq","df","p")
    lin_pval = aov2$p[2]
  }

  dat2 = data.frame(resid = residuals(mod_check),
                    mean = na.omit(dat$mean))
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

  #return(list(p_norm = p_norm,
  #            p_het = p_het,
  #            p_bias = p_bias))

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
