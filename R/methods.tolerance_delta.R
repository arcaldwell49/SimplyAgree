#' Methods for tolerance_delta objects
#'
#' Methods defined for objects returned from the tolerance_delta function(s).
#'
#' @param x object of class \code{tolerance_delta} as returned from a agreement_limit function.
#' @param delta The maximal allowable difference.
#' @param digits The number of digits to print.
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{tolerance_delta_gls}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the tolerance limits.}
#'   \item{\code{plot}}{Returns a plot of the tolerance limits.}
#'   \item{\code{check}}{Returns plots testing the assumptions of the model. P-values for the normality and heteroskedascity tests are provided as captions to the plot.}
#' }
#'
#' @name tolerance_delta-methods


### methods for tolerance_delta objects

#' @rdname tolerance_delta-methods
#' @method print tolerance_delta
#' @export

print.tolerance_delta <- function(x,
                      digits = 4,
                      ...){

  df_tolerance_delta = x$limits
  if(is.null(df_tolerance_delta$lower.CL)){
    warning(toString(colnames(df_tolerance_delta)))
  }
  call2 = x$call
  pr_table = df_tolerance_delta %>%
    select(avg,
           condition,
           bias,
           lower.CL,
           upper.CL,
           lower.PL,
           upper.PL,
           lower.TL,
           upper.TL)
  if(call2$log_tf){
    pr_table = pr_table %>%
      mutate_at(c(
        "bias",
        "lower.CL",
        "upper.CL",
        "lower.PL",
        "upper.PL",
        "lower.TL",
        "upper.TL"
      ),exp)

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
      `Prediction Interval` = paste0(
        "[",
        round(lower.PL,digits = digits),
        ", ",
        round(upper.PL,digits = digits),
        "]"
      ),
      `Tolerance Limits` = paste0(
        "[",
        round(lower.TL,digits = digits),
        ", ",
        round(upper.TL,digits = digits),
        "]"
      )
    ) %>%
    rename(
      `Average of Both Methods` = avg,
      Condition = condition,
      Bias = bias
    ) %>%
    select(Condition,
           `Average of Both Methods`,
           Bias,
           `Bias CI`,
           `Prediction Interval`,
           `Tolerance Limits`)
  if(call2$prop_bias){
    if(is.null(call2$condition)){
      pr_table3 = pr_table2[,c(
        "Average of Both Methods",
        "Bias",
        "Bias CI",
        "Prediction Interval",
        "Tolerance Limits"
      )]
    } else {
      pr_table3 = pr_table2[,c(
        "Condition",
        "Average of Both Methods",
        "Bias",
        "Bias CI",
        "Prediction Interval",
        "Tolerance Limits"
      )]
    }

  } else{
    if(!is.null(call2$condition)){
    pr_table3 = pr_table2[,c(
      "Condition",
      "Bias",
      "Bias CI",
      "Prediction Interval",
      "Tolerance Limits"
    )]
    } else {
      pr_table3 = pr_table2[,c(
        "Bias",
        "Bias CI",
        "Prediction Interval",
        "Tolerance Limits"
      )]
    }
  }

  title1 = "Agreement between Measures (Difference: x-y)"
  subtitle1 = paste0(
    x$call$pred_level*100,
    "% Prediction Interval with ",
    x$call$tol_level*100,
    "% Tolerance Limits"
  )

  if(call2$log_tf){
    title1 = "Agreement between Measures (Ratio: x/y)"
  }
  #var_print = switch(ifelse(call2$log_tf,"log","norm"),
  #                   "log" = paste0(
  #                     "Coefficient of Variation (%) = ",
  #                     round((exp(x$limits$SEP[1])-1)*100,digits=digits)
  #                   ),
  #                   "norm" =  paste0(
  #                     "Standard Error of Prediction = ",
  #                     round(x$limits$SEP[1],digits=digits)
  #                   ))



  cat(title1, sep = "")
  cat("\n")
  cat(subtitle1, sep = "")
  cat("\n")
  cat("\n")
  print(pr_table3, digits = digits, row.names = FALSE)
  cat("\n")
  #cat(var_print, sep = "")
  cat("\n")

}

#' @rdname tolerance_delta-methods
#' @method plot tolerance_delta
#' @param geom String naming the type of geometry to display the data points. Default is "geom_point". Other options include: "geom_bin2d", "geom_density_2d", "geom_density_2d_filled", and "stat_density_2d".
#' @import ggplot2
#' @export

plot.tolerance_delta <- function(x,
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

  call2 = x$call
  df = model.frame(x$call$lm_mod)
  colnames(df) = c("y","x","id","mean","delta","condition","time")
  df_loa = x$limits
  if(x$call$log){
    df_loa = df_loa %>%
      mutate_at(
        c(
          "bias",
          "lower.CL",
          "upper.CL",
          "lower.PL",
          "upper.PL",
          "lower.TL",
          "upper.TL"
        )
      )
    df$delta = exp(df$delta)
  }
  scalemin = min(c(min(df$x, na.rm = TRUE),min(df$y, na.rm = TRUE)))
  scalemax = max(c(max(df$x, na.rm = TRUE),max(df$y, na.rm = TRUE)))
  pd2 = position_dodge2(.03*(scalemax-scalemin))
  x_lab = "x"
  x_name = "x"
  y_name = "y"
  y_lab = "y"
  df_loa2 = data.frame(
    text = factor(c(rep("Upper Limit",nrow(df_loa)),
                    rep("Bias",nrow(df_loa)),
                    rep("Lower Limit",nrow(df_loa))),
                  levels = c("Upper Limit", "Bias", "Lower Limit"),
                  ordered  = TRUE),
    estimate = c(df_loa$upper.PL, df_loa$bias, df_loa$lower.PL),
    lower.ci = c(df_loa$upper.PL, df_loa$lower.CL, df_loa$lower.TL),
    upper.ci = c(df_loa$upper.TL, df_loa$upper.CL, df_loa$lower.PL)
  )
  df_loa2$x = df_loa$avg

  #df_loa2$x = scalemin

  pred.level = get_call(x$call$pred_level)
  tol.level = get_call(x$call$tol_plevel)
  #confq = qnorm(1 - (1 - get_call(x$call$conf.level)) / 2)
  delta = delta

  if(geom == "geom_point"){
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_point(na.rm = TRUE)
  }  else if(geom == "geom_bin2d") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_bin2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_density_2d(na.rm = TRUE)
  } else if(geom == "geom_density_2d_filled") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      geom_density_2d_filled(na.rm = TRUE,
                             alpha = 0.5,
                             contour_var = "ndensity")
  } else if(geom == "stat_density_2d") {
    bland_alt.plot = ggplot(df,
                            aes(x = mean, y = delta)) +
      stat_density_2d(na.rm = TRUE,
                      geom = "polygon",
                      contour = TRUE,
                      aes(fill = after_stat(level)),
                      contour_var = "ndensity",
                      colour = "black",) +
      scale_fill_distiller(palette = "Blues", direction = 1)
  }  else {
    stop("geom option not supported")
  }

  cap1 = paste0("Prediction Interval = ",
                pred.level * 100,
                "% \n",
                "Tolerance Limits = ",
                tol.level * 100,
                "%")
  if(call2$prop_bias){


  bland_alt.plot = bland_alt.plot +
    geom_ribbon(inherit.aes = FALSE,
                alpha = .2,
                data = df_loa2,
                aes(y=estimate,
                    ymin = lower.ci,
                    ymax = upper.ci,
                    x=x,
                    fill=text)) +
    geom_line(inherit.aes = FALSE,
              data = df_loa2,
              aes(y=estimate,
                  x=x,
                  color=text))
  } else {
    df_loa2$x = scalemin
    bland_alt.plot = bland_alt.plot +
      geom_pointrange(data = df_loa2,
                      aes(
                        x = x,
                        y = estimate,
                        ymin = lower.ci,
                        ymax = upper.ci,
                        color = text),
                      #width = .03*(scalemax-scalemin),
                      position = pd2,
                      inherit.aes = FALSE)
  }

  bland_alt.plot = bland_alt.plot  +
    scale_color_viridis_d(option = "C", end = .8) +
    scale_fill_viridis_d(option = "C", end = .8) +
    labs(x = paste0("Average of ", x_name ," & ", y_name),
         y = ifelse(call2$log,
                    paste0("Ratio of Methods (x/y)"),
                    paste0("Difference between Methods (x - y)")),
         caption = cap1,
         guides = "") +
    theme_bw() +
    theme(legend.position = "left",
          legend.title = element_blank())
  if(!is.null(delta)) {

    if(length(delta) == 1){
      df_delta = data.frame(y1 = c(delta, -1*delta))
    } else{
      df_delta = data.frame(y1 = c(delta[1], delta[2]))
    }
    bland_alt.plot = bland_alt.plot +
      geom_hline(data = df_delta,
                 #inherit.aes = FALSE,
                 aes(yintercept = y1),
                 linetype = 2) +
      scale_y_continuous(sec.axis = dup_axis(
        breaks = c(delta, -1*delta),
        name = "Maximal Allowable Difference"))
  }



}

#' @rdname tolerance_delta-methods
#' @method check tolerance_delta
#' @importFrom stats residuals lm na.omit pchisq shapiro.test ks.test rstudent df.residual anova rstandard sigma resid
#' @export

check.tolerance_delta <- function(x) {


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
