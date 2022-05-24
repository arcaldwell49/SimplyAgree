#' Methods for simple_agree objects
#'
#' Methods defined for objects returned from the agree functions.
#'
#' @param x object of class \code{simple_agree} as returned from a function starting with 'agree'
#' @param type Type of plot to output. Default (1) is Bland-Altman plot while type=2 will produce a line-of-identity plot.
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{agree_test}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the Limits of Agreement}
#'   \item{\code{plot}}{Returns a plot of the limits of agreement (type = 1) or concordance plot (type = 2)}
#'   \item{\code{check}}{Returns 2 plots, p_norm and p_het, testing the assumptions of a Bland-Altman analysis. P-values for the normality and heteroskedascity tests are provided as captions to the plot.}
#' }
#'
#' @name simple_agree-methods


### methods for simple_agree objects

#' @rdname simple_agree-methods
#' @method print simple_agree
#' @export

print.simple_agree <- function(x,...){
  conf_TOST = if(get_call(x$call$TOST)){
    1-(1-get_call(x$call$conf.level))*2 #ifelse(inherits(x$call$conf.level, "name"), get(test1$call$conf.level), test1$call$conf.level)
  } else {
    get_call(x$call$conf.level)
  }
  LOA = x$loa
  colnames(LOA) = c("Estimate", "Lower CI", "Upper CI", "CI Level")
  if(as.character(x$call[1]) == "agree_test" | as.character(x$call[1]) == "SimplyAgree::agree_test") {
  cat("Limit of Agreement = ", x$shieh_test$prop0*100, "%",  sep = "")
  #cat("\n")
  #cat("alpha =", (1-x$call$conf.level), "|", x$call$conf.level*100,"% Confidence Interval")
  cat("\n")
  cat("\n")
  cat("###- Shieh Results -###")
  cat("\n")
  cat("Exact ",100*conf_TOST,"% C.I. "," [",round(x$shieh_test$lower.ci,4),", ",round(x$shieh_test$upper.ci, 4), "]", sep = "")
  cat("\n")
  cat("Hypothesis Test: ",x$shieh_test$h0_test, sep = "")
  cat("\n")
  if(get_call(x$call$prop_bias) == TRUE) {cat("Warning: hypothesis test likely bogus with proportional bias.")}
  cat("\n")
  cat("###- Bland-Altman Limits of Agreement (LoA) -###")
  cat("\n")
  print(LOA, digits = 4)
  cat("\n")
  if(get_call(x$call$prop_bias) == TRUE) {cat("LoA at average of both measures. Please check plot.")
    cat("\n")}
  cat("###- Concordance Correlation Coefficient (CCC) -###")
  cat("\n")
  cat("CCC: ",round(x$ccc.xy$est.ccc,4),", ",100*get_call(x$call$conf.level),
      "% C.I. ","[",round(x$ccc.xy$lower.ci,4),", ",round(x$ccc.xy$upper.ci,4),"]",sep = "")
  cat("\n")
  } else if(as.character(x$call[1]) == "agree_reps" | as.character(x$call[1]) == "SimplyAgree::agree_reps"){
    cat("Limit of Agreement = ", get_call(x$call$agree.level)*100, "%",  sep = "")
    #cat("\n")
    #cat("alpha =", (1-x$call$conf.level), "|", x$call$conf.level*100,"% Confidence Interval")
    cat("\n")
    cat("Replicate Data Points (true value does not vary)")
    cat("\n")
    if(get_call(x$call$prop_bias) == TRUE) {cat("Warning: hypothesis test likely bogus with proportional bias.")}
    cat("\n")
    cat("Hypothesis Test: ",x$h0_test, sep = "")
    cat("\n")
    cat("\n")
    cat("###- Bland-Altman Limits of Agreement (LoA) -###")
    cat("\n")
    print(LOA, digits = 4)
    cat("\n")
    if(get_call(x$call$prop_bias) == TRUE) {cat("LoA at average of both measures. Please check plot.")
      cat("\n")}
    cat("###- Concordance Correlation Coefficient* (CCC) -###")
    cat("\n")
    cat("CCC: ",round(x$ccc.xy$est.ccc,4),", ",100*get_call(x$call$conf.level),
        "% C.I. ","[",round(x$ccc.xy$lower.ci,4),", ",round(x$ccc.xy$upper.ci,4),"]",sep = "")
    cat("\n")
    cat("*Estimated via U-statistics")
    cat("\n")
  } else if(as.character(x$call[1]) == "agree_nest" | as.character(x$call[1]) == "SimplyAgree::agree_nest"){
    cat("Limit of Agreement = ", get_call(x$call$agree.level)*100, "%",  sep = "")
    #cat("\n")
    #cat("alpha =", (1-x$call$conf.level), "|", x$call$conf.level*100,"% Confidence Interval")
    cat("\n")
    cat("Nested Data Points (true value may vary)")
    cat("\n")
    if(get_call(x$call$prop_bias) == TRUE) {cat("Warning: hypothesis test likely bogus with proportional bias.")}
    cat("\n")
    cat("Hypothesis Test: ",x$h0_test, sep = "")
    cat("\n")
    cat("\n")
    cat("###- Bland-Altman Limits of Agreement (LoA) -###")
    cat("\n")
    print(LOA, digits = 4)
    cat("\n")
    if(get_call(x$call$prop_bias) == TRUE) {cat("LoA at average of both measures. Please check plot.")
      cat("\n")}
    cat("###- Concordance Correlation Coefficient (CCC) -###")
    cat("\n")
    cat("CCC: ",round(x$ccc.xy$est.ccc,4),", ",100*get_call(x$call$conf.level),
        "% C.I. ","[",round(x$ccc.xy$lower.ci,4),", ",round(x$ccc.xy$upper.ci,4),"]",sep = "")
    cat("\n")
    cat("*Estimated via U-statistics; may be biased")
    cat("\n")
  } else if(as.character(x$call[1]) == "agree_np" | as.character(x$call[1]) == "SimplyAgree::agree_np"){
    cat("Limit of Agreement = ", get_call(x$call$agree.level)*100, "%",  sep = "")
    #cat("\n")
    #cat("alpha =", (1-x$call$conf.level), "|", x$call$conf.level*100,"% Confidence Interval")
    cat("\n")
    cat("Binomial proportions test and quantile regression for LoA")
    cat("\n")
    cat("\n")
    agree_tab = x$agree
    colnames(agree_tab) = c("Agreement", "Lower CI", "Upper CI")
    print(x$agree, digits = 4)
    cat("Hypothesis Test: ",x$h0_test, sep = "")
    cat("\n")
    cat("\n")
    cat("###- Quantile Limits of Agreement (LoA) -###")
    cat("\n")
    print(LOA, digits = 4)
    cat("\n")
  }

}

#' @rdname simple_agree-methods
#' @method plot simple_agree
#' @param x_name Name/label for x values (first measurement)
#' @param y_name Name/label for y values (second measurement)
#' @param geom String naming the type of geometry to display the data points. Default is "geom_point". Other options include: "geom_bin2d", "geom_density_2d", "geom_density_2d_filled", and "stat_density_2d".
#' @param smooth_method Smoothing method (function) to use, accepts either NULL or a character vector, e.g. "lm", "glm", "gam", "loess" or a function. Default is NULL, which will not include a trend line.
#' @param smooth_se Display confidence interval around smooth?
#' @import ggplot2
#' @export

plot.simple_agree <- function(x, type = 1,
                              x_name = "x",
                              y_name = "y",
                              geom = "geom_point",
                              smooth_method = NULL,
                              smooth_se = TRUE,
                              ...){

  if(type == 1){
    #return(x$bland_alt.plot)
    if(get_call(x$call$prop_bias) != TRUE){
      simple_ba_plot(x,
                     x_name,
                     y_name,
                     geom,
                     smooth_method,
                     smooth_se)
    } else {
      bias_ba_plot(x,
                   x_name,
                   y_name,
                   geom,
                   smooth_method,
                   smooth_se)
    }

  } else if (type == 2){
    simple_ident_plot(x,
                      x_name,
                      y_name,
                      smooth_method,
                      smooth_se)
  } else{
   stop("please select type = 1 for a Bland Altman plot or type = 2 for an identity plot")
  }

}


#' @rdname simple_agree-methods
#' @export

check <- function(x) {
  UseMethod("check")
}

#' @rdname simple_agree-methods
#' @method check simple_agree
#' @importFrom stats residuals lm na.omit pchisq shapiro.test ks.test rstudent df.residual anova rstandard sigma resid
#' @export

check.simple_agree <- function(x) {

  if(!(as.character(x$call[1]) %in% c("agree_nest","agree_reps","agree_test",
                                      "SimplyAgree::agree_nest",
                                      "SimplyAgree::agree_reps",
                                      "SimplyAgree::agree_test"))){
    stop("agree_np is not supported by check method.")
  }

  if(as.character(x$call[1]) != "agree_test" && as.character(x$call[1]) != "SimplyAgree::agree_test"){
    df = model.frame(x$call$lm_mod)
    colnames(df) = c("y","x","id")
  } else{
    df = model.frame(x$call$lm_mod)
    colnames(df) = c("y","x")
  }
  df$mean = (df$x + df$y)/2
  df$delta = df$x - df$y
  if(x$call$prop_bias == TRUE){
    form_lm1 = as.formula(delta ~ mean)
    form_lmer1 = as.formula(delta ~ mean + (1|id))
  } else {
    form_lm1 = as.formula(delta ~ 1)
    form_lmer1 = as.formula(delta ~ 1 + (1|id))
  }


  dat = df
  ## Heteroskedasticity -------
  mod_check = if (as.character(x$call[1]) != "agree_test" && as.character(x$call[1]) != "SimplyAgree::agree_test") {
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
                          signif(p_val_het,4)))


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
                          signif(norm_test$p.value,4)))

  # Proportional Bias -----
  if(as.character(x$call[1]) == "agree_test" || as.character(x$call[1]) == "SimplyAgree::agree_test"){
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
                          signif(lin_pval,4)))

  #return(list(p_norm = p_norm,
  #            p_het = p_het,
  #            p_bias = p_bias))

  wrap_plots(p_norm, p_het,
             p_bias, ncol = 2)

}
