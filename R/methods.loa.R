#' Methods for loa objects
#'
#' Methods defined for objects returned from the agreement_limit function.
#'
#' @param x object of class \code{loa} as returned from a agreement_limit function.
#' @param delta The maximal allowable difference.
#' @param digits The number of digits to print.
#' @param error.ratio For `check`, an optional known ratio of the measurement
#'   error variances, var(error of x) / var(error of y), as in [dem_reg()],
#'   on the analysis scale (the log scale with `log_tf = TRUE`). Used only
#'   with `data_type = "simple"`; see "Proportional bias diagnostic".
#' @param ... further arguments passed through, see description of return value
#'   for details.
#'   \code{\link{agreement_limit}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the Limits of Agreement.}
#'   \item{\code{plot}}{Returns a plot of the limits of agreement.}
#'   \item{\code{check}}{Returns plots testing the assumptions of a Bland-Altman analysis. P-values for the normality, heteroskedasticity, and proportional bias tests are provided as captions to the plot. The proportional bias results are attached as the attribute \code{"prop_bias"}, a data frame with the test (\code{test}, \code{corrected}, \code{n}, \code{statistic}, \code{df}, \code{p.value}, and \code{p.uncorrected}, the uncorrected Pitman-Morgan p-value), the observed slope of the differences on the means (\code{slope_obs}), the slope expected from unequal measurement error alone (\code{slope_null}) and the error-adjusted slope (\code{slope_adj}), the implied scale of y relative to x on the true values, \code{beta} = (2 - \code{slope_adj})/(2 + \code{slope_adj}), the within-subject variances (\code{sw2_x}, \code{sw2_y}), and \code{error.ratio}. Columns that do not apply are \code{NA}.}
#' }
#'
#' @section Proportional bias diagnostic:
#' The proportional bias panel of `check()` plots the differences (centred)
#' against the means, on the analysis scale (the log scale for
#' log-transformed data), whether or not the fit used `prop_bias`. Because
#' cov(x - y, x + y) = var(x) - var(y), a slope of the differences on the
#' means only shows that the two methods have unequal total variance; the
#' test of the slope is the Pitman-Morgan test of equal variances. Unequal
#' variances arise from a true proportional bias (y = a + b x with b not 1),
#' but also from unequal measurement error or from a prior calibration of one
#' method on the other (Bland & Altman, 1999; Hopkins, 2004). With one
#' measurement per method these cannot be told apart, and `check()` says so
#' when the test is significant.
#'
#' What is tested depends on the design:
#'
#'   - **One measurement per method** (`data_type = "simple"`): the
#'     Pitman-Morgan test (t on n - 2 df). With `error.ratio`, the Deming
#'     slope of y on x for that error ratio is tested against 1 instead (the
#'     test reported by [dem_reg()]), and the error-adjusted slope
#'     2(1 - b)/(1 + b) is drawn dashed.
#'   - **Replicates** (`data_type = "reps"`): the test uses subject means
#'     and is corrected for unequal measurement error. The covariance of the
#'     subject-mean differences and sums is compared with its expected value
#'     without proportional bias, \eqn{s^2_{wx}\overline{1/m_x} -
#'     s^2_{wy}\overline{1/m_y}}, from the pooled within-subject variances,
#'     rather than with zero. The standard error is a jackknife over
#'     subjects (re-estimating the within-subject variances) with n - 1 df.
#'     Only the difference of the error variances enters, so an occasion
#'     effect shared by both methods (linked replicates) cancels. The
#'     error-adjusted slope is drawn dashed. Complete pairs of measurements
#'     are used.
#'   - **Nested data** (`data_type = "nest"`): the true value changes
#'     within a subject, so the error variances cannot be estimated; the slope
#'     is tested with a random-intercept model (likelihood ratio test) and is
#'     subject to the same limitation as one measurement per method.
#'
#' @references
#' Bland, J. M., & Altman, D. G. (1999). Measuring agreement in method
#' comparison studies. Statistical Methods in Medical Research, 8(2), 135-160.
#'
#' Hopkins, W. G. (2004). Bias in Bland-Altman but not regression validity
#' analyses. Sportscience, 8, 42-46.
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
    if(call2$log_tf_display == "ratio"){
      pr_table = pr_table %>% mutate_if(is.numeric,exp)
    }

    if(call2$log_tf_display == "sympercent"){
      pr_table = pr_table %>% mutate_if(is.numeric,~100*.)
    }

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
  if(x$call$log_tf){
    if(x$call$log_tf_display == "ratio"){
      title1 = paste0(title1, " of the Ratio (x/y)")
    }

    if(x$call$log_tf_display == "sympercent"){
      title1 = paste0(title1, " of the Sympercent Difference (s%)")
    }
  }
  # objects created before bound_type existed used IU bounds
  bound_type = if(is.null(x$call$bound_type)) "iu" else x$call$bound_type
  subtitle1 = paste0(
    x$call$agree.level*100,
    "% LoA @ ",
    x$call$alpha*100,
    "% Alpha-Level"
  )
  bound_note = switch(
    bound_type,
    iu = paste0(
      "LoA CI: one-sided ", (1 - x$call$alpha) * 100,
      "% bounds (outer ends of ", (1 - 2 * x$call$alpha) * 100,
      "% CIs) for an intersection-union test against a maximal allowable difference;",
      "\n  not a joint ", (1 - x$call$alpha) * 100, "% interval for the LoA"
    ),
    joint = paste0(
      "LoA CI: outer ends of ", (1 - x$call$alpha) * 100,
      "% CIs; at least ", (1 - x$call$alpha) * 100,
      "% joint confidence for both LoA"
    )
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
                       ifelse(call2$log_tf_display == "ratio",
                       round((exp(x$loa$sd_delta[1])-1)*100,digits=digits),
                       round(((x$loa$sd_delta[1]))*100,digits=digits)
                       )
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
  cat(bound_note, sep = "")
  cat("\n")
  cat(var_print, sep = "")
  cat("\n")
  if(call2$data_type == "reps"){
    var_print2 = paste0(
      "Within-Subject SDs of X & Y = ",
      round(sqrt(x$loa$within_variance_x[1]),digits=digits),
      " & ",
      round(sqrt(x$loa$within_variance_y[1]),digits=digits),
      if(call2$log_tf) " (log scale)"
    )
    cat(var_print2, sep = "")
    cat("\n")
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

check.loa <- function(x,
                      error.ratio = NULL,
                      ...) {
  .pm_check_ratio(error.ratio)
  if (!is.null(error.ratio) && x$call$data_type != "simple") {
    message("error.ratio is only used with data_type = 'simple' and is ",
            "ignored here",
            if (x$call$data_type == "reps") {
              " (the error variances are estimated from the replicates)."
            } else ".")
    error.ratio = NULL
  }
  panels = .loa_check_panels(x, error.ratio = error.ratio)
  .pm_notify(panels$pb, design = panels$pb_design)
  p = wrap_plots(panels$p_norm, panels$p_het,
             panels$p_bias, ncol = 2) & plot_annotation(
               theme = theme(
                 panel.background = element_rect(fill='transparent'), #transparent panel bg
                 plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
                 panel.grid.major = element_blank(), #remove major gridlines
                 panel.grid.minor = element_blank(), #remove minor gridlines
                 legend.background = element_rect(fill='transparent'), #transparent legend bg
                 legend.box.background = element_rect(fill='transparent') #transparent legend panel
               ))
  attr(p, "prop_bias") = .pm_table(list(panels$pb))
  p
}

# The three diagnostic panels of check.loa() (normality, heteroskedasticity,
# proportional bias), returned separately so other methods (ccc_test) can
# reuse some of them.
.loa_check_panels <- function(x,
                              error.ratio = NULL) {

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
  # Slope of the differences on the means on the analysis scale (log scale
  # with log_tf), whether or not prop_bias was used in the fit. See
  # prop_bias_helpers.R.
  pb_design = "single"
  if (call2$data_type == "nest") {
    # true values vary within subjects, so the pairs are linked by occasion
    # and the error variances cannot be estimated
    dat_pb = data.frame(delta = dat$delta,
                        m_tf = (dat$x + dat$y) / 2,
                        id = dat$id)
    m0 = suppressMessages(lmer(delta ~ 1 + (1 | id), data = dat_pb))
    m1 = suppressMessages(lmer(delta ~ m_tf + (1 | id), data = dat_pb))
    aov2 = suppressMessages(as.data.frame(anova(m0, m1)))
    pb = list(method = "Mixed model test of the slope",
              corrected = FALSE,
              n = nrow(dat_pb),
              statistic = aov2[2, "Chisq"],
              parameter = aov2[2, "Df"],
              p.value = aov2[2, "Pr(>Chisq)"],
              slope_obs = unname(lme4::fixef(m1)[["m_tf"]]),
              data = data.frame(mean = dat_pb$m_tf, diff = dat_pb$delta))
  } else if (call2$data_type == "reps") {
    pb = .pm_slope_test(dat$x, dat$y, dat$id, correct = TRUE)
    if (pb$corrected) pb_design = "reps"
  } else if (!is.null(error.ratio)) {
    pb = .pm_known_ratio(dat$x, dat$y, error.ratio = error.ratio)
    pb_design = "ratio"
  } else {
    pb = .pm_slope_test(dat$x, dat$y)
  }

  dat2 = data.frame(resid = pb$data$diff - mean(pb$data$diff),
                    mean = pb$data$mean)
  x_lab = paste0("Mean of both methods",
                 if (call2$data_type == "reps") " (subject means)",
                 if (isTRUE(call2$log_tf)) " (log scale)")
  p_bias = plot_bias(dat2,
                     adj_slope = if (pb$corrected) pb$slope_adj,
                     adj_center = mean(dat2$mean),
                     y_lab = "Difference (centred)",
                     x_lab = x_lab) +
    labs(caption = paste0("Proportional Bias", " \n",
                          .pm_caption(pb))) +
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )

  list(p_norm = p_norm,
       p_het = p_het,
       p_bias = p_bias,
       pb = pb,
       pb_design = pb_design)
}
