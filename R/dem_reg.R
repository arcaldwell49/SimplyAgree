#' @title Deming Regression
#'
#' @description
#'
#' `r lifecycle::badge('stable')`
#'
#' A function for fitting a straight line to two-dimensional data (i.e., X and Y) that are measured with error.
#'
#' @param x Name of column with first measurement.
#' @param y Name of other column with the other measurement to compare to the first.
#' @param id Column with subject identifier.
#' @param data Data frame with all data.
#' @param conf.level The confidence level required. Default is 95%.
#' @param weighted Logical indicator (TRUE/FALSE) for whether to use weighted Deming regression. Default is FALSE.
#' @param weights an optional vector of weights to be used in the fitting process. Should be NULL or a numeric vector.
#' @param error.ratio Ratio of the two error variances. Default is 1. This argument is ignored if subject identifiers are provided.
#' @param keep_data Logical indicator (TRUE/FALSE). If TRUE, the jacknife samples are returned; default is FALSE. Users may wish to set to FALSE if data is especially large.
#' @details
#'
#' This function provides a Deming regression analysis wherein the sum of distances in both x and y direction is minimized.
#' Deming regression, also known as error-in-variable regression, is useful in situations where both X & Y are measured with error.
#' The use of Deming regression is beneficial when comparing to methods for measuring the same continuous variable.
#'
#' Currently, the dem_reg function covers simple Deming regression and weighted Deming regression.
#' Weighted Deming regression can be used by setting the weighted argument to TRUE.
#' The weights can be provided by the user or can be calculated within function.
#'
#' If the data are measured in replicates, then the measurement error can be directly derived from the data.
#' This can be accomplished by indicating the subject identifier with the id argument.
#' When the replicates are not available in the data,
#' then the ratio of error variances (y/x) can be provided with the error.ratio argument.
#' @returns
#' The function returns a simple_eiv (eiv meaning "error in variables") object.
#'
#'   - `call`: The matched call.
#'   - `model`: Data frame presenting the results from the Deming regression analysis.
#'   - `resamples`: List containing resamples from jacknife procedure.
#'
#' @references
#' Linnet, K. (1990) Estimation of the linear relationship between the measurements of two methods with proportional errors. Statistics in Medicine, 9, 1463-1473.
#'
#' Linnet, K. (1993). Evaluation of regression procedures for methods comparison studies. Clinical chemistry, 39, 424-432.
#' @importFrom stats pnorm pt qnorm qt lm anova aov complete.cases cor dchisq qchisq sd var prcomp
#' @importFrom graphics text
#' @import ggplot2
#' @export
#'

dem_reg <- function(x,
                    y,
                    id = NULL,
                    data,
                    conf.level = .95,
                    weighted = FALSE,
                    weights = NULL,
                    error.ratio = 1,
                    keep_data = FALSE){
  call2 = match.call()
  call2$weighted = weighted
  call2$conf.level = conf.level
  call2$id = id

  conf2 =  1-(1 - conf.level) / 2
  if(!is.null(id)){
  df = data %>%
    select(all_of(id),all_of(x),all_of(y)) %>%
    rename(id = all_of(id),
           x = all_of(x),
           y = all_of(y)) %>%
    select(id,x,y)
  } else {
    df = data %>%
      select(all_of(x),all_of(y)) %>%
      rename(x = all_of(x),
             y = all_of(y)) %>%
      select(x,y)
  }

  if(is.null(id)){
    df3 = df %>% drop_na()
  } else {
    df2 = df %>%
      group_by(id) %>%
      mutate(mean_y = mean(y, na.rm =TRUE),
             mean_x = mean(x, na.rm =TRUE),
             n_x = sum(!is.na(x)),
             n_y = sum(!is.na(y))) %>%
      ungroup() %>%
      mutate(diff_y = y - mean_y,
             diff_y2 = diff_y^2,
             diff_x = x - mean_x,
             diff_x2 = diff_x^2)
    df3 = df2 %>%
      group_by(id) %>%
      summarize(n_x = mean(n_x),
                x = mean(x, na.rm = TRUE),
                sum_num_x = sum(diff_x2, na.rm = TRUE),
                n_y = mean(n_y),
                y = mean(y, na.rm = TRUE),
                sum_num_y = sum(diff_y2, na.rm = TRUE),
                .groups = 'drop') %>%
      drop_na()

    var_x = sum(df3$sum_num_x) / sum(df3$n_x-1)
    var_y = sum(df3$sum_num_y) / sum(df3$n_y-1)

    error.ratio = var_x/var_y
  }

  if(weighted == FALSE){
    w_i = rep(1,nrow(df3))
  } else if(!is.null(weights)){
    w_i = weights
  } else {
    w_i = 1/((df3$x+error.ratio*df3$y)/(1+error.ratio))^2
  }

  res = jack_dem(df3$x, df3$y,
                 w_i = w_i,
                 error.ratio = error.ratio)
  if (keep_data == TRUE) {
    jacks = res$jacks
    call2$weights = w_i
  } else{
    jacks = NULL
    call2$weights = w_i
  }
  res = res$df
  confq = qt(conf2, nrow(df3)-2)
  res$df = nrow(df3)-2
  res$lower.ci = res$coef-confq*res$se
  res$upper.ci = res$coef+confq*res$se
  #res$ci.level = conf.level
  res$t = 0
  res$t[1] = res$coef[1]/res$se[1]
  res$t[2] = (res$coef[2] - 1)/res$se[2]
  res$p.value = 2*pt(abs(res$t), res$df, lower.tail=FALSE)
  call2$error.ratio = error.ratio

  lm_mod = list(call = list(formula = as.formula(df3$y~df3$x)))
  call2$lm_mod = lm_mod
  return(structure(list(model = res,
                        resamples = jacks,
                        call = call2),
                   class = "simple_eiv"))
}
