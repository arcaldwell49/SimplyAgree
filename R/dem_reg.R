#' Deming Regression
#' A function for fitting a straight line to two-dimensional data (i.e., X and Y) are measured with error.
#' @param x Name of column with first measurement
#' @param y Name of other column with the other measurement to compare to the first.
#' @param id Column with subject identifier
#' @param data Data frame with all data
#' @param conf.level The confidence level required. Default is 95\%.
#' @param weighted Logical indicator (TRUE/FALSE) for whether to use weighted Deming regression. Default is FALSE.
#' @param weights an optional vector of weights to be used in the fitting process. Should be NULL or a numeric vector.
#' @param error.ratio Ratio of the two error variances. Default is 1. This argument is ignored if subject identifiers are provided.
#' @param keep_data Logical indicator (TRUE/FALSE). If TRUE the jacknife samples are returned; default is FALSE.
#' @details
#' The function returns a simple_deming object and will print a table of the coefficients.
#'
#' @section References:
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
                    id,
                    data,
                    conf.level = .95,
                    weighted = FALSE,
                    weights = NULL,
                    error.ratio = 1,
                    keep_data = FALSE){
  call2 = match.call()

  confq = qnorm(1 - (1 - conf.level) / 2)
  if(is.null(id)){
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
                 w_i,
                 error.ratio)
  jacks = if (keep_data == TRUE) {
    res$jacks
  } else{
    NULL
  }
  res = res$df
  res$lower.ci = res$coef-confq*res$se
  res$upper.ci = res$coef+confq*res$se
  res$ci.level = conf.level
  return(structure(list(deming = res,
                        resamples = jacks,
                        call = call2),
                   class = "simple_deming"))
}
