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
#' @param compute_joint Logical indicator (TRUE/FALSE). If TRUE, joint confidence region is computed. Default is TRUE.
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
#'
#' When compute_joint = TRUE, the function computes the joint (slope, intercept) confidence region
#' based on the chi-square distribution with 2 degrees of freedom. This elliptical region accounts
#' for the correlation between slope and intercept estimates and can provide improved power for
#' detecting deviations from (null) hypothesized values (e.g., slope = 1, intercept = 0).
#'
#' @returns
#' The function returns a simple_eiv (eiv meaning "error in variables") object.
#'
#'   - `call`: The matched call.
#'   - `model`: Data frame presenting the results from the Deming regression analysis.
#'   - `resamples`: List containing resamples from jacknife procedure.
#'   - `vcov`: Variance-covariance matrix for slope and intercept.
#'   - `joint_region`: Joint confidence region ellipse coordinates (if compute_joint = TRUE).
#'   - `joint_test`: Test of whether ideal point is enclosed by joint region.
#'
#' @references
#' Linnet, K. (1990) Estimation of the linear relationship between the measurements of two methods with proportional errors. Statistics in Medicine, 9, 1463-1473.
#'
#' Linnet, K. (1993). Evaluation of regression procedures for methods comparison studies. Clinical chemistry, 39, 424-432.
#'
#' Sadler, W.A. (2010). Joint parameter confidence regions improve the power of parametric regression in method-comparison studies. Accreditation and Quality Assurance, 15, 547-554.
#'
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
                    keep_data = FALSE,
                    compute_joint = TRUE){
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

  # Always keep jacks temporarily for vcov computation
  jacks_temp = res$jacks

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

  # Compute variance-covariance matrix using temporary jacks
  # Compute correlation from jackknife samples
  if (!is.null(jacks_temp) && length(jacks_temp) >= 2) {
    jack_cor <- cor(jacks_temp[[1]], jacks_temp[[2]])
  } else {
    # Fallback: estimate correlation from data structure
    # For Deming regression, slope and intercept are typically negatively correlated
    # Estimate based on x-range
    x_range_ratio <- max(df3$x) / min(df3$x)
    if (x_range_ratio < 5) {
      jack_cor <- -0.95  # Narrow range: high negative correlation
    } else if (x_range_ratio < 20) {
      jack_cor <- -0.70  # Medium range: moderate negative correlation
    } else {
      jack_cor <- -0.30  # Wide range: low negative correlation
    }
  }

  vcov_matrix <- matrix(
    c(res$se[1]^2, res$se[1] * res$se[2] * jack_cor,
      res$se[1] * res$se[2] * jack_cor, res$se[2]^2),
    nrow = 2, ncol = 2,
    dimnames = list(c("intercept", "slope"), c("intercept", "slope"))
  )

  # Compute joint confidence region if requested
  joint_region <- NULL
  joint_test <- NULL

  if (compute_joint) {
    # Generate ellipse coordinates
    joint_region <- .compute_joint_region(
      intercept = res$coef[1],
      slope = res$coef[2],
      vcov = vcov_matrix,
      conf.level = conf.level,
      n_points = 100
    )

    # Test if ideal point (slope=1, intercept=0) is enclosed
    joint_test <- .test_joint_enclosure(
      intercept = res$coef[1],
      slope = res$coef[2],
      vcov = vcov_matrix,
      ideal_intercept = 0,
      ideal_slope = 1,
      conf.level = conf.level
    )
  }

  return(structure(list(model = res,
                        resamples = jacks,
                        vcov = vcov_matrix,
                        joint_region = joint_region,
                        joint_test = joint_test,
                        call = call2),
                   class = "simple_eiv"))
}


.compute_joint_region <- function(intercept, slope, vcov, conf.level = 0.95, n_points = 100) {

  # Chi-square critical value for 2 df
  chi2_crit <- qchisq(conf.level, df = 2)

  # Eigendecomposition of covariance matrix
  eig <- eigen(vcov)
  lambda <- eig$values
  v <- eig$vectors

  # Generate circle
  theta <- seq(0, 2 * pi, length.out = n_points)
  circle <- cbind(cos(theta), sin(theta))

  # Transform to ellipse
  ellipse <- t(v %*% diag(sqrt(lambda * chi2_crit)) %*% t(circle))

  # Center at (intercept, slope)
  ellipse[, 1] <- ellipse[, 1] + intercept
  ellipse[, 2] <- ellipse[, 2] + slope

  colnames(ellipse) <- c("intercept", "slope")

  return(as.data.frame(ellipse))
}



.test_joint_enclosure <- function(intercept, slope, vcov,
                                  ideal_intercept, ideal_slope,
                                  conf.level = 0.95) {

  # Test point
  point <- c(ideal_intercept, ideal_slope)
  center <- c(intercept, slope)

  # Mahalanobis distance
  diff <- point - center
  vcov_inv <- solve(vcov)
  mahal_dist <- as.numeric(t(diff) %*% vcov_inv %*% diff)

  # Chi-square critical value
  chi2_crit <- qchisq(conf.level, df = 2)

  is_enclosed <- mahal_dist <= chi2_crit

  list(
    mahalanobis_distance = mahal_dist,
    chi2_critical = chi2_crit,
    is_enclosed = is_enclosed,
    p_value = 1 - pchisq(mahal_dist, df = 2)
  )
}
