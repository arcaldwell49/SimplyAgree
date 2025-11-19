#' Methods for simple_eiv objects
#'
#' Methods defined for objects returned from the error-in-variables models (e.g., dem_reg).
#'
#' @param x object of class \code{simple_eiv} from the dem_reg function.
#' @param ... further arguments passed through, see description of return value.
#'   for details.
#'   \code{\link{agree_test}}.
#' @return
#' \describe{
#'   \item{\code{print}}{Prints short summary of the error-in-variables (e.g., Deming) regression model.}
#'   \item{\code{plot}}{Returns a plot of the deming regression line, the line-of-identity, and the raw data.}
#'   \item{\code{check}}{Returns plots of the optimized residuals.}
#'   \item{\code{plot_joint}}{Returns a plot of the joint confidence region in parameter space.}
#' }
#'
#' @name simple_eiv-methods


### methods for simple_eiv objects

#' @rdname simple_eiv-methods
#' @method print simple_eiv
#' @export

print.simple_eiv <- function(x, ...){
  if(x$call$weighted == TRUE){
    header = paste0("Weighted Deming Regression with ",x$call$conf.level*100,"% C.I.")
  } else{
    header = paste0("Deming Regression with ",x$call$conf.level*100,"% C.I.")
  }
  cat(header)
  cat("\n")
  cat("\n")
  cat("Coefficients:\n")
  print(x$model, digits = 4)

  # Add joint region test results if available
  if (!is.null(x$joint_test)) {
    cat("\n")
    cat("Joint Confidence Region Test (H0: slope=1, intercept=0):\n")
    cat(sprintf("  Mahalanobis distance: %.4f\n", x$joint_test$mahalanobis_distance))
    cat(sprintf("  Chi-square critical:  %.4f\n", x$joint_test$chi2_critical))
    cat(sprintf("  Identity enclosed:    %s\n",
                ifelse(x$joint_test$is_enclosed, "Yes", "No")))
    cat(sprintf("  p-value:             %.4f\n", x$joint_test$p_value))

    # Compare with individual CI tests
    slope_enclosed <- x$model$coef[2] >= x$model$lower.ci[2] &&
      1 <= x$model$upper.ci[2]
    int_enclosed <- x$model$coef[1] >= x$model$lower.ci[1] &&
      0 <= x$model$upper.ci[1]

    if (!slope_enclosed || !int_enclosed) {
      cat("\n")
      cat("Note: Individual confidence intervals suggest departure from identity,\n")
      cat("      but joint region provides more powerful test.\n")
    }
  }
}

#' @rdname simple_eiv-methods
#' @method plot simple_eiv
#' @param x_name Name/label for x values (first measurement)
#' @param y_name Name/label for y values (second measurement)
#' @param show_joint Logical. If TRUE and joint region computed, shows joint region status in subtitle.
#' @import ggplot2
#' @importFrom patchwork plot_annotation
#' @export

plot.simple_eiv <- function(x,
                            x_name = "x",
                            y_name = "y",
                            show_joint = TRUE,
                            ...){
  df = model.frame(x$call$lm_mod)
  colnames(df) = c("y", "x")
  scalemin = min(c(min(df$x, na.rm = TRUE),min(df$y, na.rm = TRUE)))
  scalemax = max(c(max(df$x, na.rm = TRUE),max(df$y, na.rm = TRUE)))
  x_lab = x_name
  y_lab = y_name

  slp <- x$model$coef[2]
  int <- x$model$coef[1]
  tmp.lm <- data.frame(the_int = int, the_slope = slp)

  # Create subtitle with joint test result if available
  subtitle_text <- NULL
  if (show_joint && !is.null(x$joint_test)) {
    if (x$joint_test$is_enclosed) {
      subtitle_text <- sprintf("Identity line enclosed by %.0f%% joint confidence region (p = %.3f)",
                               x$call$conf.level * 100,
                               x$joint_test$p_value)
    } else {
      subtitle_text <- sprintf("Identity line NOT enclosed by %.0f%% joint confidence region (p = %.3f)",
                               x$call$conf.level * 100,
                               x$joint_test$p_value)
    }
  }

  p1 = ggplot(df,aes(x = x,
                     y = y)) +
    geom_point(na.rm = TRUE) +
    geom_abline(intercept = 0,
                slope = 1,
                linetype = "solid",
                color = "black") +
    geom_abline(
      data = tmp.lm,
      aes(intercept = the_int, slope = the_slope),
      linetype = "dashed",
      color = "red"
    ) +
    xlab(paste0("Method: ",x_lab)) +
    xlim(scalemin,scalemax) +
    ylim(scalemin,scalemax) +
    ylab(paste0("Method: ",y_lab)) +
    labs(subtitle = subtitle_text) +
    coord_fixed(ratio = 1 / 1) +
    theme_bw()

  return(p1)

}

#' @rdname simple_eiv-methods
#' @method check simple_eiv
#' @export

check.simple_eiv <- function(x) {

  df = model.frame(x$call$lm_mod)
  colnames(df) = c("y", "x")
  b0 = x$model$coef[1]
  b1 = x$model$coef[2]
  w_i = x$call$weights
  error.ratio = x$call$error.ratio
  d_i = df$y - (b0+b1*df$x)
  x_hat = df$x + (error.ratio*b1*d_i)/(1+error.ratio*b1^2)
  y_hat = df$y - (d_i)/(1+error.ratio*b1^2)
  res_x = df$x - x_hat
  res_y = df$y - y_hat
  d_sign = ifelse(d_i >= 0, 1, -1)
  opt_res = d_sign * sqrt(w_i*res_x^2 + w_i * error.ratio * res_y^2)
  avg_both = ((x_hat + y_hat )/ 2)

  mod <- lm(opt_res/d_sign ~ avg_both)

  SS <- anova(mod)$"Sum Sq"
  RegSS <- sum(SS) - SS[length(SS)]
  Chisq <- RegSS / 2
  ### Breusch-Pagan Test
  p_val_het <- pchisq(Chisq, df = 1, lower.tail = FALSE)
  df1 = data.frame(x = avg_both,
                   y = opt_res/d_sign)
  p1 = ggplot(df1,
              aes(x=x,
                  y=y)) +
    geom_point() +
    geom_smooth(se = TRUE,
                method = "loess",
                linewidth = .8,
                color ="#3aaf85",
                formula = y~x) +
    labs(y = "|Optimized Residuals|",
         x = "Average of Both Estimated Values",
         title = "Homogeneity of Residuals",
         subtitle = "Reference line should be flat and horizontal",
         caption = paste0("Heteroskedasticity", " \n",
                          "Breusch-Pagan Test: p = ",
                          signif(p_val_het,4))) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )

  dat_norm <- na.omit(data.frame(y = opt_res))
  norm_test = shapiro.test(opt_res)
  norm_text = "Shapiro-Wilk Test"
  p2 = plot_qq(x = dat_norm) +
    labs(caption = paste0("Normality", " \n",
                          norm_text, ": p = ",
                          signif(norm_test$p.value,4)))+
    theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    )
  wrap_plots(p2, p1, ncol = 2) & plot_annotation(
    theme = theme(
      panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
      panel.grid.major = element_blank(), #remove major gridlines
      panel.grid.minor = element_blank(), #remove minor gridlines
      legend.background = element_rect(fill='transparent'), #transparent legend bg
      legend.box.background = element_rect(fill='transparent') #transparent legend panel
    ))
}


#' @rdname simple_eiv-methods
#' @export

plot_joint <- function(object, ...) {
  UseMethod("plot_joint")
}

#' @rdname simple_eiv-methods
#' @param object object of class \code{simple_eiv} for plot_joint method
#' @method plot_joint simple_eiv
#' @param ideal_slope The hypothesized slope value to test against (default = 1)
#' @param ideal_intercept The hypothesized intercept value to test against (default = 0)
#' @param show_intervals Logical. If TRUE, shows individual confidence intervals as well.
#' @export
plot_joint.simple_eiv <- function(object,
                                  ideal_slope = 1,
                                  ideal_intercept = 0,
                                  show_intervals = TRUE,
                                  ...) {

  if (is.null(object$joint_region)) {
    stop("Joint confidence region not computed. Re-run dem_reg() with compute_joint = TRUE")
  }

  # Get estimates
  est_slope <- object$model$coef[2]
  est_intercept <- object$model$coef[1]

  # Get confidence intervals
  ci_slope <- c(object$model$lower.ci[2], object$model$upper.ci[2])
  ci_intercept <- c(object$model$lower.ci[1], object$model$upper.ci[1])

  # Test if ideal point enclosed
  ideal_test <- .test_joint_enclosure(
    intercept = est_intercept,
    slope = est_slope,
    vcov = object$vcov,
    ideal_intercept = ideal_intercept,
    ideal_slope = ideal_slope,
    conf.level = object$call$conf.level
  )

  # Create base plot
  p <- ggplot() +
    # Joint confidence region (ellipse)
    geom_path(data = object$joint_region,
              aes(x = slope, y = intercept),
              color = "red",
              size = 1.2) +
    # Estimated point
    geom_point(aes(x = est_slope, y = est_intercept),
               size = 3,
               color = "black") +
    # Ideal point
    geom_point(aes(x = ideal_slope, y = ideal_intercept),
               size = 4,
               shape = 4,
               stroke = 1.5,
               color = ifelse(ideal_test$is_enclosed, "darkgreen", "darkred")) +
    labs(
      title = "Joint Confidence Region",
      subtitle = sprintf("%.0f%% confidence level | Identity %s by region",
                         object$call$conf.level * 100,
                         ifelse(ideal_test$is_enclosed, "ENCLOSED", "NOT enclosed")),
      x = "Slope",
      y = "Intercept",
      caption = sprintf("Mahalanobis distance = %.3f | chi-squared critical = %.3f | p = %.4f",
                        ideal_test$mahalanobis_distance,
                        ideal_test$chi2_critical,
                        ideal_test$p_value)
    ) +
    theme_bw() +
    theme(plot.caption = element_text(hjust = 0))

  # Add confidence interval rectangle if requested
  if (show_intervals) {
    rect_df <- data.frame(
      xmin = ci_slope[1],
      xmax = ci_slope[2],
      ymin = ci_intercept[1],
      ymax = ci_intercept[2]
    )

    p <- p +
      geom_rect(data = rect_df,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = "blue",
                alpha = 0.1,
                color = "blue",
                linetype = "dashed")
  }

  return(p)
}


#' @rdname simple_eiv-methods
#' @method vcov simple_eiv
#' @export

vcov.simple_eiv <- function(object, ...) {
  if (is.null(object$vcov)) {
    stop("Variance-covariance matrix not available. Re-run dem_reg().")
  }
  return(object$vcov)
}


#' @rdname simple_eiv-methods
#' @method coef simple_eiv
#' @export

coef.simple_eiv <- function(object, ...) {
  coefs <- object$model$coef
  names(coefs) <- c("intercept", "slope")
  return(coefs)
}
