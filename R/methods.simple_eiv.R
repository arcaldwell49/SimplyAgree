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
#' }
#'
#' @name simple_eiv-methods


### methods for simple_eiv objects

#' @rdname simple_eiv-methods
#' @method print simple_eiv
#' @export

print.simple_eiv <- function(x,...){
  if(x$call$weighted == TRUE){
    header = paste0("Weighted Deming Regression with ",x$call$conf.level*100,"% C.I.")
  } else{
    header = paste0("Deming Regression with ",x$call$conf.level*100,"% C.I.")
  }
  cat(header)
  cat("\n")
  print(x$model, digits = 4)
}

#' @rdname simple_eiv-methods
#' @method plot simple_eiv
#' @param x_name Name/label for x values (first measurement)
#' @param y_name Name/label for y values (second measurement)
#' @import ggplot2
#' @export

plot.simple_eiv <- function(x,
                               x_name = "x",
                               y_name = "y",
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

  p1 = ggplot(df,aes(x = x,
                     y = y)) +
    geom_point(na.rm = TRUE) +
    geom_abline(intercept = 0,
                slope = 1) +
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
                size = .8,
                color ="#3aaf85",
                formula = y~x) +
    labs(y = "|Optimized Residuals|",
         x = "Average of Both Estimated Values",
         title = "Homogeneity of Residuals",
         subtitle = "Reference line should be flat and horizontal",
         caption = paste0("Heteroskedasticity", " \n",
                                    "Breusch-Pagan Test: p = ",
                                    signif(p_val_het,4))) +
    theme_bw()

  dat_norm <- na.omit(data.frame(y = opt_res))
  norm_test = shapiro.test(opt_res)
  norm_text = "Shapiro-Wilk Test"
  p2 = plot_qq(x = dat_norm) +
    labs(caption = paste0("Normality", " \n",
                          norm_text, ": p = ",
                          signif(norm_test$p.value,4)))
  wrap_plots(p2, p1, ncol = 2)
}
