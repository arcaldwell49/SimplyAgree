#' Methods for loa_mermod objects
#'
#' Methods defined for objects returned from the loa_mixed functions.
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
  title = paste0(agree,"% Limits of Agreement with Boostrapped ", conf, "% Confidence Intervals \n")
  cat(title)
  print(x$loa)
}

#' @rdname loa_mermod-methods
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
  df_plt = model.frame(x$call$lm_mod)
  colnames(df_plt) = c("diff", "avg", "id")

  df_loa = x$loa

  scalemin = min(df_plt$avg)
  scalemax = max(df_plt$avg)
  pd2 = position_dodge2(.03 * (scalemax - scalemin))

  df_loa2 = df_loa
  df_loa2$x = scalemin - (.03 * (scalemax - scalemin))
  df_loa2$text = factor(c("Bias", "Lower LoA", "Upper LoA"),
                        levels = c("Upper LoA", "Bias", "Lower LoA"))

  conf.level = x$call$conf.level
  agree.level = x$call$agree.level

    if(geom == "geom_point"){
      bland_alt.plot = ggplot(df_plt,
                              aes(x = avg, y = diff)) +
        geom_point(na.rm = TRUE)
    }  else if(geom == "geom_bin2d") {
      bland_alt.plot = ggplot(df_plt,
                              aes(x = avg, y = diff)) +
        geom_bin2d(na.rm = TRUE)
    } else if(geom == "geom_density_2d") {
      bland_alt.plot = ggplot(df_plt,
                              aes(x = avg, y = diff)) +
        geom_density_2d(na.rm = TRUE)
    } else if(geom == "geom_density_2d_filled") {
      bland_alt.plot = ggplot(df_plt,
                              aes(x = avg, y = diff)) +
        geom_density_2d_filled(na.rm = TRUE,
                               alpha = 0.5,
                               contour_var = "ndensity")
    } else if(geom == "stat_density_2d") {
      bland_alt.plot = ggplot(df_plt,
                              aes(x = avg, y = diff)) +
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
                    inherit.aes = FALSE)+
    labs(x = x_label,
         y = y_label,
         caption = paste0("Agreement = ", agree.level * 100,"% \n",
                          "Confidence Level = ", conf.level * 100, "%"),
         color = "") +
    scale_color_viridis_d(option = "C", end = .8) +
    theme_bw() +
    theme(legend.position = "left")

  return(bland_alt.plot)

}
