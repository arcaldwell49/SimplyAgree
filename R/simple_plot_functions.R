
simple_ident_plot = function(x) {
  if(x$class == "replicates"){
    form1 = as.formula(paste0(x$call$x, "~",
                              x$call$y, " + ",
                              x$call$id))
    call2 = structure(list(formula = form1,
                 data = x$call$data), class = "call")
    df = df %>%
      group_by(id) %>%
      summarize(mxi = sum(!is.na(x)),
                myi = sum(!is.na(y)),
                x_bar = mean(x, na.rm=TRUE),
                x_var = var(x, na.rm=TRUE),
                y_bar = mean(y, na.rm=TRUE),
                y_var = var(y, na.rm=TRUE),
                .groups = "drop")

  }
  #df_loa = x$loa
  df = model.frame(x$call)
  scalemin = min(c(min(df$x, na.rm = TRUE),min(df$y, na.rm = TRUE)))
  scalemax = max(c(max(df$x, na.rm = TRUE),max(df$y, na.rm = TRUE)))
  x_lab = x$labs$x_lab
  y_lab = x$labs$y_lab
  #df_loa2 = df_loa
  #df_loa2$x = scalemin
  #df_loa2$text = factor(c("Bias", "Lower LoA", "Upper LoA"),
  #                      levels = c("Upper LoA", "Bias", "Lower LoA"))

  #pca <- prcomp(~x+y, data = df, retx = FALSE)

  pca <- prcomp(~x+y, data = df, retx = FALSE)

  slp <- with(pca, rotation[2,1] / rotation[1,1])
  int <- with(pca, center[2] - slp*center[1])
  tmp.lm <- data.frame(the_int = int, the_slope = slp)

  ggplot(df,aes(x = x,
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
}



