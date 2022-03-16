
simple_ident_plot = function(x) {
  df_loa = x$loa
  df = model.frame(x$prcomp)
  scalemin = min(c(min(x, na.rm = TRUE),min(y, na.rm = TRUE)))
  scalemax = max(c(max(x, na.rm = TRUE),max(y, na.rm = TRUE)))

  df_loa2 = df_loa
  df_loa2$x = scalemin
  df_loa2$text = factor(c("Bias", "Lower LoA", "Upper LoA"),
                        levels = c("Upper LoA", "Bias", "Lower LoA"))
}



