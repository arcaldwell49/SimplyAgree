# data of the first layer of a ggplot whose geom inherits from `geom`
geom_data = function(p, geom) {
  i = which(vapply(p$layers, function(l) inherits(l$geom, geom),
                   logical(1)))
  p$layers[[i[1]]]$data
}

# all ggplots of a patchwork, in order
patch_plots = function(p) {
  c(p$patches$plots, list(p))
}
