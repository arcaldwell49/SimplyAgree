#' @title Agreement Coefficents
#' @description
#' `r lifecycle::badge('maturing')`
#'
#' agree_coef produces inter-rater reliability or "agreement coefficients" as described by Gwet.
#' @param measure Name of column containing the measurement of interest.
#' @param item Name of column containing the items. If this is an inter-rater reliability study then this would indicate the rater (e.g., rater1, rater2, rater3, etc).
#' @param id Column with subject identifier.
#' @param data Data frame with all data.
#' @param wide Logical value (TRUE or FALSE) indicating if data is in a "wide" format. Default is TRUE.
#' @param col.names If wide is equal to TRUE then col.names is a list of the column names containing the measurements for reliability analysis.
#' @param conf.level the confidence level required. Default is 95%.
#' @param weighted Logical value (TRUE or FALSE) indicating whether to weight the responses. If TRUE (default is FALSE) then quadratic weights are utilized. This option should be set to TRUE for ordinal or continuous responses.
#'
#' @return Returns single data frame of inter-rater reliability coefficients.
#' @examples
#' data('reps')
#' agree_coef(data = reps, wide = TRUE, col.names = c("x","y"), weighted = TRUE)
#'
#' @references
#' Gwet, K.L. (2014, ISBN:978-0970806284). “Handbook of Inter-Rater Reliability,” 4th Edition. Advanced Analytics, LLC.
#' Gwet, K. L. (2008). “Computing inter-rater reliability and its variance in the presence of high agreement," British Journal of Mathematical and Statistical Psychology, 61, 29-48.
#' @importFrom stats reshape
#' @importFrom tidyselect all_of
#' @import dplyr
#' @export
#'

agree_coef = function(wide = TRUE,
                      col.names = NULL,
                      measure,
                      item,
                      id,
                      data,
                      weighted = FALSE,
                      conf.level = .95){

  alpha = 1-conf.level
  x = data
  if(wide == TRUE){
    if(is.null(col.names)){
      stop("Must provide column names (col.names) if wide = TRUE")
    }
    x = x[,col.names]
    n.obs <- dim(x)[1]
    nj <- dim(x)[2]
    x.s <- stack(as.data.frame(x))
    x.df <- data.frame(x.s, subs = rep(paste("S", 1:n.obs, sep = ""),
                                       nj))
  } else {

    x.df = x %>%
      select(all_of(measure),all_of(item),all_of(id))
  }

  colnames(x.df) <- c("values", "items", "id")

  x_w = reshape(x.df, v.names = "values",idvar="id",timevar="items",
                direction = "wide")
  col_lens = ncol(x_w)-1
  colnames(x_w) = c("id", 1:col_lens)

  ratings.mat = as.matrix(subset(x_w, select = -c(id)))

  res = pa_coef(ratings.mat,
                conf.level = conf.level,
                weighted = weighted)

  return(res)
}
