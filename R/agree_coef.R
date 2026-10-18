#' @title Agreement Coefficients
#' @description
#' `r lifecycle::badge('superseded')`
#'
#' `agree_coef()` has been superseded by [cac_test()], which returns one
#' coefficient as an `htest` with a hypothesis test, `summary()` and `plot()`
#' methods. `agree_coef()` still returns a table of all four coefficients.
#'
#' agree_coef produces inter-rater reliability or "agreement coefficients" as
#' described by Gwet: percent agreement, Gwet's AC1 (AC2 when weighted),
#' Fleiss' kappa and Krippendorff's alpha.
#' @param measure Name of column containing the measurement of interest.
#' @param item Name of column containing the items. If this is an inter-rater reliability study then this would indicate the rater (e.g., rater1, rater2, rater3, etc).
#' @param id Column with subject identifier.
#' @param data Data frame with all data.
#' @param wide Logical value (TRUE or FALSE) indicating if data is in a "wide" format. Default is TRUE.
#' @param col.names If wide is equal to TRUE then col.names is a list of the column names containing the measurements for reliability analysis.
#' @param conf.level the confidence level required. Default is 95%.
#' @param weighted Logical value (TRUE or FALSE) indicating whether to weight the responses. If TRUE (default is FALSE) then quadratic weights are utilized. This option should be set to TRUE for ordinal or continuous responses. Ignored when `weights` is given.
#' @param weights Optional weighting scheme, as in [cac_test()]: e.g.
#'   `"quadratic"`, `"linear"` or `"ordinal"`, or a custom weight matrix.
#'   Overrides `weighted`.
#' @param categories All possible rating categories, as in [cac_test()].
#'   Defaults to the sorted observed ratings (or the levels of a factor).
#'
#' @details
#' The estimates, standard errors and intervals are those of [cac_test()];
#' see there for details. Confidence limits are clipped to the possible range
#' of each coefficient. When all ratings are in one category the
#' chance-corrected coefficients are undefined and returned as `NA`.
#'
#' @return A data frame with one row per coefficient and the columns `est`
#'   (estimate), `se` (standard error), `lower.ci` and `upper.ci`.
#' @examples
#' data('reps')
#' agree_coef(data = reps, wide = TRUE, col.names = c("x","y"), weighted = TRUE)
#'
#' @references
#' Gwet, K.L. (2014, ISBN:978-0970806284). "Handbook of Inter-Rater Reliability", 4th Edition. Advanced Analytics, LLC.
#'
#' Gwet, K. L. (2008). "Computing inter-rater reliability and its variance in the presence of high agreement," British Journal of Mathematical and Statistical Psychology, 61, 29-48.
#' @seealso [cac_test()]
#' @export

agree_coef = function(wide = TRUE,
                      col.names = NULL,
                      measure,
                      item,
                      id,
                      data,
                      weighted = FALSE,
                      conf.level = .95,
                      weights = NULL,
                      categories = NULL){

  .check_conf_level(conf.level)
  if (wide) {
    if (is.null(col.names)) {
      stop("Must provide column names (col.names) if wide = TRUE")
    }
    prep <- .reli_prepare(data = data, col.names = col.names,
                          allow_categorical = TRUE)
  } else {
    if (missing(measure) || missing(item) || missing(id)) {
      stop("Must provide 'measure', 'item' and 'id' if wide = FALSE")
    }
    prep <- .reli_prepare(data = data, measure = measure, item = item, id = id,
                          allow_categorical = TRUE)
  }
  if (is.null(weights)) {
    weights <- if (weighted) "quadratic" else "unweighted"
  } else if (is.character(weights)) {
    weights <- match.arg(weights, .cac_weight_types)
  }
  if (is.null(categories) && is.factor(prep$df$values)) {
    categories <- levels(prep$df$values)
  }

  p <- .cac_prep(.cac_ratings_matrix(prep$df), categories = categories,
                 weights = weights)
  if (p$q < 2) {
    warning("All ratings are in one category; the chance-corrected ",
            "coefficients are undefined (NA).", call. = FALSE)
  }
  coefs <- c("pa", "gwet", "fleiss", "krippendorff")
  rows <- lapply(coefs, function(coef) {
    fit <- .cac_coef(p, coef)
    ci <- if (is.na(fit$se)) c(NA_real_, NA_real_) else
      .cac_inference(fit, coef, conf.level, "two.sided")$conf.int
    data.frame(est = fit$est, se = fit$se,
               lower.ci = ci[1], upper.ci = ci[2])
  })
  res <- do.call(rbind, rows)
  weighted <- p$weights_name != "unweighted"
  rownames(res) <- c("Percent Agreement",
                     if (weighted) "Gwet's AC2" else "Gwet's AC1",
                     "Fleiss' Kappa",
                     "Krippendorff's Alpha")
  res
}
