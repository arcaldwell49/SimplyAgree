#' @title Chance-Corrected Agreement Coefficients for Categorical Ratings
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Estimates and tests one agreement coefficient for categorical (nominal or
#' ordinal) ratings of the same subjects by two or more raters: percent
#' agreement, Gwet's AC1 (AC2 when weighted), Fleiss' kappa, or
#' Krippendorff's alpha. Missing ratings are allowed. For ordinal ratings
#' from many raters, see also the model-based kappa in [ord_kappa_test()].
#'
#' @param x A formula of the form `values ~ rater | id`. Leave `NULL` to use
#'   `measure`, `item` and `id` (long data) or `col.names` (wide data).
#' @param data A data frame.
#' @param measure,item,id Names of the columns with the ratings, the raters,
#'   and the subjects (long data).
#' @param col.names Names of the columns with each rater's ratings (wide data,
#'   one row per subject).
#' @param coef The coefficient: `"gwet"` (Gwet's AC1/AC2; default),
#'   `"fleiss"` (Fleiss' kappa), `"krippendorff"` (Krippendorff's alpha) or
#'   `"pa"` (percent agreement, not corrected for chance).
#' @param weights How partial agreement between different categories is
#'   credited: `"unweighted"` (default; only identical ratings agree),
#'   `"quadratic"`, `"linear"`, `"ordinal"`, `"radical"`, `"ratio"`,
#'   `"circular"` or `"bipolar"` (Gwet, 2014, ch. 3), or a custom q x q
#'   matrix with values in \[0, 1\] and 1 on the diagonal, ordered as
#'   `categories`. Use weights only for ordered categories.
#' @param categories All possible rating categories, in order. Defaults to
#'   the levels of a factor, and otherwise to the sorted observed ratings.
#'   Give it when some categories of the scale were never used: Gwet's AC1/AC2
#'   and the weights depend on the full set of categories.
#' @param conf.level Confidence level of the interval (default 0.95).
#' @param null.value The hypothesized value of the coefficient. `NULL`
#'   (default) gives no test.
#' @param alternative The alternative hypothesis: `"two.sided"` (default),
#'   `"greater"` or `"less"`. Without a `null.value` the interval is
#'   two-sided.
#'
#' @details
#'
#' All four coefficients share the observed agreement
#' \eqn{p_a}, the proportion of agreeing pairs of ratings within subjects
#' (weighted when `weights` is used), averaged over the subjects with two or
#' more ratings. The chance-corrected coefficients are
#' \eqn{(p_a - p_e)/(1 - p_e)} and differ in the chance agreement \eqn{p_e}:
#' Fleiss' kappa and Krippendorff's alpha use the overall category
#' proportions, so they are low when one category dominates even if raters
#' rarely disagree (the "kappa paradox"); Gwet's AC1 uses the chance that a
#' rater classifies at random and is stable in that situation (Gwet, 2008).
#' Krippendorff's alpha also corrects for the number of pairs.
#'
#' Standard errors come from Gwet's (2008, 2014) linearisation
#' over subjects, which treats the subjects as a random sample and the raters
#' as fixed. The interval and test use the t distribution with n - 1 degrees
#' of freedom (n subjects; for Krippendorff's alpha, those with 2 or more
#' ratings), and limits are clipped to the possible range of the coefficient.
#' These match the irrCAC package (with an infinite population).
#'
#' Ratings may be numeric, character or factor. Subjects with no
#' ratings are dropped; subjects with one rating add to the category
#' proportions but not to the observed agreement. Each subject can have at
#' most one rating per rater.
#'
#' @return A list of class `c("cac_htest", "htest")` containing:
#'   \item{statistic}{The t statistic (only with a `null.value`).}
#'   \item{parameter}{The degrees of freedom (only with a `null.value`).}
#'   \item{p.value}{The p-value (only with a `null.value`).}
#'   \item{conf.int}{The confidence interval for the coefficient.}
#'   \item{estimate}{The estimated coefficient.}
#'   \item{stderr}{The standard error of the coefficient.}
#'   \item{null.value}{The hypothesized value.}
#'   \item{alternative}{The alternative hypothesis.}
#'   \item{method}{A description of the coefficient.}
#'   \item{data.name}{A description of the data.}
#'   \item{fit}{Details: observed (`pa`) and chance (`pe`) agreement, the
#'   categories, the weight matrix, and the numbers of subjects and raters.}
#'   \item{data}{The ratings in long format.}
#'   \item{settings}{The settings used, for the methods.}
#'
#' @examples
#' # Gwet (2014): 12 subjects rated by 4 raters on a 5-point scale
#' ratings <- data.frame(
#'   R1 = c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
#'   R2 = c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, NA),
#'   R3 = c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, 3),
#'   R4 = c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA))
#' cac_test(col.names = c("R1", "R2", "R3", "R4"), data = ratings)
#'
#' # Quadratic weights, Fleiss' kappa, tested against 0.6
#' res <- cac_test(col.names = c("R1", "R2", "R3", "R4"), data = ratings,
#'                 coef = "fleiss", weights = "quadratic",
#'                 null.value = 0.6, alternative = "greater")
#' res
#' summary(res)
#' plot(res)
#'
#' @references
#' Gwet, K. L. (2008). Computing inter-rater reliability and its variance in
#' the presence of high agreement. British Journal of Mathematical and
#' Statistical Psychology, 61(1), 29-48. \doi{10.1348/000711006X126600}
#'
#' Gwet, K. L. (2014). Handbook of Inter-Rater Reliability (4th ed.).
#' Advanced Analytics, LLC. ISBN 978-0970806284.
#'
#' Fleiss, J. L. (1971). Measuring nominal scale agreement among many raters.
#' Psychological Bulletin, 76(5), 378-382. \doi{10.1037/h0031619}
#'
#' Krippendorff, K. (1970). Estimating the reliability, systematic error and
#' random error of interval data. Educational and Psychological Measurement,
#' 30(1), 61-70. \doi{10.1177/001316447003000105}
#'
#' @seealso [cac_htest-methods] for `summary()` and `plot()`;
#'   [ord_kappa_test()] for the model-based kappa for ordinal ratings.
#' @export
cac_test <- function(x = NULL,
                     data = NULL,
                     measure = NULL,
                     item = NULL,
                     id = NULL,
                     col.names = NULL,
                     coef = c("gwet", "fleiss", "krippendorff", "pa"),
                     weights = "unweighted",
                     categories = NULL,
                     conf.level = 0.95,
                     null.value = NULL,
                     alternative = c("two.sided", "greater", "less")) {
  coef <- match.arg(coef)
  alternative <- match.arg(alternative)
  if (is.character(weights)) {
    weights <- match.arg(weights, .cac_weight_types)
  }
  .check_conf_level(conf.level)
  if (is.null(null.value)) {
    alternative <- "two.sided"
  } else {
    bounds <- .cac_bounds(coef)
    if (!is.numeric(null.value) || length(null.value) != 1 ||
        is.na(null.value) || null.value < bounds[1] || null.value >= bounds[2]) {
      stop("'null.value' must be a single number in [", bounds[1], ", ",
           bounds[2], ").", call. = FALSE)
    }
  }

  prep <- .reli_prepare(x = x, data = data, measure = measure, item = item,
                        id = id, col.names = col.names,
                        allow_categorical = TRUE)
  if (is.null(categories) && is.factor(prep$df$values)) {
    categories <- levels(prep$df$values)
  }
  p <- .cac_prep(.cac_ratings_matrix(prep$df), categories = categories,
                 weights = weights)
  if (coef != "pa" && p$q < 2) {
    stop("All ratings are in one category; chance-corrected coefficients ",
         "are undefined. Give the full scale in 'categories', or use ",
         "coef = \"pa\".", call. = FALSE)
  }
  fit <- .cac_coef(p, coef)
  if (is.na(fit$se)) {
    stop("At least 2 subjects are required for a standard error.",
         call. = FALSE)
  }
  inf <- .cac_inference(fit, coef, conf.level, alternative, null.value)

  weighted <- p$weights_name != "unweighted"
  label <- .cac_label(coef, weighted)
  settings <- list(statistic = "cac",
                   coef = coef,
                   weights = p$weights_name,
                   conf.level = conf.level,
                   label = label)
  method <- paste0(if (weighted) paste0("Weighted (", p$weights_name, ") ")
                   else "", label,
                   " for ", p$r, " raters")
  method <- paste0(toupper(substr(method, 1, 1)), substring(method, 2))

  structure(list(statistic = inf$test$statistic,
                 parameter = inf$test$parameter,
                 p.value = inf$test$p.value,
                 conf.int = inf$conf.int,
                 estimate = stats::setNames(fit$est, label),
                 stderr = fit$se,
                 null.value = if (is.null(null.value)) NULL else
                   stats::setNames(null.value, label),
                 alternative = alternative,
                 method = method,
                 data.name = prep$data.name,
                 fit = list(pa = fit$pa,
                            pe = fit$pe,
                            df = fit$df,
                            categories = p$categories,
                            weights = p$weights,
                            n = p$n,
                            r = p$r,
                            N = sum(p$ri),
                            n_single = sum(p$ri == 1)),
                 data = prep$df,
                 settings = settings),
            class = c("cac_htest", "htest"))
}
