#' @title LaTeX Equation of a Fitted Model
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Writes the statistical model behind a result as a LaTeX equation: the mean
#' model, the distributions of the random effects and residuals, and any
#' variance function or correlation structure. The equation uses symbols by
#' default; with `use_coefs = TRUE` the estimates are substituted.
#'
#' @param x A result of [tolerance_limit()], [icc_test()], [reli_err_test()],
#'   [mdc_test()], [reli_facet_test()], [reli_stats()], [reli_aov()],
#'   [method_vc()] or [method_convert()], or a model fitted with
#'   [stats::lm()], [lme4::lmer()], [nlme::gls()] or [nlme::lme()].
#' @param use_coefs Logical. If `TRUE`, substitute the estimates for the
#'   symbols (fixed effects, standard deviations of the random effects and
#'   residuals, variance function and correlation parameters). Default is
#'   `FALSE`.
#' @param coef_digits Number of decimal places for the estimates. Default is
#'   3.
#' @param pair For [method_convert()] results with `bias = "linear"`: two
#'   method names to show the regression for one pair only. Default is `NULL`
#'   (all pairs).
#' @param ... Not used.
#'
#' @details
#' The equation is returned as LaTeX code. Printed at the console it shows the
#' code; in an R Markdown or Quarto document, a chunk that returns it renders
#' the equation as display math. Equations with more than one line use an
#' `aligned` environment: the mean model first, then the distributions.
#'
#' Parameters that differ by a level (e.g., the method means
#' \eqn{\alpha_m} or the ratios \eqn{\delta} of a variance function) are
#' written with an index in the equation; with `use_coefs = TRUE` their
#' estimates are listed at the end of the line.
#'
#' The notation follows the vignettes:
#'
#' - `tolerance_limit()`: \eqn{d_{ij}} is the difference for subject
#'   \eqn{i}, measurement \eqn{j}, with the fixed effects, the random
#'   intercepts (`model = "lme"`), and the variance function and correlation
#'   structure of the fitted `nlme` model. Conditions are indicators
#'   \eqn{\mathrm{I}(\cdot)} and \eqn{A} is the average of the two
#'   measurements. The model must have been kept (`keep_model = TRUE`).
#' - `icc_test()`, `reli_err_test()`, `mdc_test()`, `reli_stats()`,
#'   `reli_aov()`: \eqn{y_{ij} = \mu + s_i + t_j + e_{ij}} (one-way:
#'   without \eqn{t_j}). The same model underlies the consistency and
#'   agreement forms; the consistency forms do not use \eqn{\sigma^2_t}.
#' - `reli_facet_test()`: an effect for subjects (\eqn{p}), items
#'   (\eqn{i}), occasions (\eqn{o}) and their interactions, and repeat error
#'   \eqn{e}.
#' - `method_vc()`: \eqn{y_{mir} = \alpha_m + \mu_i + a_{ir} + c_{mi} +
#'   e_{mir}} for method \eqn{m}, item \eqn{i}, replicate \eqn{r}, on the
#'   analysis scale of the transformation, with the designs without
#'   replicates as special cases.
#' - `method_convert()`: the regression of the differences \eqn{D} on the
#'   averages \eqn{A} for each pair (`bias = "linear"`), the scaled model
#'   \eqn{y_{mir} = \alpha_m + \beta_m(\mu_i + a_{ir} + c_{mi}) + e_{mir}}
#'   (`bias = "scaled"`), or the variance component model (from a
#'   `method_vc()` result).
#'
#' For `lm`, `lmerMod`, `gls` and `lme` models, the fixed effects are
#' \eqn{\beta_0, \beta_1, \ldots}, factor levels are indicators, and a factor
#' with many levels is shown as one indexed effect \eqn{\gamma}. Random
#' effects are \eqn{u}, \eqn{v}, ... for the grouping factors, indexed
#' \eqn{j[i]}, \eqn{k[i]}, ... (the group of observation \eqn{i}). The
#' variance functions `varIdent`, `varPower`, `varExp` and `varFixed`, and the
#' correlation structures `corCompSymm`, `corAR1` and `corCAR1` are written
#' out; others are shown generically. Generalized linear models are not
#' supported.
#'
#' @return A character string of class `c("model_equation", "character")`
#'   with the LaTeX code (without math delimiters).
#'
#' @examples
#' data('reps')
#' tl = tolerance_limit(data = reps, x = "x", y = "y", id = "id")
#' model_equation(tl)
#' model_equation(tl, use_coefs = TRUE)
#'
#' fit = lm(mpg ~ wt + factor(cyl), data = mtcars)
#' model_equation(fit, use_coefs = TRUE, coef_digits = 2)
#'
#' @seealso [tolerance_limit()], [icc_test()], [method_vc()],
#'   [method_convert()]
#' @export
model_equation <- function(x, ...) {
  UseMethod("model_equation")
}

#' @rdname model_equation
#' @export
model_equation.default <- function(x, ...) {
  stop("model_equation() has no method for objects of class '",
       class(x)[1], "'.", call. = FALSE)
}

#' @rdname model_equation
#' @export
model_equation.lm <- function(x,
                              use_coefs = FALSE,
                              coef_digits = 3,
                              ...) {
  .eq_finish(.eq_from_lm(x), use_coefs, coef_digits)
}

#' @rdname model_equation
#' @export
model_equation.lmerMod <- function(x,
                                   use_coefs = FALSE,
                                   coef_digits = 3,
                                   ...) {
  .eq_finish(.eq_from_lmer(x), use_coefs, coef_digits)
}

#' @rdname model_equation
#' @export
model_equation.gls <- function(x,
                               use_coefs = FALSE,
                               coef_digits = 3,
                               ...) {
  .eq_finish(.eq_from_gls(x), use_coefs, coef_digits)
}

#' @rdname model_equation
#' @export
model_equation.lme <- function(x,
                               use_coefs = FALSE,
                               coef_digits = 3,
                               ...) {
  .eq_finish(.eq_from_lme(x), use_coefs, coef_digits)
}

#' @rdname model_equation
#' @export
print.model_equation <- function(x, ...) {
  cat(x, "\n", sep = "")
  invisible(x)
}

# display math in R Markdown and Quarto; inline = TRUE for inline math
#' @exportS3Method knitr::knit_print
#' @noRd
knit_print.model_equation <- function(x,
                                      ...,
                                      inline = FALSE) {
  out = if (inline) {
    paste0("$", x, "$")
  } else {
    paste0("$$\n", x, "\n$$")
  }
  knitr::asis_output(out)
}
