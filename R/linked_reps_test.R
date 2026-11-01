#' Permutation Test of Linked Replicates
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Checks whether treating the replicates as linked matters for a
#' [method_vc()] fit. The replicate labels are permuted at random within each
#' item and method, which breaks any link between the methods' replicates,
#' and the model is refitted with linked replicates. Under no occasion
#' (item-by-replicate) effect, the observed `omega` is like those of the
#' permuted data; a larger observed `omega` is evidence that the replicates
#' are linked. The limits of the permuted fits show how much the conclusions
#' depend on the linking (MethComp's `perm.repl()`; Carstensen, 2010).
#'
#' @param object A `simple_mvc` object from [method_vc()] fitted with
#'   `replicates = "linked"`.
#' @param n_perm Number of permutations (default 100).
#' @param seed Optional seed; the global random number generator is left as
#'   it was.
#'
#' @details
#' The p-value is `(1 + #{omega_perm >= omega}) / (n_perm + 1)`, one-sided.
#' Permutations that fail to fit are dropped (with a warning). The other
#' settings of `object` (`tau`, `interval`, `transform`, levels) are kept.
#'
#' A small p-value supports `replicates = "linked"`. A large one means the
#' data do not show an occasion effect; the linked and exchangeable analyses
#' then usually give similar limits, which the `limits` table shows.
#'
#' @return A list with class `c("mvc_perm_htest", "htest")`:
#'   \item{statistic}{The observed `omega` (SD of the item-by-replicate
#'   effects).}
#'   \item{parameter}{The number of successful permutations.}
#'   \item{p.value}{The permutation p-value.}
#'   \item{estimate}{The mean `omega` of the permuted fits.}
#'   \item{method, data.name}{Descriptions.}
#'   \item{limits}{For each pair of methods: the observed SD of the
#'   differences and limits, and their mean and 2.5% and 97.5% quantiles
#'   over the permutations.}
#'   \item{omega_perm}{The `omega` of each permuted fit.}
#'
#' @references
#' Carstensen, B. (2010). Comparing Clinical Measurement Methods: A Practical
#' Guide. Wiley.
#'
#' @seealso [method_vc()]
#'
#' @examples
#' dat = sim_methods(n_items = 20, n_reps = 3, methods = c("A", "B"),
#'                   alpha = c(0, 0.5), tau = 0.3, omega = 0.4,
#'                   sigma = 0.3, replicates = "linked", seed = 1)
#' fit = method_vc(dat, y = "y", method = "method", id = "id",
#'                 replicate = "repl", replicates = "linked")
#' linked_reps_test(fit, n_perm = 20, seed = 1)
#'
#' @export
linked_reps_test <- function(object, n_perm = 100, seed = NULL) {
  if (!inherits(object, "simple_mvc")) {
    stop("object must be a simple_mvc object from method_vc().")
  }
  if (!isTRUE(object$design$linked) || !isTRUE(object$design$has_reps)) {
    stop("object must be fitted with replicates and replicates = 'linked'.")
  }
  if (!is.numeric(n_perm) || length(n_perm) != 1 || n_perm < 1) {
    stop("n_perm must be a positive number.")
  }
  n_perm <- as.integer(n_perm)

  omega_of <- function(fit) {
    vc <- fit$varcomp
    unname(vc$sd[vc$component == "omega"][1])
  }
  dat <- as.data.frame(object$data)[, c("y", "method", "id", "repl")]
  args <- as.list(object$call)[-1]
  args <- args[setdiff(names(args), c("data", "y", "method", "id",
                                      "replicate", "keep_model"))]
  refit <- function(d) {
    do.call(method_vc, c(list(data = d, y = "y", method = "method",
                              id = "id", replicate = "repl",
                              keep_model = FALSE), args))
  }
  limit_cols <- setdiff(names(object$limits),
                        c("method1", "method2", "bias", "df_sd"))
  limit_cols <- limit_cols[!grepl("_ci$", limit_cols)]

  perms <- mc_with_seed(seed, lapply(seq_len(n_perm), function(k) {
    d <- dat
    grp <- interaction(d$id, d$method, drop = TRUE)
    d$repl <- stats::ave(d$repl, grp, FUN = function(r) {
      if (length(r) > 1) r[sample.int(length(r))] else r
    })
    tryCatch(suppressWarnings(suppressMessages(refit(d))),
             error = function(e) NULL)
  }))
  ok <- !vapply(perms, is.null, logical(1))
  if (sum(ok) < n_perm) {
    warning(n_perm - sum(ok), " of ", n_perm, " permuted fits failed and ",
            "were dropped.")
  }
  if (!any(ok)) stop("All permuted fits failed.")
  perms <- perms[ok]

  omega <- omega_of(object)
  omega_perm <- vapply(perms, omega_of, numeric(1))
  p <- (1 + sum(omega_perm >= omega - 1e-12 * max(omega, 1))) /
    (length(omega_perm) + 1)

  lim <- object$limits
  rows <- list()
  for (i in seq_len(nrow(lim))) {
    for (col in limit_cols) {
      v <- vapply(perms, function(f) {
        l <- f$limits
        j <- which(l$method1 == lim$method1[i] & l$method2 == lim$method2[i])
        if (length(j)) l[[col]][j] else NA_real_
      }, numeric(1))
      rows[[length(rows) + 1]] <- data.frame(
        method1 = lim$method1[i], method2 = lim$method2[i], quantity = col,
        observed = lim[[col]][i], perm_mean = mean(v, na.rm = TRUE),
        perm_lower = unname(stats::quantile(v, 0.025, na.rm = TRUE)),
        perm_upper = unname(stats::quantile(v, 0.975, na.rm = TRUE)),
        stringsAsFactors = FALSE)
    }
  }
  limits <- do.call(rbind, rows)

  structure(
    list(statistic = c(omega = omega),
         parameter = c(permutations = length(omega_perm)),
         p.value = p,
         estimate = c("mean permuted omega" = mean(omega_perm)),
         alternative = "greater",
         method = "Permutation test of linked replicates (occasion effect)",
         data.name = paste(deparse(object$call$data), collapse = " "),
         limits = limits,
         omega_perm = omega_perm),
    class = c("mvc_perm_htest", "htest")
  )
}

#' @rdname linked_reps_test
#' @param x An `mvc_perm_htest` object.
#' @param digits Number of significant digits to print.
#' @param ... Passed to the htest print method.
#' @method print mvc_perm_htest
#' @export
print.mvc_perm_htest <- function(x, digits = 4, ...) {
  NextMethod()
  cat("Limits, observed and over the permutations (mean, 2.5% - 97.5%):\n")
  tab <- x$limits
  num <- c("observed", "perm_mean", "perm_lower", "perm_upper")
  tab[num] <- lapply(tab[num], signif, digits = digits)
  print(tab, row.names = FALSE)
  cat("\n")
  invisible(x)
}
