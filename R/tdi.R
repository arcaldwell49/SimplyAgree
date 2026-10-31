#' @title Total Deviation Index and Coverage Probability
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The total deviation index (TDI) is the value that a proportion `p` of the
#' absolute differences between two methods do not exceed: P(|D| <= TDI) = p
#' (Lin, 2000). The coverage probability (CP) is the reverse: the proportion of
#' absolute differences that do not exceed a given value `delta`. `tdi()`
#' estimates both from a fitted agreement analysis, with a one-sided upper
#' confidence bound for the TDI and a lower bound for the CP, and optionally
#' tests whether the TDI is below `delta`.
#'
#' @param object An object from [agreement_limit()] (class `loa`),
#'   [tolerance_limit()] (class `tolerance_delta`), or [method_vc()] (class
#'   `simple_mvc`).
#' @param p Proportion of absolute differences that the TDI covers. Default
#'   (`NULL`) is the agreement or prediction level of `object`
#'   (`agree.level`, `pred_level`), so that the TDI is comparable with the
#'   limits it reports.
#' @param delta Optional maximal allowable difference. When given, the CP of
#'   `delta` and its lower bound are reported, and `H0: TDI >= delta` is tested
#'   against `H1: TDI < delta` (equivalently, `H0: CP(delta) <= p`). On the
#'   log scale (`log_tf = TRUE` or `transform = "log"`), `delta` is a ratio
#'   greater than 1.
#' @param conf.level Confidence level of the one-sided bounds. Default
#'   (`NULL`) is the confidence level of `object`.
#' @param method How the bounds are computed: `"gpq"` (default), generalized
#'   pivotal quantities; `"delta"`, the delta method on the log scale (Lin,
#'   2000); or `"tolerance"`, a one-sided tolerance bound for the quantile
#'   of the differences that defines the TDI (Escaramis et al., 2010). See Details.
#' @param B Number of draws for `method = "gpq"`. Default is 10000.
#' @param seed Optional seed for `method = "gpq"`; the global random number
#'   state is restored afterwards.
#' @param ... Not used.
#'
#' @details
#' For differences `D ~ N(mu, sigma^2)`, the TDI is computed exactly,
#' `sigma * sqrt(qchisq(p, 1, ncp = (mu / sigma)^2))`, the `p` quantile of
#' `|D|`. Lin's (2000) approximation, `qnorm((1 + p) / 2) * sqrt(mu^2 +
#' sigma^2)`, is reported for reference; it overstates the TDI when the bias
#' is large relative to the SD. The CP of `delta` is
#' `pnorm((delta - mu) / sigma) - pnorm((-delta - mu) / sigma)`.
#'
#' `mu` and `sigma` are the bias and the SD of a single difference reported by
#' `object`, with their degrees of freedom: for `agreement_limit()`, `df` and
#' `df_sd` of the `loa` table (`df_sd` is a Satterthwaite df for replicated
#' and nested data); for `tolerance_limit()`, `df` and `SD.df`; for
#' `method_vc()`, the `df` of the bias and `df_sd`. With a bias that depends
#' on the level (`prop_bias = TRUE`), several conditions, or several pairs of
#' methods, the TDI is computed for every row, and the printed estimate and
#' bound are those of the row with the largest upper bound.
#'
#' Bounds:
#' - `"gpq"`: generalized pivotal quantities (Krishnamoorthy & Mathew, 2009).
#'   The variance of a single difference is split into independent pieces
#'   `v_k` with `df_k` degrees of freedom, each drawn as
#'   `v_k* = v_k df_k / U_k` with `U_k` chi-square on `df_k` df, and
#'   `sigma*^2 = sum(v_k*)`. For independent differences there is one piece,
#'   the variance itself. For `agreement_limit(data_type = "reps")` the pieces
#'   are the variance of the subject mean differences and the rest of the
#'   within-subject variance of each method; for `data_type = "nest"` and
#'   `tolerance_limit()` with subjects, the variance of a subject mean and the
#'   remaining within-subject (and nested) variance. The standard error of
#'   the bias depends on the first piece, so `mu* = mu - Z SE sqrt(v_1* /
#'   v_1)`. For `method_vc()` and other correlation structures, the variance
#'   is drawn on its Satterthwaite df and `mu* = mu - T SE`, with `T` on the
#'   df of the bias. The bound for the TDI is the `conf.level`
#'   quantile of `TDI(mu*, sigma*)`, the bound for the CP the `1 -
#'   conf.level` quantile of `CP(delta; mu*, sigma*)`, and the p-value is the
#'   proportion of draws with `TDI(mu*, sigma*) >= delta`.
#' - `"delta"`: the delta method for `log(TDI)` (and `logit(CP)`), with
#'   `var(mu) = SE^2`, `var(sigma^2) = 2 sigma^4 / nu`, and `t` quantiles on
#'   `nu` df.
#' - `"tolerance"`: Escaramis et al. (2010). The TDI is written as
#'   `|mu| + z_q sigma`, where `q` solves
#'   `pnorm(z_q) - pnorm(-z_q - 2 |mu| / sigma) = p`, and the bound is the
#'   one-sided `conf.level` upper tolerance bound for the `q` quantile: the
#'   exact noncentral t bound for independent differences, as in
#'   `tolerance_limit(bound_type = "iu")`, or its MOVER bound with subjects.
#'   `q` is estimated, so the bound is approximate. TDIagree's version of this
#'   bound counts measurements rather than differences (`n` = 2 x subjects,
#'   `df` = subjects), which makes it narrower; here the SE of the bias and the
#'   df of the SD of the differences are used. The CP bound is found by
#'   inverting the TDI bound in `p`.
#'
#' @return A list with class `c("tdi_htest", "htest")`:
#'   \item{estimate}{The TDI (as a ratio on the log scale).}
#'   \item{conf.int}{The one-sided interval from 0 (1 on the log scale) to the
#'   upper bound, with attribute `conf.level`.}
#'   \item{statistic, parameter}{With `delta` and `method = "delta"`, the t
#'   statistic and its df.}
#'   \item{p.value, null.value, alternative}{With `delta`, the test of
#'   `H0: TDI >= delta`; otherwise `NULL`.}
#'   \item{cp}{With `delta`, the CP of `delta` and its lower bound.}
#'   \item{lin_approx}{Lin's approximation to the TDI.}
#'   \item{table}{One row per bias row of `object`: the bias, SD, df, TDI,
#'   bound, Lin's approximation and, with `delta`, the CP, its bound, and the
#'   p-value.}
#'   \item{method, data.name, settings, plot_data}{Description and settings.}
#'
#' @references
#' Lin, L. I. (2000). Total deviation index for measuring individual agreement
#' with applications in laboratory performance and bioequivalence.
#' Statistics in Medicine, 19(2), 255-270.
#'
#' Lin, L., Hedayat, A. S., Sinha, B., & Yang, M. (2002). Statistical methods
#' in assessing agreement: models, issues, and tools. Journal of the American
#' Statistical Association, 97(457), 257-270.
#' \doi{10.1198/016214502753479392}
#'
#' Choudhary, P. K., & Nagaraja, H. N. (2007). Tests for assessment of
#' agreement using probability criteria. Journal of Statistical Planning and
#' Inference, 137(1), 279-290. \doi{10.1016/j.jspi.2005.11.009}
#'
#' Escaramis, G., Ascaso, C., & Carrasco, J. L. (2010). The total deviation
#' index estimated by tolerance intervals to evaluate the concordance of
#' measurement devices. BMC Medical Research Methodology, 10, 31.
#' \doi{10.1186/1471-2288-10-31}
#'
#' Krishnamoorthy, K., & Mathew, T. (2009). Statistical Tolerance Regions:
#' Theory, Applications, and Computation. Wiley.
#'
#' @seealso [agreement_limit()], [tolerance_limit()], [method_vc()]
#'
#' @examples
#' data("reps")
#' fit <- agreement_limit(x = "x", y = "y", data = reps)
#' tdi(fit, seed = 1)
#' tdi(fit, delta = 2, seed = 1)
#' tdi(fit, p = 0.9, method = "delta")
#'
#' @export
tdi <- function(object, ...) {
  UseMethod("tdi")
}

#' @rdname tdi
#' @export
tdi.default <- function(object, ...) {
  stop("tdi() has methods for agreement_limit(), tolerance_limit(), ",
       "and method_vc() results, not for objects of class ",
       paste(class(object), collapse = "/"), ".")
}

#' @rdname tdi
#' @method tdi loa
#' @export
tdi.loa <- function(object, p = NULL, delta = NULL, conf.level = NULL,
                    method = c("gpq", "delta", "tolerance"), B = 10000,
                    seed = NULL, ...) {
  method <- match.arg(method)
  cl <- object$call
  l <- object$loa
  if (is.null(l$df_sd)) {
    stop("This agreement_limit() result has no df_sd column; ",
         "refit it with this version of SimplyAgree.")
  }
  prop_bias <- isTRUE(eval(cl$prop_bias))
  inputs <- data.frame(
    panel = "all",
    x_at = if (prop_bias) as.numeric(l$avg) else NA_real_,
    bias = l$bias,
    se = l$SE,
    df_bias = l$df,
    sd = l$sd_delta,
    df_sd = l$df_sd,
    scale_sd = identical(cl$data_type, "simple"),
    stringsAsFactors = FALSE
  )
  info <- list(type = "df", nu = inputs$df_sd)
  # independent variance components for the GPQ (one for simple data)
  comps <- if (!is.null(object$var_comp)) {
    rep(list(c(object$var_comp, scale = TRUE)), nrow(inputs))
  } else {
    .tdi_comps(inputs)
  }
  pd <- stats::model.frame(cl$lm_mod)
  .tdi_run(inputs = inputs, info = info, comps = comps,
           p = if (is.null(p)) cl$agree.level else p,
           delta = delta,
           conf.level = if (is.null(conf.level)) cl$conf.level else conf.level,
           method = method, B = B, seed = seed,
           log_scale = isTRUE(eval(cl$log_tf)),
           plot_data = data.frame(panel = "all", avg = pd$avg, delta = pd$delta),
           source = "agreement_limit()",
           data.name = paste(deparse(substitute(object)), collapse = " "))
}

#' @rdname tdi
#' @method tdi tolerance_delta
#' @export
tdi.tolerance_delta <- function(object, p = NULL, delta = NULL,
                                conf.level = NULL,
                                method = c("gpq", "delta", "tolerance"),
                                B = 10000, seed = NULL, ...) {
  method <- match.arg(method)
  cl <- object$call
  l <- as.data.frame(object$limits)
  model <- object$model
  has_cond <- "condition" %in% names(l) && !all(is.na(l$condition))
  has_avg <- "avg" %in% names(l) && !all(is.na(l$avg))
  independent <- if (!is.null(model)) {
    !inherits(model, "lme") && is.null(model$modelStruct$corStruct)
  } else {
    is.null(cl$id)
  }
  inputs <- data.frame(
    panel = if (has_cond) as.character(l$condition) else "all",
    x_at = if (has_avg) as.numeric(l$avg) else NA_real_,
    bias = l$bias,
    se = l$SEM,
    df_bias = l$df,
    sd = l$SD,
    df_sd = l$SD.df,
    scale_sd = independent,
    stringsAsFactors = FALSE
  )
  # the bound on the SD used by tolerance_limit() itself (MOVER for variance
  # components); a chi-square bound on SD.df without the model
  info <- list(type = "df", nu = inputs$df_sd)
  if (!is.null(model)) {
    info <- tryCatch(sd_bound_info(model = model, grid = l,
                                   data = nlme::getData(model)),
                     error = function(e) info)
  } else if (method == "tolerance") {
    message("The model was not kept (keep_model = FALSE); the tolerance ",
            "bound uses a chi-square bound on SD.df.")
  }
  # with variance components, the GPQ draws each piece of the variance on its
  # own df; the first (the variance of a subject mean) gives the SE of the
  # bias
  comps <- if (info$type == "comp") {
    lapply(seq_len(nrow(inputs)), function(j) {
      ij <- sd_info_row(info, j)
      list(v = as.vector(ij$shares) * inputs$sd[j]^2,
           df = as.vector(ij$df), scale = TRUE)
    })
  } else {
    .tdi_comps(inputs)
  }
  pd <- stats::model.frame(cl$lm_mod)
  plot_data <- data.frame(panel = if (has_cond) as.character(pd$condition) else "all",
                          avg = pd$avg, delta = pd$delta)
  .tdi_run(inputs = inputs, info = info, comps = comps,
           p = if (is.null(p)) cl$pred_level else p,
           delta = delta,
           conf.level = if (is.null(conf.level)) cl$tol_level else conf.level,
           method = method, B = B, seed = seed,
           log_scale = isTRUE(eval(cl$log_tf)),
           plot_data = plot_data,
           source = "tolerance_limit()",
           data.name = paste(deparse(substitute(object)), collapse = " "))
}

#' @rdname tdi
#' @param pair_seed For `method_vc()` results with exchangeable replicates:
#'   seed for the random pairing of replicates in the plot data.
#' @method tdi simple_mvc
#' @export
tdi.simple_mvc <- function(object, p = NULL, delta = NULL, conf.level = NULL,
                           method = c("gpq", "delta", "tolerance"),
                           B = 10000, seed = NULL, pair_seed = NULL, ...) {
  method <- match.arg(method)
  d <- object$design
  l <- as.data.frame(object$limits)
  b <- as.data.frame(object$bias)
  b <- b[match(paste(l$method1, l$method2), paste(b$method1, b$method2)), ]
  panel <- paste(l$method1, "-", l$method2)
  inputs <- data.frame(
    panel = panel,
    x_at = NA_real_,
    bias = l$bias,
    se = b$se,
    df_bias = b$df,
    sd = l$sd,
    df_sd = l$df_sd,
    scale_sd = identical(d$model_type, "single_2"),
    stringsAsFactors = FALSE
  )
  info <- list(type = "df", nu = inputs$df_sd)
  tf_name <- if (is.null(object$transform$name)) "none" else object$transform$name
  plot_data <- do.call(rbind, lapply(seq_len(nrow(l)), function(j) {
    pr <- mc_pair(object$data, as.character(l$method1[j]),
                  as.character(l$method2[j]),
                  replicates = d$replicates, seed = pair_seed)
    data.frame(panel = panel[j], avg = pr$avg, delta = pr$d)
  }))
  res <- .tdi_run(inputs = inputs, info = info, comps = .tdi_comps(inputs),
                  p = if (is.null(p)) d$pred_level else p,
                  delta = delta,
                  conf.level = if (is.null(conf.level)) d$conf_level else conf.level,
                  method = method, B = B, seed = seed,
                  log_scale = identical(tf_name, "log"),
                  plot_data = plot_data,
                  source = "method_vc()",
                  data.name = paste(deparse(substitute(object)), collapse = " "))
  if (!tf_name %in% c("none", "log")) {
    res$settings$scale_note <- paste0("The TDI is on the ", tf_name,
                                      " scale of the analysis.")
  }
  res
}

# Engine ----------------------------------------------------------------------

# exact TDI: the p quantile of |D| for D ~ N(mu, sigma^2),
# sigma * sqrt(qchisq(p, 1, ncp = (mu / sigma)^2)). qchisq() with a
# noncentrality parameter is slow (about 0.5 ms per value), so for p >= 0.5
# the standardized root c of g(c) = pnorm(c - z) + pnorm(c + z) - 1 - p,
# z = |mu| / sigma, is found by Newton's method. Both z + qnorm(p) and
# qnorm((1 + p) / 2) are below the root, and g is increasing and concave for
# c >= z, so the iterates increase monotonically to the root.
tdi_exact <- function(mu, sigma, p) {
  if (p < 0.5) {
    return(sigma * sqrt(stats::qchisq(p, df = 1, ncp = (mu / sigma)^2)))
  }
  z <- abs(mu) / sigma
  cc <- pmax(z + stats::qnorm(p), stats::qnorm((1 + p) / 2))
  for (i in seq_len(100)) {
    g <- stats::pnorm(cc - z) + stats::pnorm(cc + z) - 1 - p
    step <- g / (stats::dnorm(cc - z) + stats::dnorm(cc + z))
    cc <- cc - step
    if (all(abs(step) < 1e-12 * pmax(cc, 1))) break
  }
  sigma * cc
}

# Lin's (2000) approximation
tdi_lin <- function(mu, sigma, p) {
  stats::qnorm((1 + p) / 2) * sqrt(mu^2 + sigma^2)
}

# coverage probability of delta
cp_exact <- function(delta, mu, sigma) {
  stats::pnorm((delta - mu) / sigma) - stats::pnorm((-delta - mu) / sigma)
}

# one variance component per row: the total variance on df_sd
.tdi_comps <- function(inputs) {
  lapply(seq_len(nrow(inputs)), function(j) {
    list(v = inputs$sd[j]^2, df = inputs$df_sd[j],
         scale = isTRUE(inputs$scale_sd[j]))
  })
}

.tdi_run <- function(inputs, info, comps, p, delta, conf.level, method, B, seed,
                     log_scale, plot_data, source, data.name) {
  if (!is.numeric(p) || length(p) != 1 || p <= 0 || p >= 1) {
    stop("p must be a single number between 0 and 1.")
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1 ||
      conf.level <= 0.5 || conf.level >= 1) {
    stop("conf.level must be a single number between 0.5 and 1.")
  }
  if (!is.null(delta)) {
    if (!is.numeric(delta) || length(delta) != 1 || is.na(delta)) {
      stop("delta must be a single number.")
    }
    if (log_scale) {
      if (delta <= 1) stop("On the log scale, delta is a ratio greater than 1.")
      delta_a <- log(delta)
    } else {
      if (delta <= 0) stop("delta must be positive.")
      delta_a <- delta
    }
  } else {
    delta_a <- NULL
  }
  if (method == "gpq" && (!is.numeric(B) || B < 100)) {
    stop("B must be at least 100.")
  }

  if (method == "gpq") {
    if (!is.null(seed)) {
      if (exists(".Random.seed", envir = globalenv())) {
        old_seed <- get(".Random.seed", envir = globalenv())
        on.exit(assign(".Random.seed", old_seed, envir = globalenv()),
                add = TRUE)
      } else {
        on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
      }
      set.seed(seed)
    }
  }

  n_row <- nrow(inputs)
  out <- vector("list", n_row)
  no_df <- !is.finite(inputs$df_sd) | !is.finite(inputs$se)
  if (any(no_df)) {
    warning("The degrees of freedom of the SD (or the SE of the bias) are ",
            "unavailable for ", sum(no_df), " row(s), as in ", source,
            ", which then gives no confidence bounds; the TDI is estimated ",
            "but its bound is not computed.", call. = FALSE)
  }
  for (j in seq_len(n_row)) {
    r <- inputs[j, ]
    info_j <- sd_info_row(info, j)
    if (no_df[j]) {
      out[[j]] <- list(upper = NA_real_, cp_lower = NA_real_,
                       p.value = NA_real_)
      next
    }
    out[[j]] <- switch(method,
                       gpq = .tdi_gpq(r, comps[[j]], p, delta_a, conf.level, B),
                       delta = .tdi_delta(r, p, delta_a, conf.level),
                       tolerance = .tdi_tol(r, info_j, p, delta_a, conf.level))
  }
  tab <- data.frame(panel = inputs$panel,
                    avg = inputs$x_at,
                    bias = inputs$bias,
                    sd = inputs$sd,
                    df_sd = inputs$df_sd,
                    tdi = tdi_exact(inputs$bias, inputs$sd, p),
                    tdi_upper = vapply(out, `[[`, numeric(1), "upper"),
                    tdi_lin = tdi_lin(inputs$bias, inputs$sd, p),
                    stringsAsFactors = FALSE)
  if (!is.null(delta_a)) {
    tab$cp <- cp_exact(delta_a, inputs$bias, inputs$sd)
    tab$cp_lower <- vapply(out, `[[`, numeric(1), "cp_lower")
    tab$p.value <- vapply(out, `[[`, numeric(1), "p.value")
  }
  if (log_scale) {
    tab$tdi_ratio <- exp(tab$tdi)
    tab$tdi_upper_ratio <- exp(tab$tdi_upper)
  }

  # report the row with the largest bound (all rows must meet the criterion)
  # (no bound when any row has none: the criterion cannot then be checked)
  w <- if (all(is.na(tab$tdi_upper))) which.max(tab$tdi) else which.max(tab$tdi_upper)
  tr <- if (log_scale) exp else identity
  est_name <- if (log_scale) "TDI (ratio)" else "TDI"
  estimate <- stats::setNames(tr(tab$tdi[w]), est_name)
  upper_w <- if (anyNA(tab$tdi_upper)) NA_real_ else tab$tdi_upper[w]
  conf.int <- structure(c(tr(0), tr(upper_w)), conf.level = conf.level)

  statistic <- parameter <- p.value <- null.value <- cp <- NULL
  if (!is.null(delta_a)) {
    p.value <- max(tab$p.value)
    null.value <- stats::setNames(delta, est_name)
    cp <- c(CP = tab$cp[w], lower = tab$cp_lower[w])
    if (method == "delta" && !anyNA(tab$p.value)) {
      wp <- which.max(tab$p.value)
      statistic <- c(t = out[[wp]]$statistic)
      parameter <- c(df = inputs$df_sd[wp])
    }
  }

  method_text <- paste0("Total deviation index (p = ", format(p), "; ",
                        switch(method,
                               gpq = "generalized pivotal quantity bound",
                               delta = "delta-method bound",
                               tolerance = "tolerance interval bound"),
                        ")")

  structure(list(statistic = statistic,
                 parameter = parameter,
                 p.value = p.value,
                 conf.int = conf.int,
                 estimate = estimate,
                 null.value = null.value,
                 alternative = "less",
                 method = method_text,
                 data.name = data.name,
                 cp = cp,
                 lin_approx = stats::setNames(tr(tab$tdi_lin[w]), est_name),
                 table = tab,
                 settings = list(p = p, delta = delta, conf.level = conf.level,
                                 method = method, B = B, seed = seed,
                                 log_scale = log_scale, source = source,
                                 worst_row = w),
                 plot_data = plot_data),
            class = c("tdi_htest", "htest"))
}

# generalized pivotal quantities. `comp` holds independent pieces v of the
# variance of a single difference with their df: each is drawn as
# v * df / chi-square(df), and sigma*^2 is their sum. With `scale = TRUE`, the
# SE of the bias is proportional to the square root of the first piece (the
# variance of a subject mean, or the whole variance for independent data), so
# mu* = mu - Z * SE * sqrt(v1* / v1); otherwise mu* = mu - T * SE, with T on
# the df of the bias. Pieces with no df or no variance are held fixed.
.tdi_gpq <- function(r, comp, p, delta, conf.level, B) {
  v_star <- vapply(seq_along(comp$v), function(k) {
    v <- comp$v[k]
    d <- comp$df[k]
    if (is.finite(d) && d > 0 && v > 0) v * d / stats::rchisq(B, d) else rep(max(v, 0), B)
  }, numeric(B))
  v_star <- matrix(v_star, nrow = B)
  sig <- sqrt(rowSums(v_star))
  mu <- if (isTRUE(comp$scale) && comp$v[1] > 0) {
    r$bias - stats::rnorm(B) * r$se * sqrt(v_star[, 1] / comp$v[1])
  } else {
    r$bias - stats::rt(B, r$df_bias) * r$se
  }
  t_star <- tdi_exact(mu, sig, p)
  res <- list(upper = unname(stats::quantile(t_star, conf.level)))
  if (!is.null(delta)) {
    res$cp_lower <- unname(stats::quantile(cp_exact(delta, mu, sig),
                                           1 - conf.level))
    res$p.value <- mean(t_star >= delta)
  }
  res
}

# delta method on log(TDI) and logit(CP)
.tdi_delta <- function(r, p, delta, conf.level) {
  b <- r$bias
  v <- r$sd^2
  nu <- r$df_sd
  var_v <- 2 * v^2 / nu
  grad <- function(f) {
    hb <- 1e-5 * max(abs(b), sqrt(v))
    hv <- 1e-5 * v
    c((f(b + hb, v) - f(b - hb, v)) / (2 * hb),
      (f(b, v + hv) - f(b, v - hv)) / (2 * hv))
  }
  q <- stats::qt(conf.level, nu)
  # log TDI and its standard error at proportion pp
  log_tdi <- function(pp) {
    f_t <- function(m, vv) log(tdi_exact(m, sqrt(vv), pp))
    g <- grad(f_t)
    c(est = f_t(b, v), se = sqrt(g[1]^2 * r$se^2 + g[2]^2 * var_v))
  }
  lt <- log_tdi(p)
  res <- list(upper = exp(lt[["est"]] + q * lt[["se"]]))
  if (!is.null(delta)) {
    # CP bound: the largest proportion whose bound is still below delta,
    # so that the CP bound and the test always agree
    f_q <- function(pp) {
      l <- log_tdi(pp)
      l[["est"]] + q * l[["se"]] - log(delta)
    }
    res$cp_lower <- .invert_in_p(f_q)
    res$statistic <- (log(delta) - lt[["est"]]) / lt[["se"]]
    res$p.value <- stats::pt(res$statistic, nu, lower.tail = FALSE)
  }
  res
}

# root in p of an increasing function f(p) = bound(p) - delta, or 0 / 1 when
# the bound is above / below delta for all p
.invert_in_p <- function(f, lo = 1e-6, hi = 1 - 1e-10) {
  if (f(lo) >= 0) return(0)
  if (f(hi) <= 0) return(1)
  stats::uniroot(f, c(lo, hi), tol = 1e-10)$root
}

# Escaramis et al. (2010): with q such that |mu| + z_q sigma is the TDI,
# i.e. pnorm(z_q) - pnorm(-z_q - 2 |mu| / sigma) = p, the bound is the
# one-sided upper tolerance bound for the q quantile of |mu| + e, computed as
# the upper "iu" bound of tolerance_limit(): the exact noncentral t bound for
# independent differences, the MOVER bound otherwise. q is estimated from
# mu-hat and sigma-hat, so the bound is approximate.
.tdi_tol <- function(r, info, p, delta, conf.level) {
  b <- abs(r$bias)
  s <- r$sd
  q_level <- function(pp) {
    f <- function(m) m - stats::pnorm(-stats::qnorm(m) - 2 * b / s) - pp
    if (f(1 - 1e-12) <= 0) return(1 - 1e-12)
    stats::uniroot(f, c(pp, 1 - 1e-12), tol = 1e-14)$root
  }
  tl_max <- function(level, content) {
    q <- q_level(content)
    analytic_bounds(b = b, s = s, sem = r$se, df = r$df_bias,
                    level = level, info = info, pred_level = 2 * q - 1,
                    bound_type = "iu",
                    independent = isTRUE(r$scale_sd))$upper
  }
  res <- list(upper = tl_max(conf.level, p))
  if (!is.null(delta)) {
    # CP bound: the largest content whose bound is still below delta
    res$cp_lower <- .invert_in_p(function(q) tl_max(conf.level, q) - delta)
    # p-value: the smallest alpha at which the bound is below delta
    f_l <- function(level) tl_max(level, p) - delta
    lo <- 1e-8
    hi <- 1 - 1e-12
    res$p.value <- if (f_l(lo) >= 0) 1 else if (f_l(hi) <= 0) 1 - hi else
      1 - stats::uniroot(f_l, c(lo, hi), tol = 1e-12)$root
  }
  res
}
