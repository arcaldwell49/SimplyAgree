# Internal helpers for the multi-method comparison functions ----
# (method_vc(), method_convert(), sim_methods(), mc_long())

# a level as a percentage for printing: 0.95 -> "95", 2 * pnorm(2) - 1 ->
# "95.45" (not "95.4499736103642")
mc_pct = function(level) {
  format(signif(100 * level, 4), drop0trailing = TRUE)
}

# transformation registry ----

#' Transformation registry for method comparison
#'
#' Returns the functions for a transformation applied to the measurements
#' before analysis. The full entry (functions included) is stored in returned
#' objects so plots and back-transformations work after `readRDS()`.
#'
#' @param transform Either the name of a built-in transformation ("none",
#'   "log", "logit", "pctlogit", "sqrt", "asinsqrt") or a list with elements
#'   `fun` and `inv` (functions) and, optionally, `name`.
#' @return A list with `name`, `fun`, `inv`, `deriv` (derivative of `fun`),
#'   `domain_check` (function returning `TRUE` if all values are in the
#'   domain), `domain` (text description) and `label` (for axis labels).
#' @noRd
mc_transform = function(transform = "none") {
  if (is.list(transform)) {
    return(mc_transform_custom(transform))
  }
  if (!is.character(transform) || length(transform) != 1) {
    stop("transform must be a single character string or a list with ",
         "elements 'fun' and 'inv'.", call. = FALSE)
  }
  builtin = c("none", "log", "logit", "pctlogit", "sqrt", "asinsqrt")
  if (!(transform %in% builtin)) {
    stop("Unknown transform '", transform, "'. Use one of: ",
         paste0("'", builtin, "'", collapse = ", "),
         ", or a list with elements 'fun' and 'inv'.", call. = FALSE)
  }

  switch(
    transform,
    none = list(
      name = "none",
      fun = function(y) y,
      inv = function(z) z,
      deriv = function(y) rep(1, length(y)),
      domain_check = function(y) all(is.finite(y)),
      domain = "finite values",
      label = function(lab) lab
    ),
    log = list(
      name = "log",
      fun = function(y) log(y),
      inv = function(z) exp(z),
      deriv = function(y) 1 / y,
      domain_check = function(y) all(y > 0),
      domain = "y > 0",
      label = function(lab) paste0("log(", lab, ")")
    ),
    logit = list(
      name = "logit",
      fun = function(y) stats::qlogis(y),
      inv = function(z) stats::plogis(z),
      deriv = function(y) 1 / (y * (1 - y)),
      domain_check = function(y) all(y > 0 & y < 1),
      domain = "0 < y < 1",
      label = function(lab) paste0("logit(", lab, ")")
    ),
    pctlogit = list(
      name = "pctlogit",
      fun = function(y) stats::qlogis(y / 100),
      inv = function(z) 100 * stats::plogis(z),
      deriv = function(y) 100 / (y * (100 - y)),
      domain_check = function(y) all(y > 0 & y < 100),
      domain = "0 < y < 100",
      label = function(lab) paste0("logit(", lab, "/100)")
    ),
    sqrt = list(
      name = "sqrt",
      fun = function(y) sqrt(y),
      inv = function(z) z^2,
      deriv = function(y) 1 / (2 * sqrt(y)),
      domain_check = function(y) all(y >= 0),
      domain = "y >= 0",
      label = function(lab) paste0("sqrt(", lab, ")")
    ),
    asinsqrt = list(
      name = "asinsqrt",
      fun = function(y) asin(sqrt(y)),
      inv = function(z) sin(z)^2,
      deriv = function(y) 1 / (2 * sqrt(y * (1 - y))),
      domain_check = function(y) all(y >= 0 & y <= 1),
      domain = "0 <= y <= 1",
      label = function(lab) paste0("asin(sqrt(", lab, "))")
    )
  )
}

## custom transformation ----
mc_transform_custom = function(transform) {
  if (!is.function(transform$fun) || !is.function(transform$inv)) {
    stop("A custom transform must be a list with functions 'fun' ",
         "(the transformation) and 'inv' (its inverse), e.g. ",
         "list(fun = log, inv = exp, name = 'log').", call. = FALSE)
  }
  nm = if (is.null(transform$name)) "custom" else as.character(transform$name)[1]
  fun = transform$fun
  inv = transform$inv
  list(
    name = nm,
    fun = fun,
    inv = inv,
    # central difference derivative
    deriv = function(y) {
      h = 1e-6 * pmax(1, abs(y))
      (fun(y + h) - fun(y - h)) / (2 * h)
    },
    # validated numerically on the data: finite and inv(fun(y)) == y
    domain_check = function(y) {
      z = suppressWarnings(fun(y))
      if (length(z) != length(y) || !all(is.finite(z))) {
        return(FALSE)
      }
      yb = suppressWarnings(inv(z))
      isTRUE(all(abs(yb - y) <= 1e-6 * pmax(1, abs(y))))
    },
    domain = "values where fun is finite and inv(fun(y)) == y",
    label = function(lab) paste0(nm, "(", lab, ")")
  )
}

# data organization ----

#' Organize long-format method comparison data
#'
#' @param data Data frame in long format.
#' @param y,method,id,replicate Column names.
#' @param transform Transformation name or custom list (see `mc_transform()`).
#' @param replicates "exchangeable" or "linked".
#' @return Tibble with `y`, `y_t`, `method`, `id`, `repl` (and `ir` for linked
#'   replicates) with attributes `n_items`, `n_methods`, `rep_table`,
#'   `has_reps` and `transform`.
#' @noRd
mc_data_org = function(data,
                       y,
                       method,
                       id,
                       replicate = NULL,
                       transform = "none",
                       replicates = "exchangeable") {
  replicates = match.arg(replicates, c("exchangeable", "linked"))
  if (!is.data.frame(data)) {
    stop("data must be a data frame in long format (one row per ",
         "measurement). Use mc_long() to convert wide data.", call. = FALSE)
  }
  tf = mc_transform(transform)

  ## check column names ----
  cols = list(y = y, method = method, id = id)
  if (!is.null(replicate)) cols$replicate = replicate
  for (arg in names(cols)) {
    val = cols[[arg]]
    if (!is.character(val) || length(val) != 1) {
      stop("`", arg, "` must be a single column name (a character string).",
           call. = FALSE)
    }
    if (!(val %in% names(data))) {
      stop("Column '", val, "' (given as `", arg, "`) was not found in data.",
           call. = FALSE)
    }
  }
  if (anyDuplicated(unlist(cols))) {
    stop("`y`, `method`, `id`, and `replicate` must name different columns.",
         call. = FALSE)
  }
  if (replicates == "linked" && is.null(replicate)) {
    stop("replicates = 'linked' requires `replicate`, the column identifying ",
         "the replicate (measurement occasion) shared by all methods.",
         call. = FALSE)
  }

  ## select and rename ----
  df = dplyr::tibble(
    y = data[[y]],
    method = data[[method]],
    id = data[[id]],
    repl = if (is.null(replicate)) 1 else data[[replicate]]
  )
  if (!is.numeric(df$y)) {
    stop("Column '", y, "' (`y`) must be numeric.", call. = FALSE)
  }
  df$method = if (is.factor(df$method)) df$method else factor(df$method)
  df$id = if (is.factor(df$id)) df$id else factor(df$id)
  if (anyNA(df$method) || anyNA(df$id) || anyNA(df$repl)) {
    stop("`method`, `id`, and `replicate` must not contain missing values.",
         call. = FALSE)
  }

  ## missing measurements ----
  n_na = sum(is.na(df$y))
  if (n_na > 0) {
    message(n_na, " row(s) with a missing measurement removed.")
    df = df[!is.na(df$y), ]
  }

  ## replicate structure ----
  if (is.null(replicate)) {
    if (anyDuplicated(df[, c("method", "id")])) {
      stop("Multiple measurements per method and id found; supply ",
           "`replicate`.", call. = FALSE)
    }
  } else if (anyDuplicated(df[, c("method", "id", "repl")])) {
    stop("Multiple measurements with the same method, id, and replicate ",
         "found. Each replicate must identify a single measurement per ",
         "method and id.", call. = FALSE)
  }

  ## methods ----
  df$method = droplevels(df$method)
  if (nlevels(df$method) < 2) {
    stop("At least two methods are required in `method`.", call. = FALSE)
  }

  ## items measured by at least two methods ----
  n_meth_id = tapply(df$method, df$id, function(m) length(unique(m)))
  drop_ids = names(n_meth_id)[!is.na(n_meth_id) & n_meth_id < 2]
  if (length(drop_ids) > 0) {
    message(length(drop_ids), " item(s) measured by fewer than two ",
            "methods removed.")
    df = df[!(df$id %in% drop_ids), ]
  }
  df$id = droplevels(df$id)
  df$method = droplevels(df$method)
  if (nlevels(df$method) < 2 || nlevels(df$id) < 2) {
    stop("Fewer than two items measured by at least two methods remain.",
         call. = FALSE)
  }

  ## transformation ----
  if (!isTRUE(tf$domain_check(df$y))) {
    stop("Some measurements are outside the domain of transform '", tf$name,
         "' (", tf$domain, ").", call. = FALSE)
  }
  df$y_t = tf$fun(df$y)

  if (replicates == "linked") {
    df$ir = interaction(df$id, df$repl, drop = TRUE)
  }

  df = df %>%
    dplyr::arrange(id, repl, method) %>%
    dplyr::select(y, y_t, method, id, repl, dplyr::any_of("ir"))

  ## design summary ----
  n_mi = df %>%
    dplyr::count(method, id, name = "n_reps")
  rep_table = n_mi %>%
    dplyr::count(method, n_reps, name = "n_items") %>%
    dplyr::arrange(method, n_reps)
  # replicates are available for every method
  has_reps = all(tapply(n_mi$n_reps, n_mi$method, max) > 1)

  attr(df, "n_items") = nlevels(df$id)
  attr(df, "n_methods") = nlevels(df$method)
  attr(df, "rep_table") = rep_table
  attr(df, "has_reps") = has_reps
  attr(df, "replicates") = replicates
  attr(df, "transform") = tf
  df
}

# seed handling ----

# Evaluate expr with a seed without changing the global RNG state
mc_with_seed = function(seed, expr) {
  if (is.null(seed)) {
    return(expr)
  }
  has_seed = exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_seed) {
    old_seed = get(".Random.seed", envir = globalenv(), inherits = FALSE)
  }
  on.exit({
    if (has_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  expr
}

# pairing ----

#' Paired (wide) data for two methods
#'
#' @param df Output of `mc_data_org()`.
#' @param m1,m2 Method levels; differences are `m1 - m2`.
#' @param replicates "exchangeable" or "linked".
#' @param seed Optional seed for the random pairing of exchangeable
#'   replicates; the global RNG state is restored afterwards.
#' @return Tibble with `id`, `repl`, `y1`, `y2`, `d`, `avg` (transformed
#'   scale), `y1_orig`, `y2_orig`.
#' @noRd
mc_pair = function(df, m1, m2, replicates = "exchangeable", seed = NULL) {
  replicates = match.arg(replicates, c("exchangeable", "linked"))
  m1 = as.character(m1)
  m2 = as.character(m2)
  lv = levels(df$method)
  if (!all(c(m1, m2) %in% lv) || m1 == m2) {
    stop("pair must name two different methods from: ",
         paste(lv, collapse = ", "), ".", call. = FALSE)
  }

  d1 = df[df$method == m1, ]
  d2 = df[df$method == m2, ]

  if (replicates == "exchangeable") {
    # randomly permute replicate labels within (method, id)
    # (Carstensen 2010, section 5.7), so pairing does not depend on the order
    # in which exchangeable replicates were recorded
    permute = function(d) {
      d %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(repl = sample.int(dplyr::n())) %>%
        dplyr::ungroup()
    }
    mc_with_seed(seed, {
      d1 = permute(d1)
      d2 = permute(d2)
    })
  }

  d1 = d1 %>%
    dplyr::select(id, repl, y1 = y_t, y1_orig = y)
  d2 = d2 %>%
    dplyr::select(id, repl, y2 = y_t, y2_orig = y)

  dplyr::inner_join(d1, d2, by = c("id", "repl")) %>%
    dplyr::mutate(d = y1 - y2,
                  avg = (y1 + y2) / 2) %>%
    dplyr::arrange(id, repl) %>%
    dplyr::select(id, repl, y1, y2, d, avg, y1_orig, y2_orig)
}
