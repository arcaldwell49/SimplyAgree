#' @title Simulate Method Comparison Data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Simulates measurements of a set of items by several methods, with or
#' without replicates, from the variance component model of Carstensen (2010)
#' with a linear (scaled) relation between methods. Useful for planning
#' studies, checking methods by simulation, and examples.
#'
#' @param n_items Number of items (subjects).
#' @param n_reps Number of replicate measurements of each item by each method
#'   (before any are removed by `p_missing`).
#' @param methods Character vector of method names (at least two). The first
#'   is the reference method.
#' @param alpha,beta Intercepts and slopes of each method on the common item
#'   scale (see details). Scalars are recycled to `length(methods)`.
#' @param mu_mean,mu_sd Mean and SD of the true item values `mu_i`, which are
#'   drawn from a normal distribution.
#' @param tau SD of the method-by-item interaction `c_mi` on the item scale.
#'   A scalar (common) or one value per method.
#' @param omega SD of the item-by-replicate effect `a_ir` (common to all
#'   methods, on the item scale). Only used when `replicates = "linked"`.
#' @param sigma Residual (measurement error) SD of each method, on that
#'   method's scale. A scalar or one value per method.
#' @param replicates "exchangeable" (default) or "linked". With linked
#'   replicates, all methods measure an item on the same occasions, which
#'   share the effect `a_ir`.
#' @param p_missing Probability that each measurement is removed at random.
#'   At least one measurement of each item by each method is kept.
#' @param transform Transformation (see `method_vc()`). If not "none", the
#'   data are simulated on the transformed scale and back-transformed, so all
#'   parameters refer to the transformed scale.
#' @param seed Optional seed. The global random number generator state is
#'   restored afterwards.
#'
#' @details
#' Measurement `r` of item `i` by method `m` is
#' \deqn{y_{mir} = \alpha_m + \beta_m (\mu_i + a_{ir} + c_{mi}) + e_{mir},}
#' with \eqn{\mu_i \sim N(\mu, \sigma_\mu^2)},
#' \eqn{a_{ir} \sim N(0, \omega^2)} (linked replicates only),
#' \eqn{c_{mi} \sim N(0, \tau_m^2)}, and
#' \eqn{e_{mir} \sim N(0, \sigma_m^2)} (Carstensen, 2010, eq. 7.1). With all
#' \eqn{\beta_m = 1}, this is the constant bias model (eq. 5.1/5.2) used by
#' `method_vc()`.
#'
#' The `truth` attribute holds the parameters and the quantities derived from
#' them, on the (transformed) analysis scale:
#'
#'   - `params`: the parameters, one row per method, with the y-scale SDs
#'     `tau_y` (\eqn{\beta_m \tau_m}) and `omega_y` (\eqn{\beta_m \omega}).
#'   - `pairs`: for each pair of methods (`method1` - `method2`), the bias at
#'     `mu_mean`, the SD of the difference between single measurements of the
#'     same item (on the same occasion, for linked replicates), and the
#'     corresponding 95% limits of agreement. The bias is constant over items
#'     only when the slopes are equal (`constant_bias`).
#'   - `repeatability`: for each method, `sigma`, the 95% repeatability
#'     coefficient \eqn{1.96 \sqrt{2} \sigma_m}, and, for linked replicates,
#'     the reproducibility coefficient
#'     \eqn{1.96 \sqrt{2} \sqrt{\sigma_m^2 + \beta_m^2 \omega^2}}.
#'   - `conversion`: for each ordered pair (`to`, `from`), the conversion
#'     intercept, slope, and prediction SD (Carstensen, 2010, section 7.1).
#'
#' @return A tibble with columns `id`, `method` (a factor, levels in the
#'   order of `methods`), `repl`, and `y`, with attribute `truth` (see
#'   details).
#'
#' @examples
#' # three methods, linked replicates, constant bias
#' dat = sim_methods(n_items = 30, n_reps = 3, methods = c("A", "B", "C"),
#'                   alpha = c(0, 0.5, -0.2), tau = 0.3, omega = 0.4,
#'                   sigma = c(0.3, 0.5, 0.4), replicates = "linked",
#'                   seed = 1)
#' head(dat)
#' attr(dat, "truth")$pairs
#'
#' @references
#' Carstensen, B. (2010). Comparing Clinical Measurement Methods: A Practical
#' Guide. Wiley.
#'
#' @seealso `method_vc()`, [mc_long()]
#' @importFrom stats rnorm runif
#' @export

sim_methods = function(n_items = 50,
                       n_reps = 1,
                       methods = c("A", "B"),
                       alpha = 0,
                       beta = 1,
                       mu_mean = 0,
                       mu_sd = 1,
                       tau = 0,
                       omega = 0,
                       sigma = 1,
                       replicates = c("exchangeable", "linked"),
                       p_missing = 0,
                       transform = "none",
                       seed = NULL) {
  replicates = match.arg(replicates)
  tf = mc_transform(transform)

  ## checks ----
  if (!is.character(methods) || length(methods) < 2 || anyDuplicated(methods)) {
    stop("`methods` must be a character vector of at least two distinct ",
         "method names.", call. = FALSE)
  }
  M = length(methods)
  chk_count = function(x, nm) {
    if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 1 || x != round(x)) {
      stop("`", nm, "` must be a single positive whole number.", call. = FALSE)
    }
  }
  chk_count(n_items, "n_items")
  chk_count(n_reps, "n_reps")
  if (n_items < 2) {
    stop("`n_items` must be at least 2.", call. = FALSE)
  }
  recycle = function(x, nm, nonneg = FALSE) {
    if (!is.numeric(x) || !(length(x) %in% c(1, M)) || anyNA(x)) {
      stop("`", nm, "` must be a single number or one number per method (",
           M, ").", call. = FALSE)
    }
    if (nonneg && any(x < 0)) {
      stop("`", nm, "` must not be negative.", call. = FALSE)
    }
    rep_len(x, M)
  }
  alpha = recycle(alpha, "alpha")
  beta = recycle(beta, "beta")
  tau = recycle(tau, "tau", nonneg = TRUE)
  sigma = recycle(sigma, "sigma", nonneg = TRUE)
  for (nm in c("mu_mean", "mu_sd", "omega")) {
    val = get(nm)
    if (!is.numeric(val) || length(val) != 1 || is.na(val)) {
      stop("`", nm, "` must be a single number.", call. = FALSE)
    }
  }
  if (mu_sd < 0 || omega < 0) {
    stop("`mu_sd` and `omega` must not be negative.", call. = FALSE)
  }
  if (any(beta == 0)) {
    stop("`beta` must not be zero.", call. = FALSE)
  }
  if (!is.numeric(p_missing) || length(p_missing) != 1 ||
      p_missing < 0 || p_missing >= 1) {
    stop("`p_missing` must be a single number in [0, 1).", call. = FALSE)
  }
  if (replicates == "exchangeable" && omega > 0) {
    warning("`omega` is ignored with replicates = 'exchangeable'.",
            call. = FALSE)
    omega = 0
  }

  ## simulate ----
  dat = mc_with_seed(seed, {
    mu = rnorm(n_items, mu_mean, mu_sd)
    a_ir = matrix(rnorm(n_items * n_reps, 0, omega), n_items, n_reps)
    c_mi = matrix(rnorm(M * n_items, 0, rep(tau, n_items)), M, n_items)

    grid = expand.grid(m = seq_len(M), r = seq_len(n_reps),
                       i = seq_len(n_items))
    e = rnorm(nrow(grid), 0, sigma[grid$m])
    y_t = alpha[grid$m] +
      beta[grid$m] * (mu[grid$i] + a_ir[cbind(grid$i, grid$r)] +
                        c_mi[cbind(grid$m, grid$i)]) + e

    keep = rep(TRUE, nrow(grid))
    if (p_missing > 0) {
      keep = runif(nrow(grid)) >= p_missing
      # keep at least one measurement per (method, item)
      grp = interaction(grid$m, grid$i, drop = TRUE)
      none = tapply(keep, grp, function(k) !any(k))
      for (g in names(none)[none]) {
        rows = which(grp == g)
        keep[rows[sample.int(length(rows), 1)]] = TRUE
      }
    }

    dplyr::tibble(
      id = grid$i,
      method = factor(methods[grid$m], levels = methods),
      repl = grid$r,
      y = tf$inv(y_t)
    )[keep, ]
  })

  dat = dat %>%
    dplyr::arrange(id, method, repl)
  attr(dat, "truth") = sim_methods_truth(
    methods = methods, alpha = alpha, beta = beta, mu_mean = mu_mean,
    mu_sd = mu_sd, tau = tau, omega = omega, sigma = sigma,
    replicates = replicates, n_items = n_items, n_reps = n_reps,
    p_missing = p_missing, transform = tf$name
  )
  dat
}

# True values derived from the simulation parameters ----
sim_methods_truth = function(methods, alpha, beta, mu_mean, mu_sd, tau,
                             omega, sigma, replicates, n_items, n_reps,
                             p_missing, transform, level = 0.95) {
  z = qnorm((1 + level) / 2)
  M = length(methods)
  linked = replicates == "linked"
  tau_y = beta * tau
  omega_y = beta * omega

  params = dplyr::tibble(method = methods, alpha = alpha, beta = beta,
                         tau = tau, sigma = sigma, tau_y = tau_y,
                         omega_y = omega_y)

  ## pairs (method1 earlier in order) ----
  idx = utils::combn(M, 2)
  m = idx[1, ]
  k = idx[2, ]
  pair_sd = sqrt(tau_y[m]^2 + tau_y[k]^2 + sigma[m]^2 + sigma[k]^2 +
                   (beta[m] - beta[k])^2 * omega^2)
  pair_bias = alpha[m] - alpha[k] + (beta[m] - beta[k]) * mu_mean
  pairs = dplyr::tibble(method1 = methods[m],
                        method2 = methods[k],
                        bias = pair_bias,
                        sd = pair_sd,
                        lower_loa = pair_bias - z * pair_sd,
                        upper_loa = pair_bias + z * pair_sd,
                        constant_bias = beta[m] == beta[k])

  ## repeatability ----
  repeatability = dplyr::tibble(method = methods,
                                sigma = sigma,
                                rc = z * sqrt(2) * sigma)
  if (linked) {
    repeatability$rpc = z * sqrt(2) * sqrt(sigma^2 + omega_y^2)
  }

  ## conversion (to k from m) ----
  conv = expand.grid(from = seq_len(M), to = seq_len(M))
  i_fr = conv$from
  i_to = conv$to
  b_conv = beta[i_to] / beta[i_fr]
  a_conv = alpha[i_to] - alpha[i_fr] * beta[i_to] / beta[i_fr]
  sd_conv = ifelse(
    i_fr == i_to,
    sqrt(2) * sigma[i_fr],
    sqrt(b_conv^2 * (tau_y[i_fr]^2 + sigma[i_fr]^2) +
           tau_y[i_to]^2 + sigma[i_to]^2)
  )
  conversion = dplyr::tibble(to = methods[i_to],
                             from = methods[i_fr],
                             alpha = a_conv,
                             beta = b_conv,
                             sd_pred = sd_conv) %>%
    dplyr::arrange(match(to, methods), match(from, methods))

  list(
    params = params,
    mu_mean = mu_mean,
    mu_sd = mu_sd,
    omega = if (linked) omega else NA_real_,
    pairs = pairs,
    repeatability = repeatability,
    conversion = conversion,
    design = list(n_items = n_items, n_reps = n_reps,
                  replicates = replicates, p_missing = p_missing,
                  transform = transform, level = level)
  )
}
