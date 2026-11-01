
# Bootstrap covariance of the coefficients of a quantile regression (rq or
# rqs fit), one matrix per tau. xy-pair resampling, as in quantreg's default
# summary(se = "boot"), but each resample is fitted by Frisch-Newton
# (rq.fit.fnb): the simplex ("br") used by quantreg's bootstrap can cycle
# without end on resamples with many exact ties (GitHub issue #50 data).
# idx: optional n x R matrix of row indices (for testing).
rq_boot_vcov <- function(quan_mod, R = 200, idx = NULL) {
  X <- model.matrix(terms(quan_mod), quan_mod$model)
  y <- model.response(quan_mod$model)
  n <- length(y)
  if (is.null(idx)) {
    idx <- matrix(sample.int(n, n * R, replace = TRUE), n, R)
  }
  lapply(quan_mod$tau, function(tau) {
    B <- apply(idx, 2, function(i) {
      quantreg::rq.fit.fnb(X[i, , drop = FALSE], y[i], tau = tau)$coefficients
    })
    stats::cov(t(matrix(B, nrow = ncol(X))))
  })
}

# Coefficient table for each tau from the bootstrap covariance (V from
# rq_boot_vcov()): estimate, std.error, t statistic and p-value on n - p df,
# and t-based confidence limits, as summary(se = "boot") reports them.
rq_boot_coef <- function(quan_mod, V, conf.level = 0.95) {
  b <- as.matrix(quan_mod$coefficients)
  rdf <- nrow(quan_mod$model) - nrow(b)
  cv <- qt(1 - (1 - conf.level) / 2, rdf)
  do.call(rbind, lapply(seq_along(quan_mod$tau), function(j) {
    se <- sqrt(diag(V[[j]]))
    data.frame(estimate = b[, j], std.error = se,
               statistic = b[, j] / se,
               p.value = 2 * pt(-abs(b[, j] / se), rdf),
               conf.low = b[, j] - cv * se,
               conf.high = b[, j] + cv * se,
               tau = quan_mod$tau[j])
  }))
}

# Estimates and t-based confidence limits (n - p df) of the tau quantile at
# the given values of `mean`, from the bootstrap covariance; the same columns
# as confint(emmeans(ref_grid(quan_mod, tau = tau, at = ...), ~ mean)).
rq_boot_ci <- function(quan_mod, V, tau, at, conf.level) {
  j <- which(abs(quan_mod$tau - tau) < 1e-4)
  b <- as.matrix(quan_mod$coefficients)[, j]
  X <- model.matrix(delete.response(terms(quan_mod)),
                    data.frame(mean = at))
  est <- drop(X %*% b)
  se <- sqrt(rowSums((X %*% V[[j]]) * X))
  df <- nrow(quan_mod$model) - length(b)
  cv <- qt(1 - (1 - conf.level) / 2, df)
  data.frame(mean = at, emmean = est, SE = se, df = df,
             lower.CL = est - cv * se, upper.CL = est + cv * se)
}


get_qemm = function(quan_mod,
                    df,
                    agree.l,
                    agree.u,
                    conf.level){
  V = rq_boot_vcov(quan_mod)
  at = seq(min(df$mean, na.rm = TRUE),
           max(df$mean, na.rm = TRUE),
           length.out = 100)
  quan_emm_med = rq_boot_ci(quan_mod, V, tau = .5, at = at,
                            conf.level = conf.level)
  df_coef_med = data.frame(at = quan_emm_med$mean,
                           estimate = quan_emm_med$emmean,
                           lower.ci = quan_emm_med$lower.CL,
                           upper.ci = quan_emm_med$upper.CL,
                           text = "Bias")

  quan_emm_lloa = rq_boot_ci(quan_mod, V, tau = agree.u, at = at,
                             conf.level = conf.level)
  df_coef_lloa = data.frame(at = quan_emm_lloa$mean,
                            estimate = quan_emm_lloa$emmean,
                            lower.ci = quan_emm_lloa$lower.CL,
                            upper.ci = quan_emm_lloa$emmean,
                            text = "Lower LoA")

  quan_emm_uloa = rq_boot_ci(quan_mod, V, tau = agree.l, at = at,
                             conf.level = conf.level)
  df_coef_uloa = data.frame(at = quan_emm_uloa$mean,
                            estimate = quan_emm_uloa$emmean,
                            lower.ci = quan_emm_uloa$emmean,
                            upper.ci = quan_emm_uloa$upper.CL,
                            text = "Upper LoA")

  df_emm = rbind(df_coef_uloa,
                 df_coef_med,
                 df_coef_lloa)
  df_emm$text = factor(df_emm$text,
                       levels = c("Upper LoA", "Bias", "Lower LoA"))
  return(df_emm)
}



