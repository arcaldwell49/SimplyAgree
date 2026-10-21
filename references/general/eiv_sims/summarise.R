# Tabulate the eiv simulations against the pass criteria in README.md.
#   Rscript references/general/eiv_sims/summarise.R
# Writes summary_type1.csv and summary_power.csv next to this script.

dir <- "references/general/eiv_sims"
out_dir <- Sys.getenv("EIV_SIMS_OUT", "references/working/eiv_out")
alpha <- 0.05

# ---- Part A ----------------------------------------------------------------

cell_metrics <- function(m, p = "") {
  g <- function(k) m[, paste0(p, k)]
  fail <- g("fail") == 1
  ok <- !fail
  pF <- g("pF")[ok]; pC <- g("pC")[ok]
  lo0 <- g("lo0")[ok]; hi0 <- g("hi0")[ok]
  lo1 <- g("lo1")[ok]; hi1 <- g("hi1")[ok]
  b0 <- g("b0")[ok]; b1 <- g("b1")[ok]
  v00 <- g("v00")[ok]; v11 <- g("v11")[ok]; v01 <- g("v01")[ok]
  has_v <- any(!is.na(pF))
  data.frame(
    n_ok = sum(ok), fail = mean(fail),
    rej_F = if (has_v) mean(pF < alpha, na.rm = TRUE) else NA,
    rej_C = if (has_v) mean(pC < alpha, na.rm = TRUE) else NA,
    rej_SS = if (p == "" && "pSS" %in% colnames(m))
      mean(m[ok, "pSS"] < alpha, na.rm = TRUE) else NA,
    rej_BB = if (p == "" && "pBB" %in% colnames(m))
      mean(m[ok, "pBB"] < alpha, na.rm = TRUE) else NA,
    rej_either = mean(lo0 > 0 | hi0 < 0 | lo1 > 1 | hi1 < 1, na.rm = TRUE),
    cov_int = mean(lo0 <= 0 & hi0 >= 0, na.rm = TRUE),
    int_above = mean(lo0 > 0, na.rm = TRUE), int_below = mean(hi0 < 0, na.rm = TRUE),
    cov_slope = mean(lo1 <= 1 & hi1 >= 1, na.rm = TRUE),
    slope_above = mean(lo1 > 1, na.rm = TRUE), slope_below = mean(hi1 < 1, na.rm = TRUE),
    bias_int = mean(b0), bias_slope = mean(b1 - 1),
    sd_slope = sd(b1),
    se_slope = if (has_v) sqrt(mean(v11, na.rm = TRUE)) else NA,
    sd_int = sd(b0),
    se_int = if (has_v) sqrt(mean(v00, na.rm = TRUE)) else NA,
    cor_emp = cor(b0, b1),
    cor_vcov = if (has_v) mean(v01 / sqrt(v00 * v11), na.rm = TRUE) else NA)
}

files <- list.files(out_dir, "^type1_.*\\.rds$", full.names = TRUE)
rows <- list()
for (f in files) {
  x <- readRDS(f)
  cell <- x$cell
  base <- data.frame(block = x$block, n = cell$n, range = cell$range,
                     lambda = cell$lambda, err = cell$err,
                     var_type = if (is.null(cell$var_type)) "const" else cell$var_type,
                     method = if (is.null(cell$method)) "deming" else cell$method)
  if (x$block == "pb") {
    for (p in c("an", "bs", "jk", "du")) {
      if (!paste0(p, "_fail") %in% colnames(x$res)) next
      se <- c(an = "analytic", bs = "bootstrap", jk = "jackknife", du = "dufey")[[p]]
      rows[[length(rows) + 1]] <- cbind(base, se = se,
                                        cell_metrics(x$res, paste0(p, "_")))
    }
  } else {
    r <- cbind(base, se = "jackknife(dem)", cell_metrics(x$res))
    if (x$block == "reps") r$er_hat_med <- median(x$res[, "er_hat"], na.rm = TRUE)
    rows[[length(rows) + 1]] <- r
  }
}
if (length(rows)) {
  t1 <- do.call(rbind, lapply(rows, function(r) {
    if (is.null(r$er_hat_med)) r$er_hat_med <- NA
    r
  }))
  t1$pass_F <- t1$rej_F >= 0.025 & t1$rej_F <= 0.075 & t1$fail < 0.01
  t1$target_F <- t1$rej_F >= 0.04 & t1$rej_F <= 0.06
  t1$pass_cov <- t1$cov_int >= 0.925 & t1$cov_int <= 0.975 &
    t1$cov_slope >= 0.925 & t1$cov_slope <= 0.975
  t1 <- t1[order(t1$block, t1$method, t1$se, t1$range, t1$lambda, t1$err,
                 t1$var_type, t1$n), ]
  write.csv(t1, file.path(dir, "summary_type1.csv"), row.names = FALSE)

  r3 <- function(x) round(x, 3)
  cat("\n==== Part A: type I error of joint_test() ====\n")
  for (b in unique(t1$block)) {
    s <- t1[t1$block == b, ]
    cat("\n--", b, "-- mean rejection by se x n (F | chi-sq | either CI); pass_F share\n")
    s$pass_BB <- s$rej_BB >= 0.025 & s$rej_BB <= 0.075
    s$pass_SS <- s$rej_SS >= 0.025 & s$rej_SS <= 0.075
    a <- aggregate(cbind(rej_F, rej_C, rej_SS, rej_BB, rej_either, pass_F, target_F, pass_SS,
                         pass_BB, pass_cov) ~
                     method + se + n, data = s, FUN = mean, na.rm = TRUE,
                   na.action = na.pass)
    print(transform(a, rej_F = r3(rej_F), rej_C = r3(rej_C), rej_SS = r3(rej_SS),
                    rej_BB = r3(rej_BB), pass_SS = r3(pass_SS),
                    pass_BB = r3(pass_BB),
                    rej_either = r3(rej_either), pass_F = r3(pass_F),
                    target_F = r3(target_F), pass_cov = r3(pass_cov)),
          row.names = FALSE)
    for (fac in c("range", "lambda", "err", "var_type")) {
      if (length(unique(s[[fac]])) < 2) next
      a <- aggregate(s$rej_F, list(se = s$se, level = s[[fac]]), mean, na.rm = TRUE)
      cat("  rej_F by", fac, ":",
          paste(sprintf("%s/%s=%.3f", a$se, a$level, a$x), collapse = "  "), "\n")
      if (any(!is.na(s$rej_BB))) {
        a <- aggregate(s$rej_BB, list(level = s[[fac]]), mean, na.rm = TRUE)
        cat("  rej_BB by", fac, ":",
            paste(sprintf("%s=%.3f", a$level, a$x), collapse = "  "), "\n")
      }
    }
    w <- s[!is.na(s$rej_F) & !s$pass_F, ]
    if (nrow(w)) {
      cat("  cells failing 2.5-7.5%:\n")
      print(w[, c("method", "se", "n", "range", "lambda", "err", "var_type",
                  "rej_F", "rej_C", "rej_SS", "rej_BB", "fail", "cor_emp", "cor_vcov",
                  "sd_slope", "se_slope")], row.names = FALSE, digits = 3)
    }
  }
}

# ---- Part B ----------------------------------------------------------------

pf_truth <- file.path(out_dir, "power_truth.rds")
if (file.exists(pf_truth)) {
  tr <- readRDS(pf_truth)
  if (is.null(tr$jBB)) tr$jBB <- NA
  if (is.null(tr$jSS)) tr$jSS <- NA
  tr_agg <- aggregate(cbind(jF, jC, jSS, jBB, either, fail) ~ scenario + n, data = tr,
                      FUN = mean, na.rm = TRUE, na.action = na.pass)
  tr_agg$nsim <- aggregate(jF ~ scenario + n, data = tr, FUN = length)$jF
  # Smoothed truth: binomial GLM on a natural spline in log n
  smooth_truth <- function(scn, col) {
    d <- tr[tr$scenario == scn & !is.na(tr[[col]]), ]
    if (length(unique(d[[col]])) < 2) return(function(n) rep(mean(d[[col]]), length(n)))
    fit <- glm(d[[col]] ~ splines::ns(log(d$n), df = 4), family = binomial)
    function(n) {
      nd <- data.frame(n = n)
      kn <- splines::ns(log(d$n), df = 4)
      X <- cbind(1, predict(kn, log(n)))
      as.numeric(plogis(X %*% coef(fit)))
    }
  }
  n_for <- function(f, target) {
    grid <- 3:300
    p <- f(grid)
    if (any(p >= target)) grid[which(p >= target)[1]] else NA
  }
  cat("\n==== Part B: truth curves (rejection rate; F = joint_test default) ====\n")
  print(transform(tr_agg, jF = round(jF, 3), jC = round(jC, 3),
                  jSS = round(jSS, 3), jBB = round(jBB, 3), either = round(either, 3)),
        row.names = FALSE)
  cat("\nTrue N for 90% power (smoothed): joint F | chi-sq | Sadler SS | BB | either CI\n")
  for (scn in setdiff(unique(tr$scenario), "P5")) {
    cat(sprintf("  %s: %s | %s | %s | %s | %s\n", scn,
                n_for(smooth_truth(scn, "jF"), .9), n_for(smooth_truth(scn, "jC"), .9),
                n_for(smooth_truth(scn, "jSS"), .9), n_for(smooth_truth(scn, "jBB"), .9),
                n_for(smooth_truth(scn, "either"), .9)))
  }
  write.csv(tr_agg, file.path(dir, "summary_truth.csv"), row.names = FALSE)

  out <- list()
  pf_ps <- file.path(out_dir, "power_powersim.rds")
  if (file.exists(pf_ps)) {
    ps <- readRDS(pf_ps)
    cat("\n==== Part B: deming_power_sim() vs truth ====\n")
    for (scn in unique(ps$scenario)) for (n in unique(ps$n)) {
      s <- ps[ps$scenario == scn & ps$n == n, ]
      t <- tr_agg[tr_agg$scenario == scn & tr_agg$n == n, ]
      if (!nrow(t) || !nrow(s)) next
      mc <- function(p) sqrt(p * (1 - p) / (nrow(s) * 1000) + p * (1 - p) / t$nsim)
      out[[length(out) + 1]] <- data.frame(
        part = "powersim", scenario = scn, n = n, runs = nrow(s),
        sim_joint = mean(s$joint), truth_F = t$jF, truth_C = t$jC,
        z_F = (mean(s$joint) - t$jF) / mc(t$jF),
        z_C = (mean(s$joint) - t$jC) / mc(t$jC),
        sim_either = mean(s$either), truth_either = t$either,
        z_either = (mean(s$either) - t$either) / mc(t$either))
    }
    o <- do.call(rbind, out)
    o$pass <- abs(o$z_F) <= 3
    print(o[, -1], row.names = FALSE, digits = 3)
    write.csv(o, file.path(dir, "summary_powersim.csv"), row.names = FALSE)
  }

  pf_ss <- file.path(out_dir, "power_samplesize.rds")
  if (file.exists(pf_ss)) {
    ss <- readRDS(pf_ss)
    cat("\n==== Part B: deming_sample_size() (target 0.90) vs truth ====\n")
    so <- list()
    for (scn in unique(ss$scenario)) {
      s <- ss[ss$scenario == scn, ]
      fF <- smooth_truth(scn, "jF"); fC <- smooth_truth(scn, "jC")
      fE <- smooth_truth(scn, "either")
      pj <- fF(s$n_joint[!is.na(s$n_joint)])
      pe <- fE(s$n_ci[!is.na(s$n_ci)])
      so[[length(so) + 1]] <- data.frame(
        scenario = scn, runs = nrow(s),
        trueN_joint_F = n_for(fF, .9), trueN_joint_C = n_for(fC, .9),
        med_N_joint = median(s$n_joint, na.rm = TRUE),
        q10_N_joint = quantile(s$n_joint, .1, na.rm = TRUE, names = FALSE),
        q90_N_joint = quantile(s$n_joint, .9, na.rm = TRUE, names = FALSE),
        med_truepow_joint = median(pj), q10_truepow_joint = quantile(pj, .1, names = FALSE),
        share_below_85_joint = mean(pj < .85),
        trueN_either = n_for(fE, .9),
        med_N_ci = median(s$n_ci, na.rm = TRUE),
        med_truepow_ci = median(pe), q10_truepow_ci = quantile(pe, .1, names = FALSE),
        share_below_85_ci = mean(pe < .85),
        not_found = mean(is.na(s$n_joint)))
    }
    so <- do.call(rbind, so)
    so$pass_joint <- abs(so$med_truepow_joint - .9) <= .025 & so$share_below_85_joint < .2
    so$pass_ci <- abs(so$med_truepow_ci - .9) <= .025 & so$share_below_85_ci < .2
    print(so, row.names = FALSE, digits = 3)
    write.csv(so, file.path(dir, "summary_samplesize.csv"), row.names = FALSE)
  }
}
