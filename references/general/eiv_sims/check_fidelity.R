# Fidelity checks for the eiv simulation harness. Run from the package root:
#   Rscript references/general/eiv_sims/check_fidelity.R
dir <- "references/general/eiv_sims"
suppressMessages(devtools::load_all(".", quiet = TRUE))
source(file.path(dir, "gen.R"))
source(file.path(dir, "sim_funs.R"))

ok <- function(label, cond) {
  cat(sprintf("%-68s %s\n", label, if (isTRUE(cond)) "OK" else "FAIL"))
  invisible(cond)
}

# 1. Generators: H0 holds and the error ratio is right
set.seed(1)
d <- gen_h0(2e5, c(5, 100), lambda = 4, err = 0.2)
sd_y <- 0.2 * 95 / sqrt(12)
ok("gen_h0 const: var(x - y) = (1 + lambda) sd_y^2 (2% tol)",
   abs(var(d$x - d$y) / (5 * sd_y^2) - 1) < 0.02)
ok("gen_h0 const: mean(y - x) ~ 0",
   abs(mean(d$y - d$x)) < 4 * sqrt(5 * sd_y^2 / 2e5))
d <- gen_h0(2e5, c(50, 100), lambda = 1, err = 0.2, var_type = "prop")
r <- (d$x - d$y) / ((d$x + d$y) / 2)
cv <- 0.2 * (50 / sqrt(12)) / 75
ok("gen_h0 prop: SD((x - y) / mean) ~ sqrt(2) cv (3% tol)",
   abs(sd(r) / (sqrt(2) * cv) - 1) < 0.03)
d <- gen_h0_reps(2e4, c(5, 100), lambda = 4, err = 0.2)
fit <- dem_reg(y ~ x, data = d, id = "id")
ok("gen_h0_reps: dem_reg(id = ) recovers lambda = 4 (3% tol)",
   abs(fit$error.ratio / 4 - 1) < 0.03)

# 2. gen_power() reproduces the package generator draw for draw
for (nm in names(power_scenarios)) {
  sc <- power_scenarios[[nm]]
  set.seed(42); a <- gen_power(37, sc)
  set.seed(42)
  b <- .generate_deming_data(37, sc$x_range, "uniform", sc$actual_slope,
                             sc$actual_intercept, sc$y_var_params,
                             sc$x_var_params)
  ok(sprintf("gen_power == .generate_deming_data (%s)", nm),
     isTRUE(all.equal(a$x, b$x, tolerance = 0)) &&
       isTRUE(all.equal(a$y, b$y, tolerance = 0)))
}

# 3. joint_test() p-values match a hand-computed Mahalanobis test
set.seed(3)
d <- gen_h0(30, c(50, 100), 1, 0.2)
fit <- dem_reg(y ~ x, data = d)
dd <- c(0, 1) - coef(fit)
m <- as.numeric(t(dd) %*% solve(vcov(fit)) %*% dd)
ok("joint_test F p = pf(D2 / 2, 2, n - 2)",
   abs(joint_test(fit, method = "wald")$p.value - pf(m / 2, 2, 28, lower.tail = FALSE)) < 1e-12)
ok("joint_test asymptotic p = pchisq(D2, 2)",
   abs(joint_test(fit, method = "wald", test_method = "asymptotic")$p.value -
         pchisq(m, 2, lower.tail = FALSE)) < 1e-12)

# 4. deming_power_sim() reproduced data set by data set. Before the 2026-10-20
#    fix its joint test matched the chi-squared Wald test (n = 12, P1: .595
#    vs .515 for the F Wald test); it now calls joint_test(), whose default
#    for Deming is the Sadler test.
for (nm in c("P1", "P3")) {
  sc <- power_scenarios[[nm]]
  set.seed(7)
  r <- suppressMessages(do.call(deming_power_sim,
                                c(list(n_sims = 200, sample_size = 12),
                                  sc_args(sc))))
  set.seed(7)
  rej <- t(replicate(200, {
    d <- .generate_deming_data(12, sc$x_range, "uniform", sc$actual_slope,
                               sc$actual_intercept, sc$y_var_params,
                               sc$x_var_params)
    er <- power_ratio(d, sc)
    s <- fit_stats(dem_reg(y ~ x, data = d, error.ratio = er,
                           weighted = sc$weighted))
    w <- if (sc$weighted) dem_weights(d$x, d$y, er)
    c(F = s[["pF"]] < 0.05, C = s[["pC"]] < 0.05,
      SS = ss_p(d$x, d$y, er, w = w) < 0.05,
      either = s[["lo0"]] > 0 || s[["hi0"]] < 0 || s[["lo1"]] > 1 || s[["hi1"]] < 1)
  }))
  cat(sprintf("  %s: deming_power_sim joint %.3f | harness Sadler %.3f, Wald chi-sq %.3f, Wald F %.3f\n",
              nm, r$power_joint, mean(rej[, "SS"]), mean(rej[, "C"]), mean(rej[, "F"])))
  ok(sprintf("deming_power_sim either-CI power reproduced (%s)", nm),
     abs(r$power_either_ci - mean(rej[, "either"])) < 1e-12)
  ok(sprintf("deming_power_sim joint power == Sadler reproduction (%s)", nm),
     abs(r$power_joint - mean(rej[, "SS"])) < 1e-12)
}

# 5. Generalised Bradley-Blackwood test: matches anova() of nested lm fits,
#    reduces to the classic test (d on mean) at lambda = 1, and is exact
#    at n = 10 (constant normal errors, known lambda)
set.seed(5)
d <- gen_h0(15, c(50, 100), 4, 0.2)
dd <- d$y - d$x; s <- d$x + 4 * d$y
ok("bb_p == anova(lm(d ~ 0), lm(d ~ s))",
   abs(bb_p(d$x, d$y, 4) - anova(lm(dd ~ 0), lm(dd ~ s))[2, "Pr(>F)"]) < 1e-12)
ok("bb_p at lambda = 1 == classic Bradley-Blackwood (d on mean)",
   abs(bb_p(d$x, d$y, 1) -
         anova(lm(dd ~ 0), lm(dd ~ I((d$x + d$y) / 2)))[2, "Pr(>F)"]) < 1e-12)
set.seed(6)
r <- replicate(20000, {
  d <- gen_h0(10, c(50, 100), 4, 0.2)
  bb_p(d$x, d$y, 4) < 0.05
})
cat(sprintf("  BB type I at n = 10, lambda = 4: %.4f (MC SE 0.0015)\n", mean(r)))
ok("BB type I within 3 MC SE of 0.05 at n = 10", abs(mean(r) - 0.05) < 3 * 0.0015)

# 6. Sadler (2010) sum-of-squares test: the Deming fit (calc_dem) minimises
#    S(a, b) (plain and weighted), and type I error at n = 3 and 10
for (wtd in c(FALSE, TRUE)) {
  set.seed(8)
  d <- gen_h0(20, c(5, 100), 4, 0.2, if (wtd) "prop" else "const")
  w <- if (wtd) dem_weights(d$x, d$y, 4) else rep(1, 20)
  f <- calc_dem(d$x, d$y, w, 4)
  o <- optim(c(f$b0, f$b1) + c(0.5, 0.05),
             function(p) ss_S(d$x, d$y, 4, p[1], p[2], w),
             control = list(reltol = 1e-14, maxit = 1e4))
  ok(sprintf("Deming fit minimises Sadler S(a, b) (weighted = %s)", wtd),
     ss_S(d$x, d$y, 4, f$b0, f$b1, w) <= o$value * (1 + 1e-8))
}
for (n in c(3, 10)) {
  set.seed(9)
  r <- replicate(20000, {
    d <- gen_h0(n, c(50, 100), 4, 0.2)
    ss_p(d$x, d$y, 4) < 0.05
  })
  cat(sprintf("  Sadler SS type I at n = %d, lambda = 4: %.4f (MC SE 0.0015)\n",
              n, mean(r)))
}
