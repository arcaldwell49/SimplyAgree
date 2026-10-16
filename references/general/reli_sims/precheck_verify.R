# Pre-check (Section 12): numerical checks of the design-doc formulas
# (F-test type I error, CIs vs psych, Var(ybar), z-scale SE, exact power, identities).
# Standalone (does not load the package). Written 2026-10-15 on branch reli-update.
# Results are recorded in references/general/reli_redesign.md (Section 12).

set.seed(2026)
ms <- function(Y) {
  n <- nrow(Y); k <- ncol(Y); gm <- mean(Y); rm <- rowMeans(Y); cm <- colMeans(Y)
  SSB <- k * sum((rm - gm)^2); SSJ <- n * sum((cm - gm)^2); SST <- sum((Y - gm)^2); SSE <- SST - SSB - SSJ
  list(n = n, k = k, MSB = SSB/(n-1), MSJ = SSJ/(k-1), MSE = SSE/((n-1)*(k-1)), MSW = (SSJ+SSE)/(n*(k-1)), gm = gm)
}
sim_Y <- function(n, k, vid, vit, ve, oneway = FALSE, items = NULL) {
  id <- rnorm(n, 0, sqrt(vid))
  it <- if (oneway) rep(0, k) else if (is.null(items)) rnorm(k, 0, sqrt(vit)) else items
  e <- matrix(rnorm(n*k, 0, sqrt(ve)), n)
  if (oneway) e <- e + matrix(rnorm(n*k, 0, sqrt(vit)), n)  # rater effects unique per cell
  outer(id, rep(1, k)) + outer(rep(1, n), it) + e
}
# ---- doc test statistics ----
Ftest <- function(m, form, r0) with(m, switch(form,
  "1"  = list(F = MSB/MSW*(1-r0)/(1+(k-1)*r0), d1 = n-1, d2 = n*(k-1)),
  "k"  = list(F = MSB/MSW*(1-r0),              d1 = n-1, d2 = n*(k-1)),
  "C1" = list(F = MSB/MSE*(1-r0)/(1+(k-1)*r0), d1 = n-1, d2 = (n-1)*(k-1)),
  "Ck" = list(F = MSB/MSE*(1-r0),              d1 = n-1, d2 = (n-1)*(k-1)),
  "A1" = { a <- k*r0/(n*(1-r0)); b <- 1 + k*r0*(n-1)/(n*(1-r0))
           list(F = MSB/(a*MSJ+b*MSE), d1 = n-1, d2 = (a*MSJ+b*MSE)^2/((a*MSJ)^2/(k-1) + (b*MSE)^2/((n-1)*(k-1)))) },
  "Ak" = { a <- r0/(n*(1-r0)); b <- 1 + r0*(n-1)/(n*(1-r0))
           list(F = MSB/(a*MSJ+b*MSE), d1 = n-1, d2 = (a*MSJ+b*MSE)^2/((a*MSJ)^2/(k-1) + (b*MSE)^2/((n-1)*(k-1)))) }))
cat("== 1. Type I error of doc F tests (H0: rho = 0.3, one-sided, alpha .05; n=15, k=3; 4000 reps) ==\n")
r0 <- 0.3; n <- 15; k <- 3; vit <- 0.2; ve <- 0.5
vid_for <- function(form) switch(form, "1" = r0*(vit+ve)/(1-r0), "k" = r0*(vit+ve)/k/(1-r0),
  "C1" = r0*ve/(1-r0), "Ck" = r0*ve/k/(1-r0), "A1" = r0*(vit+ve)/(1-r0), "Ak" = r0*(vit+ve)/k/(1-r0))
fixed_items <- rnorm(k, 0, sqrt(vit))
for (form in c("1", "k", "C1", "Ck", "A1", "Ak")) {
  rej <- mean(replicate(4000, {
    Y <- sim_Y(n, k, vid_for(form), vit, ve, oneway = form %in% c("1", "k"),
               items = if (form %in% c("C1", "Ck")) fixed_items else NULL)
    t <- Ftest(ms(Y), form, r0); pf(t$F, t$d1, t$d2, lower.tail = FALSE) < .05 }))
  cat(sprintf("  %-3s rejection rate %.3f\n", form, rej))
}
cat("\n== 2. Doc two-sided CIs vs psych 2.6.3 (Shrout & Fleiss data) ==\n")
Y <- as.matrix(data.frame(J1=c(9,6,8,7,10,6),J2=c(2,1,4,1,5,2),J3=c(5,3,6,2,6,4),J4=c(8,2,8,6,9,7)))
m <- ms(Y); a2 <- .025
ci <- with(m, {
  F1 <- MSB/MSW; FL <- F1/qf(1-a2, n-1, n*(k-1)); FU <- F1*qf(1-a2, n*(k-1), n-1)
  F3 <- MSB/MSE; GL <- F3/qf(1-a2, n-1, (n-1)*(k-1)); GU <- F3*qf(1-a2, (n-1)*(k-1), n-1)
  r <- (MSB-MSE)/(MSB+(k-1)*MSE+k*(MSJ-MSE)/n)
  aa <- k*r/(n*(1-r)); bb <- 1 + k*r*(n-1)/(n*(1-r))
  v <- (aa*MSJ+bb*MSE)^2/((aa*MSJ)^2/(k-1) + (bb*MSE)^2/((n-1)*(k-1)))
  Fl <- qf(1-a2, n-1, v); Fu <- qf(1-a2, v, n-1)
  L2 <- n*(MSB-Fl*MSE)/(Fl*(k*MSJ+(k*n-k-n)*MSE)+n*MSB); U2 <- n*(Fu*MSB-MSE)/(k*MSJ+(k*n-k-n)*MSE+n*Fu*MSB)
  # S&F eq 7 df form, as in current code
  Fj <- MSJ/MSE
  v_sf <- (k-1)*(n-1)*(k*r*Fj + n*(1+(k-1)*r) - k*r)^2/((n-1)*k^2*r^2*Fj^2 + (n*(1+(k-1)*r) - k*r)^2)
  # Stata A,k: special case of (6) with k = 1, a,b from rho_Ak-hat
  rk <- (MSB-MSE)/(MSB+(MSJ-MSE)/n); ak <- rk/(n*(1-rk)); bk <- 1 + rk*(n-1)/(n*(1-rk))
  vk <- (ak*MSJ+bk*MSE)^2/((ak*MSJ)^2/(k-1) + (bk*MSE)^2/((n-1)*(k-1)))
  Flk <- qf(1-a2, n-1, vk); Fuk <- qf(1-a2, vk, n-1)
  Lk <- n*(MSB-Flk*MSE)/(Flk*(MSJ+(n-1-n)*MSE)+n*MSB); Uk <- n*(Fuk*MSB-MSE)/(MSJ+(n-1-n)*MSE+n*Fuk*MSB)
  cat(sprintf("  Satterthwaite v: a,b form %.6f   S&F eq.7 form %.6f\n", v, v_sf))
  rbind(ICC1 = c((FL-1)/(FL+k-1), (FU-1)/(FU+k-1)), ICC2 = c(L2, U2), ICC3 = c((GL-1)/(GL+k-1), (GU-1)/(GU+k-1)),
        ICC1k = c(1-1/FL, 1-1/FU), ICC2k_SB = c(k*L2/(1+(k-1)*L2), k*U2/(1+(k-1)*U2)),
        ICC2k_Stata = c(Lk, Uk), ICC3k = c(1-1/GL, 1-1/GU))
})
p <- psych::ICC(as.data.frame(Y), alpha = .05, lmer = FALSE)$results[, 7:8]
print(round(cbind(doc = ci[c(1:5, 7), ], psych = as.matrix(p)), 5))
cat("  ICC(A,k) Stata-style (k=1 special case):", round(ci["ICC2k_Stata", ], 5), "\n")

cat("\n== 3. Var(ybar) expressions (n=12, k=3, 20000 reps): empirical var vs mean of formula ==\n")
n <- 12; k <- 3; vid <- 1; vit <- .4; ve <- .6
chk <- function(label, gen, f) { g <- replicate(20000, { Y <- gen(); c(mean(Y), f(ms(Y))) })
  cat(sprintf("  %-28s empirical %.5f  formula %.5f\n", label, var(g[1, ]), mean(g[2, ]))) }
it_fix <- rnorm(k, 0, sqrt(vit))
chk("consistency (items fixed)", function() sim_Y(n, k, vid, vit, ve, items = it_fix), function(m) m$MSB/(m$n*m$k))
chk("agreement (items random)",  function() sim_Y(n, k, vid, vit, ve), function(m) (m$MSB+m$MSJ-m$MSE)/(m$n*m$k))
chk("one-way",                   function() sim_Y(n, k, vid, vit, ve, oneway = TRUE), function(m) m$MSB/(m$n*m$k))

cat("\n== 4. Studentizing scale: z = 0.5*log F; sd(z) vs sqrt(.5(1/d1+1/d2)) vs exact trigamma ==\n")
for (nk in list(c(10, 2), c(20, 3), c(40, 4))) { n <- nk[1]; k <- nk[2]; d1 <- n-1; d2 <- (n-1)*(k-1)
  z <- replicate(20000, { m <- ms(sim_Y(n, k, 1, .2, .5)); r <- (m$MSB-m$MSE)/(m$MSB+(k-1)*m$MSE)
    0.5*log((1+(k-1)*r)/(1-r)) })
  cat(sprintf("  n=%d k=%d: sd(z) %.4f  approx %.4f  exact %.4f\n", n, k, sd(z), sqrt(.5*(1/d1+1/d2)),
              0.5*sqrt(trigamma(d1/2)+trigamma(d2/2)))) }

cat("\n== 5. Exact power via scaled central F vs simulation (consistency, rho0=.5, rho1=.75, alpha .05 one-sided) ==\n")
for (nk in list(c(20, 2), c(30, 3))) { n <- nk[1]; k <- nk[2]; d1 <- n-1; d2 <- (n-1)*(k-1)
  R <- function(r) (1+(k-1)*r)/(1-r)
  exact <- pf(qf(.95, d1, d2)*R(.5)/R(.75), d1, d2, lower.tail = FALSE)
  simp <- mean(replicate(5000, { t <- Ftest(ms(sim_Y(n, k, .75*.5/.25, .2, .5)), "C1", .5); t$F > qf(.95, t$d1, t$d2) }))
  cat(sprintf("  n=%d k=%d: exact %.3f  simulated %.3f\n", n, k, exact, simp)) }

cat("\n== 6. Other identities ==\n")
Y <- sim_Y(25, 2, 1, .2, .5); m <- ms(Y); d <- Y[, 2] - Y[, 1]
cat(sprintf("  k=2: 1.96*sqrt(2)*sqrt(MSE) = %.6f ; 1.96*sd(diff) = %.6f\n", 1.96*sqrt(2)*sqrt(m$MSE), 1.96*sd(d)))
Y <- sim_Y(25, 4, 1, .2, .5); m <- ms(Y)
cat(sprintf("  agreement SEM^2 (MSJ+(n-1)MSE)/n = %.6f ; MSW = %.6f\n", (m$MSJ+(m$n-1)*m$MSE)/m$n, m$MSW))
nu1 <- with(m, (MSJ+(n-1)*MSE)^2/(MSJ^2/(k-1) + ((n-1)*MSE)^2/((n-1)*(k-1))))
nu2 <- with(m, {c1 <- 1/n; c2 <- (n-1)/n; (c1*MSJ+c2*MSE)^2/((c1*MSJ)^2/(k-1) + (c2*MSE)^2/((n-1)*(k-1)))})
cat(sprintf("  Satterthwaite nu, doc form %.6f ; general form %.6f\n", nu1, nu2))
for (s in c(.05, .10, .15, .25)) cat(sprintf("  sympercent %4.1f -> CV%% %6.2f\n", 100*s, 100*sqrt(exp(s^2)-1)))
