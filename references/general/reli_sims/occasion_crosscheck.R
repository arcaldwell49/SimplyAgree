# Phase C1: reference values for reli_facet_test() with occasions (the
# crossed p x i x o design), hard-coded in tests/testthat/test-reli_facet_test.R.
# None of these packages is a dependency. Run from the package root:
#   Rscript references/general/reli_sims/occasion_crosscheck.R <lib>
# Versions used (2026-10-20): VCA 1.5.2, gtheoryr 0.2.0 (CRAN source),
# lme4 as installed. The data come from facet_test_data() below, which the
# test file repeats.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) .libPaths(c(args[1], .libPaths()))
suppressMessages(devtools::load_all(".", quiet = TRUE))
for (p in c("VCA", "gtheoryr")) cat(p, as.character(packageVersion(p)), "\n")

occasion_test_data <- function(seed, n = 15, k = 3, m = 3, r = 1,
                               v = c(p = 1, i = 0.2, o = 0.15, pi = 0.1,
                                     po = 0.1, io = 0.05, e = 0.3)) {
  set.seed(seed)
  d <- expand.grid(rep = seq_len(r), occ = paste0("day", seq_len(m)),
                   items = paste0("R", seq_len(k)), id = seq_len(n))
  ii <- as.integer(d$items); oo <- as.integer(d$occ)
  p <- rnorm(n, 0, sqrt(v[["p"]])); it <- rnorm(k, 0, sqrt(v[["i"]]))
  o <- rnorm(m, 0, sqrt(v[["o"]]))
  pi <- matrix(rnorm(n * k, 0, sqrt(v[["pi"]])), n, k)
  po <- matrix(rnorm(n * m, 0, sqrt(v[["po"]])), n, m)
  io <- matrix(rnorm(k * m, 0, sqrt(v[["io"]])), k, m)
  d$values <- 20 + p[d$id] + it[ii] + o[oo] + pi[cbind(d$id, ii)] +
    po[cbind(d$id, oo)] + io[cbind(ii, oo)] + rnorm(nrow(d), 0, sqrt(v[["e"]]))
  d$id <- factor(d$id)
  d
}
cr <- occasion_test_data(301)
cr2 <- occasion_test_data(302, r = 2)
dig <- function(x) print(signif(x, 10))

fit_of <- function(d, method = "anova") {
  .facet_fit(.facet_prepare(values ~ items | id, data = d, occasion = "occ"), method)
}

for (nm in c("cr", "cr2")) {
  d <- get(nm)
  f <- fit_of(d)
  cat("\n== ours, ANOVA,", nm, "==\n"); dig(f$ms); dig(f$df); dig(f$vc)
  form <- if (nm == "cr") values ~ id + items + occ + id:items + id:occ + items:occ else
    values ~ id + items + occ + id:items + id:occ + items:occ + id:items:occ
  va <- VCA::anovaVCA(form, Data = d, NegVC = TRUE, quiet = TRUE)
  cat("VCA anovaVCA:\n"); print(va$aov.tab[, c("DF", "MS", "VC")])
  fr <- fit_of(d, "reml")
  cat("ours, REML:\n"); dig(fr$vc)
  vr <- VCA::remlVCA(form, Data = d, quiet = TRUE)
  cat("VCA remlVCA:\n"); print(vr$aov.tab[, "VC", drop = FALSE])
}

# unbalanced (REML): 15 measurements removed
set.seed(303)
un <- cr[-sample(nrow(cr), 15), ]
fu <- fit_of(un, "reml")
cat("\n== ours, REML, unbalanced ==\n"); dig(fu$vc); dig(fu$df)
vu <- VCA::remlVCA(values ~ id + items + occ + id:items + id:occ + items:occ, Data = un, quiet = TRUE)
print(vu$aov.tab[, "VC", drop = FALSE])

# gtheoryr p x i x o G- and D-study ---------------------------------------------
g <- gtheoryr::gstudy_pxio(cr, person = "id", item = "items", occasion = "occ",
                           score = "values")
cat("\n== gtheoryr gstudy_pxio ==\n"); print(g$variance_components)
f <- fit_of(cr)
for (sz in list(c(1, 1), c(3, 3), c(2, 1))) {
  dd <- gtheoryr::dstudy_pxio(g, n_items = sz[1], n_occasions = sz[2])
  cat("\n== gtheoryr dstudy_pxio, items", sz[1], "occasions", sz[2], "==\n")
  print(unlist(dd[c("relative_error", "absolute_error", "g_coefficient", "phi_coefficient")]))
  sp <- function(stat, type) list(stat = stat, facet = "overall", type = type,
                                  k_avg = sz[1], m_avg = sz[2], r_avg = 1, scale = "raw")
  cat("ours:\n")
  dig(c(rel = .facet_estimate(f, sp("SEM", "consistency"))^2,
        abs = .facet_estimate(f, sp("SEM", "agreement"))^2,
        g = .facet_estimate(f, sp("ICC", "consistency")),
        phi = .facet_estimate(f, sp("ICC", "agreement"))))
}
