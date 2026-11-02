# reli_facet_test() on trending data -------------------------------------
# Do the facets of reli_facet_test(occasion =) answer the trending question?
# Run: Rscript facet_check.R <path-to-SimplyAgree>

args = commandArgs(trailingOnly = TRUE)
pkgload::load_all(args[1], quiet = TRUE, export_all = FALSE)

sim = function(n = 100, m = 6, sd_wt = 1, b_w = 1, sd_alt = 0.25, seed = 1) {
  set.seed(seed)
  id = rep(seq_len(n), each = m)
  mu = rep(rnorm(n, 5, 1), each = m)
  w = rnorm(n * m, 0, sd_wt)
  u = rep(rnorm(n, 0, 0.25), each = m)
  d = data.frame(id = id, occ = rep(seq_len(m), n),
                 ref = mu + w + rnorm(n * m, 0, 0.25),
                 alt = mu + b_w * w + u + rnorm(n * m, 0, sd_alt))
  rbind(data.frame(id = d$id, occ = d$occ, dev = "ref", y = d$ref),
        data.frame(id = d$id, occ = d$occ, dev = "alt", y = d$alt))
}

# components vs truth -----------------------------------------------------
for (s in list(c(1.5, 1), c(1.5, 0), c(0.3, 0))) {
  long = sim(sd_wt = s[1], b_w = s[2])
  cat("\n== sd_wt =", s[1], " b_w =", s[2],
      " (truth if b_w = 1: po SD =", s[1], ", resid SD = 0.25, pi SD = 0.177)\n")
  for (f in c("intra", "inter")) {
    r = reli_facet_test(y ~ dev | id, data = long, occasion = "occ", facet = f)
    cat(f, "ICC:", round(r$estimate, 3), " CI:", round(r$conf.int, 3), "\n")
  }
  print(r$components)
}
