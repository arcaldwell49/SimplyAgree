suppressMessages(devtools::load_all("C:/GitHub/SimplyAgree", quiet = TRUE))
set.seed(6)
gd = data.frame(id = rep(1:20, length.out = 366))
gd$x = rnorm(366, 100, 10)
gd$y = gd$x - 0.8 - rnorm(20, 0, 0.5)[gd$id] + rnorm(366, 0, 1)
for (bt in c("joint", "iu")) for (tm in c("approx", "perc")) {
  set.seed(1)
  r = suppressMessages(tolerance_limit(gd, x = "x", y = "y", id = "id", tol_method = tm, bound_type = bt, replicates = 999))
  cat(sprintf("%-5s %-6s TL: %6.3f %6.3f\n", bt, tm, r$limits$lower.TL, r$limits$upper.TL))
}
cat(sprintf("PL: %6.3f %6.3f\n", r$limits$lower.PL, r$limits$upper.PL))
a = agreement_limit(x = "x", y = "y", id = "id", data = gd, data_type = "nest")
aj = agreement_limit(x = "x", y = "y", id = "id", data = gd, data_type = "nest", bound_type = "joint")
cat(sprintf("agreement_limit nest LoA: %6.3f %6.3f; iu: %6.3f %6.3f; joint: %6.3f %6.3f\n",
            a$loa$lower_loa, a$loa$upper_loa, a$loa$lower_loa_ci, a$loa$upper_loa_ci, aj$loa$lower_loa_ci, aj$loa$upper_loa_ci))
print(a)
