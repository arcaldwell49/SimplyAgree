suppressMessages(devtools::load_all("C:/GitHub/SimplyAgree", quiet = TRUE))
set.seed(5); n = 30
d = data.frame(x = rnorm(n, 100, 10)); d$y = d$x + rnorm(n, 0.5, 2)
t = tolerance_limit(d, x = "x", y = "y")
print(t$limits[, c("SD.df", "lower.TL", "upper.TL")])   # Howe: -6.1724 4.5375
dd = d$x - d$y; nu = n - 1
k_howe = sqrt(nu * (1 + 1/n) * qnorm(0.975)^2 / qchisq(0.05, nu))
print(mean(dd) + c(-1, 1) * k_howe * sd(dd))
ti = tolerance_limit(d, x = "x", y = "y", bound_type = "iu")
k1 = qt(0.95, n - 1, ncp = qnorm(0.975) * sqrt(n)) / sqrt(n)
print(c(ti$limits$lower.TL, ti$limits$upper.TL)); print(mean(dd) + c(-1, 1) * k1 * sd(dd))

set.seed(6)
gd = data.frame(id = rep(1:20, length.out = 366))
gd$x = rnorm(366, 100, 10)
gd$y = gd$x - 0.8 - rnorm(20, 0, 0.5)[gd$id] + rnorm(366, 0, 1)
for (bt in c("joint", "iu")) for (tm in c("approx", "perc")) {
  set.seed(1)
  r = tolerance_limit(gd, x = "x", y = "y", id = "id", tol_method = tm, bound_type = bt, replicates = 199)
  cat(bt, tm, ":", round(unlist(r$limits[, c("SD.df", "lower.PL", "upper.PL", "lower.TL", "upper.TL")]), 3), "\n")
}
r = tolerance_limit(gd, x = "x", y = "y", id = "id", condition = NULL, prop_bias = TRUE, tol_method = "perc", replicates = 50)
print(r$limits[, c("avg", "SD.df", "lower.TL", "upper.TL")])
set.seed(2)
d1 = data.frame(id = rep(1:40, 2), condition = rep(c("A", "B"), each = 40))
d1$x = rnorm(80, 100, 10); d1$y = d1$x + rnorm(80, 0, ifelse(d1$condition == "A", 1, 5))
r = tolerance_limit(d1, x = "x", y = "y", id = "id", condition = "condition")
print(r$limits[, c("condition", "SD", "SD.df", "lower.TL", "upper.TL")])
