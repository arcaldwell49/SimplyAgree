suppressMessages(devtools::load_all("C:/GitHub/SimplyAgree", quiet = TRUE))
set.seed(5); n = 30
d = data.frame(x = rnorm(n, 100, 10)); d$y = d$x + rnorm(n, 0.5, 2)
t = tolerance_limit(d, x = "x", y = "y"); print(unlist(t$limits[, c("lower.TL", "upper.TL")]))  # -6.1724 4.5375
set.seed(1)
tb = tolerance_limit(d, x = "x", y = "y", tol_method = "perc", replicates = 999)
print(unlist(tb$limits[, c("lower.TL", "upper.TL", "lower.TL.level", "upper.TL.level")]))
set.seed(6)
gd = data.frame(id = rep(1:20, length.out = 366))
gd$x = rnorm(366, 100, 10)
gd$y = gd$x - 0.8 - rnorm(20, 0, 0.5)[gd$id] + rnorm(366, 0, 1)
for (bt in c("joint", "iu")) for (tm in c("approx", "perc")) {
  set.seed(1)
  tm0 = Sys.time()
  r = tolerance_limit(gd, x = "x", y = "y", id = "id", tol_method = tm, bound_type = bt, replicates = 199)
  cat(sprintf("%-5s %-6s TL: %6.3f %6.3f  levels: %s  (%.1fs)\n", bt, tm, r$limits$lower.TL, r$limits$upper.TL,
              if (tm == "perc") paste(round(c(r$limits$lower.TL.level, r$limits$upper.TL.level), 3), collapse = " ") else "-",
              as.numeric(Sys.time() - tm0, units = "secs")))
}
set.seed(1)
r = tolerance_limit(gd, x = "x", y = "y", id = "id", cor_type = "ar1", tol_method = "perc", replicates = 50)
print(r$limits[, c("SD.df", "lower.TL", "upper.TL", "lower.TL.level", "upper.TL.level")])
set.seed(2)
d1 = data.frame(id = rep(1:40, 2), condition = rep(c("A", "B"), each = 40))
d1$x = rnorm(80, 100, 10); d1$y = d1$x + rnorm(80, 0, ifelse(d1$condition == "A", 1, 5))
r = tolerance_limit(d1, x = "x", y = "y", id = "id", condition = "condition", prop_bias = TRUE, tol_method = "perc", replicates = 50)
print(r$limits[, c("condition", "avg", "lower.TL", "upper.TL", "lower.TL.level")])
