# mdc_test on the difference score vs tolerance_limit SD.within ----------
args = commandArgs(trailingOnly = TRUE)
pkgload::load_all(args[1], quiet = TRUE, export_all = FALSE)
data("reps")
d = reps
d$D = d$x - d$y
d$occ = factor(ave(d$D, d$id, FUN = seq_along))
print(table(table(d$id)))
tl = tolerance_limit(data = d, x = "x", y = "y", id = "id", model = "lme")
cat("1.96*sqrt(2)*SD.within =", 1.96 * sqrt(2) * tl$limits$SD.within[1], "\n")
r = try(mdc_test(D ~ occ | id, data = d, model = "oneway", method = "reml"))
print(r)
