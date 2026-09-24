suppressMessages(devtools::load_all("C:/GitHub/SimplyAgree", quiet = TRUE))
suppressMessages(library(nlme))

# 1. covariance of new simulator (unsorted ids, CS + varIdent)
set.seed(4)
n = 30
d = data.frame(id = rep(1:n, 2), condition = rep(c("A","B"), each = n))
a = rnorm(n, 0, 2)
d$delta = a[d$id] + rnorm(2*n, 0, ifelse(d$condition == "A", 1, 3))
g = gls(delta ~ condition, d, correlation = corCompSymm(form = ~1|id),
        weights = varIdent(form = ~1|condition))
print(round(getVarCov(g, individual = "1"), 2))
st = gls_sim_setup(g, d)
sims = replicate(20000, gls_sim_draw(st) - st$mu)
S = cov(t(sims))
cat("new: cov(1,31) =", round(S[1,31],2), " var1 =", round(S[1,1],2),
    " var31 =", round(S[31,31],2), " cov(1,2) =", round(S[1,2],2), "\n")

# AR1 with time, shuffled rows
set.seed(5)
d3 = data.frame(id = rep(1:10, each = 6), time = rep(1:6, 10))
d3$delta = rnorm(60)
for (i in 1:10) d3$delta[d3$id == i] = arima.sim(list(ar = .6), 6)
d3 = d3[sample(60), ]
g3 = gls(delta ~ 1, d3, correlation = corAR1(form = ~time|id))
st3 = gls_sim_setup(g3, d3)
sims3 = replicate(20000, gls_sim_draw(st3) - st3$mu)
S3 = cov(t(sims3))
r = which(d3$id == 1); r = r[order(d3$time[r])]
cat("AR1 fitted:\n"); print(round(getVarCov(g3, individual = "1"), 2))
cat("AR1 simulated (id 1, time order):\n"); print(round(S3[r, r], 2))

# 2. review's Issue 2 check: SD of bootstrapped bias / model SE (expect ~1)
set.seed(3)
n = 20; k = 19
df = data.frame(id = rep(1:n, each = k)); a = rnorm(n, 0, 0.5)
df$delta = 0.8 + a[df$id] + rnorm(n * k, 0, 1)
gg = gls(delta ~ 1, data = df, correlation = corCompSymm(form = ~1 | id))
st = gls_sim_setup(gg, df)
cf = replicate(400, { d2 = df; d2$delta = gls_sim_draw(st); coef(update(gg, data = d2)) })
cat("sd(boot coef)/SE =", round(sd(cf) / sqrt(vcov(gg))[1], 3), "\n")

# 3. timing on a golfer-sized example (20 x ~18 = 366 rows, CS)
set.seed(6)
gd = data.frame(id = rep(1:20, length.out = 366))
gd$x = rnorm(366, 100, 10)
gd$y = gd$x - 0.8 - rnorm(20, 0, 0.5)[gd$id] + rnorm(366, 0, 1)
tm = system.time(tp <- tolerance_limit(gd, x = "x", y = "y", id = "id",
                                       tol_method = "perc", replicates = 199))
cat("perc, 199 replicates:", round(tm[["elapsed"]], 1), "s\n")
ta = tolerance_limit(gd, x = "x", y = "y", id = "id")
print(rbind(approx = ta$limits[, c("lower.PL","upper.PL","lower.TL","upper.TL")],
            perc = tp$limits[, c("lower.PL","upper.PL","lower.TL","upper.TL")]))
