library(nlme)
set.seed(4)
n = 30
# unsorted ids (like the review's d1), 2 rows per subject
d = data.frame(id = rep(1:n, 2), condition = rep(c("A","B"), each = n))
a = rnorm(n, 0, 2)
d$delta = a[d$id] + rnorm(2*n, 0, ifelse(d$condition == "A", 1, 3))
g = gls(delta ~ condition, d, correlation = corCompSymm(form = ~1|id),
        weights = varIdent(form = ~1|condition))
V = getVarCov(g, individual = "1")
cat("fitted within-subject covariance for id 1 (rows 1 and 31):\n"); print(round(V, 2))
mu = fitted(g)
sims = replicate(4000, SimplyAgree:::sim_gls(g, psim = 2, data = d) - mu)
S = cov(t(sims))
cat("old sim_gls: cov(rows 1,31) =", round(S[1,31],2), " var row1 =", round(S[1,1],2),
    " var row31 =", round(S[31,31],2), " cov(rows 1,2) [different ids] =", round(S[1,2],2), "\n")
