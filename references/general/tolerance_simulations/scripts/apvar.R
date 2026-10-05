library(nlme)
set.seed(1)
n = 20; k = 19
df = data.frame(id = rep(1:n, each = k), condition = rep(c("A","B"), length.out = n*k))
df$delta = 0.8 + rnorm(n, 0, .5)[df$id] + rnorm(n*k, 0, ifelse(df$condition=="A", 1, 2))
g0 = gls(delta ~ 1, df)
cat("plain gls apVar:\n"); str(g0$apVar)
g1 = gls(delta ~ 1, df, correlation = corCompSymm(form = ~1|id))
cat("CS apVar:\n"); print(g1$apVar); print(attr(g1$apVar, "Pars"))
cat("1/(2 var(lSigma)) =", 1/(2*g1$apVar["lSigma","lSigma"]), " N-p =", nrow(df)-1, "\n")
g2 = gls(delta ~ condition, df, correlation = corCompSymm(form = ~1|id),
         weights = varIdent(form = ~1|condition))
print(g2$apVar); print(attr(g2$apVar, "Pars"))
vs = g2$modelStruct$varStruct
print(coef(vs)); print(unique(varWeights(vs)))
coef(vs) = coef(vs) + 0.1
print(coef(vs)); print(unique(varWeights(vs)))
# independent data: apVar for lSigma vs 1/(2(N-p))
g3 = gls(delta ~ 1, df[1:30,], weights = varIdent(form = ~1|condition))
print(1/(2*g3$apVar["lSigma","lSigma"]))
