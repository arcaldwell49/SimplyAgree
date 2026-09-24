library(nlme); library(emmeans)
set.seed(1)
d2 = data.frame(id = rep(1:40, each = 5))
d2$x = runif(200, 10, 200)
d2$y = d2$x + rnorm(40, 0, 1)[d2$id] + rnorm(200, 0, 0.05 * d2$x)
d2$avg = (d2$x + d2$y)/2; d2$delta = d2$x - d2$y
d2$condition = rep(c("A","B"), 100)
strip = function(m) { m$call$weights = NULL; m }

g = gls(delta ~ 1, data = d2, weights = varPower(form = ~avg),
        correlation = corCompSymm(form = ~1|id))
print(summary(emmeans(strip(g), ~1, data = d2, mode = "satterthwaite")))
print(c(coef(g), sqrt(vcov(g))))

# models where emmeans already works: stripped call must give identical results
g3 = gls(delta ~ condition, data = d2, weights = varIdent(form = ~1|condition),
         correlation = corCompSymm(form = ~1|id))
a = summary(emmeans(g3, ~condition, data = d2, mode = "satterthwaite"))
b = summary(emmeans(strip(g3), ~condition, data = d2, mode = "satterthwaite"))
print(a); print(all.equal(as.data.frame(a), as.data.frame(b)))
g4 = gls(delta ~ avg, data = d2, weights = varPower(form = ~avg),
         correlation = corCompSymm(form = ~1|id))
a = summary(emmeans(ref_grid(g4, at = list(avg = c(20, 100)), data = d2), ~avg, data = d2, mode = "satterthwaite"))
b = summary(emmeans(ref_grid(strip(g4), at = list(avg = c(20, 100)), data = d2), ~avg, data = d2, mode = "satterthwaite"))
print(a); print(all.equal(as.data.frame(a), as.data.frame(b)))
