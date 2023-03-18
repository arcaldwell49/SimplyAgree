library(nlme)
library(emmeans)
library(SimplyAgree)
library(tidyverse)
data(reps)
data("reps")
data("temps")
reps2 = reps

# match.fun and do.call to do back transforms

reps2$y[5] = 6.35
reps2$y[9] = 4.09
reps2$delta = reps2$x - reps2$y
reps2$avg = (reps2$x + reps2$y )/ 2

# Simple average model -------
simple_model = gls(trec_delta ~ 1,
                   data = temps,
                   method = "REML")

# Response by condition model -----
simple_condition_model = update(simple_model,
                                ~ tod)

## Mean and variance by condition model ------
simp_con_hetvar = update(simple_condition_model,
                         weights = varIdent(form= ~ 1|tod))

# Simple covariate model -----

simp_cov_model = update(simple_model,
                        ~ trec_pre)

## Mean and variance by covariate model ----

simp_cov_hetvar = update(simp_cov_model,
                         weights = varFixed(~ trec_pre))

emmmeans(simple_condition_model, ~ )
#rg = ref_grid(simp_con_hetvar, at = list(trec_pre = c(36,36.5,37)))
#predict(rg, interval = "prediction")
#ggplot(temps, aes(x=trec_pre,y=trec_delta)) +geom_point()
predict.
## Covariate and condition model ----

simp_cov_model = update(simple_model,
                        ~ trec_pre + tod)
