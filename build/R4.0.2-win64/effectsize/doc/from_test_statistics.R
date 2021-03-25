## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
library(effectsize)

knitr::opts_chunk$set(comment = ">")
options(digits = 2)
options(knitr.kable.NA = '')

pkgs <- c("effectsize", "afex", "lmerTest", "emmeans", "parameters")
if (!all(sapply(pkgs, requireNamespace))) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(afex)
  library(lmerTest)
  library(emmeans)
  library(parameters)
}

set.seed(747)

## -----------------------------------------------------------------------------
library(afex)

data(md_12.1)

aov_fit <- aov_car(rt ~ angle * noise + Error(id/(angle * noise)),
                   data = md_12.1,
                   anova_table=list(correction = "none", es = "pes"))
aov_fit

## -----------------------------------------------------------------------------
library(effectsize)

F_to_eta2(
  f = c(40.72, 33.77, 45.31),
  df = c(2, 1, 2),
  df_error = c(18, 9, 18)
)

## -----------------------------------------------------------------------------
library(emmeans)

joint_tests(aov_fit, by = "noise")

F_to_eta2(f = c(5, 79),
          df = 2,
          df_error = 29)

## -----------------------------------------------------------------------------
pairs(emmeans(aov_fit, ~ angle))

t_to_eta2(t = c(-5.7, -8.9, -3.2),
          df_error = 18)

## -----------------------------------------------------------------------------
library(lmerTest)

fit_lmm <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

anova(fit_lmm)

F_to_eta2(45.8, 1, 17)

## -----------------------------------------------------------------------------
model_parameters(fit_lmm, df_method = "satterthwaite")

t_to_eta2(6.77, df_error = 17)

## -----------------------------------------------------------------------------
F_to_eta2(45.8, 1, 17)
F_to_epsilon2(45.8, 1, 17)
F_to_omega2(45.8, 1, 17)

## -----------------------------------------------------------------------------
model_parameters(fit_lmm, df_method = "satterthwaite")

t_to_r(6.77, df_error = 17)

## -----------------------------------------------------------------------------

fit_lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris) 

model_parameters(fit_lm)

t_to_r(t = c(8.59, 27.57),
       df_error = 147)

## ---- eval=require(correlation, quietly = TRUE)-------------------------------
correlation::correlation(iris[,1:3], partial = TRUE)[1:2, c(1:3,7:8)]

## -----------------------------------------------------------------------------
pairs(emmeans(aov_fit, ~ angle))

t_to_r(t = c(-5.7, -8.9, -3.2),
       df_error = 18)

## -----------------------------------------------------------------------------
warp.lm <- lm(breaks ~ tension, data = warpbreaks)

pairs(emmeans(warp.lm,  ~ tension))

t_to_d(t = c(2.5, 3.7, 1.2),
       df_error = 51)


## -----------------------------------------------------------------------------

pairs(emmeans(aov_fit, ~ angle))

t_to_d(t = c(-5.7,-5.9,-3.2),
       df_error = 18,
       paired = TRUE)


