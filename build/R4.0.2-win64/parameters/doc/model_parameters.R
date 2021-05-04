## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
options(digits = 2)
knitr::opts_chunk$set(comment = "#>")

if (!requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("BayesFactor", quietly = TRUE) ||
    !requireNamespace("lme4", quietly = TRUE) ||
    !requireNamespace("metafor", quietly = TRUE) ||
    !requireNamespace("lavaan", quietly = TRUE) ||
    !requireNamespace("brms", quietly = TRUE) ||
    !requireNamespace("psych", quietly = TRUE) ||
    !requireNamespace("rstanarm", quietly = TRUE) ||
    !requireNamespace("FactoMineR", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(parameters)
  library(dplyr)
}

set.seed(333)

## ---- warning=FALSE, message=FALSE--------------------------------------------
cor.test(iris$Sepal.Length, iris$Sepal.Width) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
t.test(mpg ~ vs, data = mtcars) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(BayesFactor)

BayesFactor::correlationBF(iris$Sepal.Length, iris$Sepal.Width) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
BayesFactor::ttestBF(formula = mpg ~ vs, data = mtcars) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
aov(Sepal.Length ~ Species, data = iris) %>%
  parameters(omega_squared = "partial", eta_squared = "partial", epsilon_squared = "partial")

## ---- warning=FALSE, message=FALSE--------------------------------------------
aov(mpg ~ am + Error(gear), data = mtcars) %>%
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
glm(vs ~ poly(mpg, 2) + cyl, data = mtcars) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(lme4)

lmer(Sepal.Width ~ Petal.Length + (1|Species), data = iris) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(GLMMadaptive)
library(glmmTMB)
data("Salamanders")
model <- mixed_model(
  count ~ spp + mined,
  random = ~1 | site,
  zi_fixed = ~spp + mined,
  family = zi.negative.binomial(), 
  data = Salamanders
)
parameters(model)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(glmmTMB)
sim1 <- function(nfac = 40, nt = 100, facsd = 0.1, tsd = 0.15, mu = 0, residsd = 1) {
  dat <- expand.grid(fac = factor(letters[1:nfac]), t = 1:nt)
  n <- nrow(dat)
  dat$REfac <- rnorm(nfac, sd = facsd)[dat$fac]
  dat$REt <- rnorm(nt, sd = tsd)[dat$t]
  dat$x <- rnorm(n, mean = mu, sd = residsd) + dat$REfac + dat$REt
  dat
}
set.seed(101)
d1 <- sim1(mu = 100, residsd = 10)
d2 <- sim1(mu = 200, residsd = 5)
d1$sd <- "ten"
d2$sd <- "five"
dat <- rbind(d1, d2)
model <- glmmTMB(x ~ sd + (1 | t), dispformula =  ~ sd, data = dat)

parameters(model)

## ---- warning=FALSE, message=FALSE, eval = FALSE------------------------------
#  library(rstanarm)
#  
#  stan_glm(mpg ~ wt * cyl, data = mtcars) %>%
#    parameters()

## ---- warning=FALSE, message=FALSE, echo = FALSE------------------------------
library(rstanarm)

stan_glm(mpg ~ wt * cyl, data = mtcars, iter = 500, chains = 2, refresh = 0) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE, eval=FALSE--------------------------------
#  library(brms)
#  data(fish)
#  set.seed(123)
#  model <- brm(bf(
#     count ~ persons + child + camper + (1 | persons),
#     zi ~ child + camper + (1 | persons)
#   ),
#   data = fish,
#   family = zero_inflated_poisson()
#  )
#  parameters(model, component = "conditional")
#  #> Parameter   | Median |         89% CI |     pd | % in ROPE | ESS |  Rhat
#  #> ------------------------------------------------------------------------
#  #> b_Intercept |  -0.87 | [-1.49, -0.08] | 96.80% |     4.80% |  78 | 1.000
#  #> b_persons   |   0.84 | [ 0.60,  1.06] |   100% |        0% |  75 | 0.997
#  #> b_child     |  -1.16 | [-1.32, -1.00] |   100% |        0% | 107 | 1.027
#  #> b_camper1   |   0.74 | [ 0.52,  0.91] |   100% |        0% | 224 | 0.993
#  
#  parameters(model, effects = "all", component = "all")
#  #> # Fixed Effects (Count Model)
#  #>
#  #> Parameter   | Median |         89% CI |     pd | % in ROPE | ESS |  Rhat
#  #> ------------------------------------------------------------------------
#  #> (Intercept) |  -0.87 | [-1.49, -0.08] | 96.80% |     4.80% |  78 | 1.000
#  #> persons     |   0.84 | [ 0.60,  1.06] |   100% |        0% |  75 | 0.997
#  #> child       |  -1.16 | [-1.32, -1.00] |   100% |        0% | 107 | 1.027
#  #> camper1     |   0.74 | [ 0.52,  0.91] |   100% |        0% | 224 | 0.993
#  #>
#  #> # Fixed Effects (Zero-Inflated Model)
#  #>
#  #> Parameter   | Median |         89% CI |     pd | % in ROPE | ESS |  Rhat
#  #> ------------------------------------------------------------------------
#  #> (Intercept) |  -0.76 | [-1.66,  0.51] | 87.20% |    10.40% |  98 | 0.992
#  #> child       |   1.87 | [ 1.37,  2.43] |   100% |        0% | 262 | 0.999
#  #> camper1     |  -0.83 | [-1.44, -0.22] | 99.20% |     0.80% | 168 | 0.997
#  #>
#  #> # Random Effects (Count Model)
#  #>
#  #> Parameter | Median |        89% CI |     pd | % in ROPE | ESS |  Rhat
#  #> ---------------------------------------------------------------------
#  #> persons.1 |  -0.01 | [-0.40, 0.35] | 59.20% |    57.60% |  80 | 1.012
#  #> persons.2 |   0.03 | [-0.15, 0.33] | 61.60% |    60.80% |  88 | 0.994
#  #> persons.3 |  -0.02 | [-0.38, 0.11] | 63.20% |    64.80% |  66 | 1.008
#  #> persons.4 |   0.00 | [-0.51, 0.29] | 51.20% |    62.40% |  76 | 0.992
#  #>
#  #> # Random Effects (Zero-Inflated Model)
#  #>
#  #> Parameter | Median |         89% CI |     pd | % in ROPE | ESS |  Rhat
#  #> ----------------------------------------------------------------------
#  #> persons.1 |   1.38 | [ 0.58,  2.66] | 97.60% |     1.60% | 108 | 0.992
#  #> persons.2 |   0.27 | [-0.62,  1.40] | 68.80% |    13.60% | 100 | 1.002
#  #> persons.3 |  -0.11 | [-1.36,  0.86] | 60.80% |    16.80% |  96 | 0.993
#  #> persons.4 |  -1.19 | [-2.62, -0.31] | 95.20% |     0.80% | 115 | 0.992

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(psych)

psych::pca(mtcars, nfactors = 3) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE, eval = FALSE------------------------------
#  library(FactoMineR)
#  
#  FactoMineR::FAMD(iris, ncp = 3) %>%
#    parameters()

## ---- warning=FALSE, message=FALSE, echo = FALSE------------------------------
library(FactoMineR)

FactoMineR::FAMD(iris, ncp = 3, graph = FALSE) %>% 
  parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(lavaan)

model <- lavaan::cfa(' visual  =~ x1 + x2 + x3
                       textual =~ x4 + x5 + x6
                       speed   =~ x7 + x8 + x9 ', 
                       data = HolzingerSwineford1939)

model_parameters(model)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(metafor)

mydat <- data.frame(
  effectsize = c(-0.393, 0.675, 0.282, -1.398),
  standarderror = c(0.317, 0.317, 0.13, 0.36)
)

rma(yi = effectsize, sei = standarderror, method = "REML", data = mydat) %>% 
  model_parameters()

