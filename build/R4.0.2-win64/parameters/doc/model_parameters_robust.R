## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
options(digits = 2)
knitr::opts_chunk$set(comment = "#>")

if (!requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("sandwich", quietly = TRUE) ||
    !requireNamespace("lme4", quietly = TRUE) ||
    !requireNamespace("clubSandwich", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(parameters)
  library(dplyr)
}

set.seed(333)

## -----------------------------------------------------------------------------
data(iris)
model <- lm(Petal.Length ~ Sepal.Length * Species + Sepal.Width, data = iris)

# model parameters, where SE, CI and p-values are based on robust estimation
mp <- model_parameters(model, robust = TRUE)
mp

# compare standard errors to result from sandwich-package
mp$SE
unname(sqrt(diag(sandwich::vcovHC(model))))

## -----------------------------------------------------------------------------
# change estimation-type
mp <- model_parameters(model, robust = TRUE, vcov_estimation = "CL", vcov_type = "HC1")
mp

# compare standard errors to result from sandwich-package
mp$SE
unname(sqrt(diag(sandwich::vcovCL(model))))

## -----------------------------------------------------------------------------
iris$cluster <- factor(rep(LETTERS[1:8], length.out = nrow(iris)))
# change estimation-type, defining additional arguments
mp <- model_parameters(
  model, 
  robust = TRUE, 
  vcov_estimation = "CL", 
  vcov_type = "HC1",
  vcov_args = list(cluster = iris$cluster)
)
mp

# compare standard errors to result from sandwich-package
mp$SE
unname(sqrt(diag(sandwich::vcovCL(model, cluster = iris$cluster))))

## -----------------------------------------------------------------------------
# create fake-cluster-variable, to demonstrate cluster robust standard errors
iris$cluster <- factor(rep(LETTERS[1:8], length.out = nrow(iris)))

# cluster-robust estimation
mp <- model_parameters(
  model, 
  robust = TRUE, 
  vcov_estimation = "CR", 
  vcov_type = "CR1", 
  vcov_args = list(cluster = iris$cluster)
)
mp

# compare standard errors to result from clubSsandwich-package
mp$SE
unname(sqrt(diag(clubSandwich::vcovCR(model, type = "CR1", cluster = iris$cluster))))

## -----------------------------------------------------------------------------
# model parameters, robust estimation on standardized model
model_parameters(model, standardize = "refit", robust = TRUE)

## -----------------------------------------------------------------------------
library(lme4)
data(iris)
set.seed(1234)
iris$grp <- as.factor(sample(1:3, nrow(iris), replace = TRUE))

# fit example model
model <- lme4::lmer(
  Sepal.Length ~ Species * Sepal.Width + Petal.Length + (1 | grp),
  data = iris
)

# normal model parameters, like from 'summary()'
model_parameters(model)

# model parameters, cluster robust estimation for mixed models
model_parameters(
  model, 
  robust = TRUE, 
  vcov_estimation = "CR", 
  vcov_type = "CR1", 
  vcov_args = list(cluster = iris$grp)
)

## -----------------------------------------------------------------------------
# model parameters, cluster robust estimation on standardized mixed model
model_parameters(
  model, 
  standardize = "refit",
  robust = TRUE, 
  vcov_estimation = "CR", 
  vcov_type = "CR1", 
  vcov_args = list(cluster = iris$grp)
)

