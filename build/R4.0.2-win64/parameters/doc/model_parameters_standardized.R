## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
options(digits = 2)
knitr::opts_chunk$set(comment = "#>")

if (!requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("lme4", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(parameters)
  library(dplyr)
}

set.seed(333)

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

# classic model parameters
model_parameters(model)

# standardized model parameters
model_parameters(model, standardize = "refit")

## -----------------------------------------------------------------------------
# standardize continuous variables manually
model2 <- lme4::lmer(
  scale(Sepal.Length) ~ Species * scale(Sepal.Width) + scale(Petal.Length) + (1 | grp),
  data = iris
)
model_parameters(model2)

## -----------------------------------------------------------------------------
model_parameters(model, standardize = "posthoc")

## -----------------------------------------------------------------------------
model_parameters(model, standardize = "basic")

## -----------------------------------------------------------------------------
model_parameters(model, standardize = "smart")

