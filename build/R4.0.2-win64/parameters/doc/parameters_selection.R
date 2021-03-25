## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
options(digits = 2)
knitr::opts_chunk$set(comment = "#>")

if (!requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("performance", quietly = TRUE) ||
    !requireNamespace("rstanarm", quietly = TRUE) ||
    !requireNamespace("lme4", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(parameters)
  library(dplyr)
}

set.seed(333)

## ----message=FALSE, warning=FALSE---------------------------------------------
model <- lm(Sepal.Length ~ .*., data = iris)
summary(model)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(performance)

check_normality(model)
check_heteroscedasticity(model)
check_autocorrelation(model)
check_collinearity(model)

## ----message=FALSE, warning=FALSE---------------------------------------------
lm(Sepal.Length ~ .*., data = iris) %>% 
  select_parameters() %>% 
  summary()

## ----message=FALSE, warning=FALSE---------------------------------------------
library(lme4)
data("qol_cancer")

# multiple models are checked, however, initial models
# already seems to be the best one...
lmer(
  QoL ~ time + phq4 + age + (1 + time | hospital / ID),
  data = qol_cancer
) %>% 
  select_parameters() %>%
  summary()

## ----message=FALSE, warning=FALSE---------------------------------------------
library(rstanarm)
model <- stan_glm(
  mpg ~ ., data = mtcars,
  iter = 500, refresh = 0, verbose = FALSE
)
select_parameters(model, cross_validation = TRUE)

