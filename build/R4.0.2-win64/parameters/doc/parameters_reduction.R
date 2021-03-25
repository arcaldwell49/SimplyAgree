## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
options(digits = 2)
knitr::opts_chunk$set(comment = "#>")

if (!requireNamespace("dplyr", quietly = TRUE) ||
    !requireNamespace("psych", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
} else {
  library(parameters)
  library(dplyr)
}

set.seed(333)

## ----message=FALSE, warning=FALSE---------------------------------------------
model <- lm(rating ~ ., data = attitude)
parameters(model)

## ----message=FALSE, warning=FALSE---------------------------------------------
newmodel <- reduce_parameters(model)
parameters(newmodel)

## ----message=FALSE, warning=FALSE---------------------------------------------
reduce_parameters(model, method = "cMDS") %>% 
  parameters()

## ----message=FALSE, warning=FALSE---------------------------------------------
pca <- principal_components(insight::get_predictors(model), n = "auto")
pca

## ----message=FALSE, warning=FALSE---------------------------------------------
newdata <- predict(pca)
newdata$rating <- attitude$rating

## ----message=FALSE, warning=FALSE---------------------------------------------
update(model, rating ~ PC1, data = newdata) %>% 
  parameters()

## ----message=FALSE, warning=FALSE---------------------------------------------
library(psych)

# Fit the PCA
pca <- psych::principal(attitude, nfactors = 1) %>% 
  model_parameters()
pca

## ----message=FALSE, warning=FALSE---------------------------------------------
df <- cbind(attitude, predict(pca))

update(model, rating ~ PC1, data = df) %>% 
  model_parameters()

