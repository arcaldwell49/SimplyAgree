## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
knitr::opts_chunk$set(comment = ">")
options(digits = 2)
options(knitr.kable.NA = '')

pkgs <- c("effectsize", "dplyr", "parameters", "correlation")
if (!all(sapply(pkgs, requireNamespace))) {
  knitr::opts_chunk$set(eval = FALSE)
}

set.seed(333)

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(effectsize)
library(dplyr)

lm(Sepal.Length ~ Petal.Length, data = iris) %>% 
  standardize_parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(parameters)

cor.test(iris$Sepal.Length, iris$Petal.Length) %>% 
  model_parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
df <- iris[, 1:4]  # Remove the Species factor
correlation::correlation(df, partial = TRUE)[1:3, 1:3] # Select the rows of interest

## ---- warning=FALSE, message=FALSE--------------------------------------------
model <- lm(Sepal.Length ~ ., data = df)
params <- model_parameters(model)

t_to_r(params$t[2:4], params$df_error[2:4])

## ---- warning=FALSE, message=FALSE--------------------------------------------
model %>% 
  standardize_parameters() 

## ---- warning=FALSE, message=FALSE--------------------------------------------
# Select portion of data containing the two levels of interest
data <- iris[iris$Species %in% c("setosa", "versicolor"), ]

lm(Sepal.Length ~ Species, data = data) %>% 
  standardize_parameters()

## ---- warning=FALSE, message=FALSE--------------------------------------------
cohens_d(Sepal.Length ~ Species, data = data) 

## -----------------------------------------------------------------------------
(parameters <- lm(Sepal.Length ~ Species, data = data) %>% 
  model_parameters())
t_to_d(10.52, df_error = 98)

## ---- warning=FALSE, message=FALSE--------------------------------------------
lm(Sepal.Length ~ Species, data = data) %>%
  standardize_parameters(method = "smart")

## ---- warning=FALSE, message=FALSE--------------------------------------------
glass_delta(data$Sepal.Length[data$Species=="versicolor"],
            data$Sepal.Length[data$Species=="setosa"])
# glass_delta takes SD from second group

