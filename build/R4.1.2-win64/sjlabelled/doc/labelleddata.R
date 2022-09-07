## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
factor(c("low", "high", "mid", "high", "low"))

## -----------------------------------------------------------------------------
library(sjlabelled)
data(efc)
str(efc$e42dep)

## ----warning=FALSE, fig.height=6, fig.width=7---------------------------------
library(sjlabelled)
data(efc)
barplot(
  table(efc$e42dep, efc$e16sex), 
  beside = TRUE, 
  legend.text = TRUE
)

## ----warning=FALSE, fig.height=6, fig.width=7---------------------------------
barplot(
  table(sjlabelled::as_label(efc$e42dep),
        sjlabelled::as_label(efc$e16sex)), 
  beside = TRUE, 
  legend.text = TRUE
)

## ----warning=FALSE, fig.height=6, fig.width=7---------------------------------
barplot(
  table(sjlabelled::as_label(efc$e42dep),
        sjlabelled::as_label(efc$e16sex)), 
  beside = TRUE, 
  legend.text = TRUE,
  main = get_label(efc$e42dep)
)

## -----------------------------------------------------------------------------
efc.sub <- subset(efc, subset = e16sex == 1, select = c(4:8))
str(efc.sub)

## ---- message=FALSE-----------------------------------------------------------
efc.sub <- copy_labels(efc.sub, efc)
str(efc.sub)

