## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)

pkgs <- c("effectsize")
if (!all(sapply(pkgs, requireNamespace))) {
  knitr::opts_chunk$set(eval = FALSE)
}

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(effectsize)

convert_d_to_r(d = 1)

