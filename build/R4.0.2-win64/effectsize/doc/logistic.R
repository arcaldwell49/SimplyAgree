## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)

pkgs <- c("effectsize", "bayestestR", "ggplot2", "see", "parameters", "modelbased")
if (!all(sapply(pkgs, requireNamespace))) {
  knitr::opts_chunk$set(eval = FALSE)
}

## ----message=FALSE, warning=FALSE---------------------------------------------
data <- bayestestR::simulate_difference(n = 100,
                                        d = 1,
                                        names = c("Group", "Outcome"))

summary(data)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(ggplot2)

ggplot(data, aes(x=Group, y=Outcome, fill=Group)) +
  geom_boxplot() +
  see::theme_modern()

ttest <- t.test(Outcome ~ Group, data=data, var.equal=TRUE)
ttest_pars <- parameters::parameters(ttest)
ttest_pars

## ----message=FALSE, warning=FALSE---------------------------------------------
sd(data$Outcome)

## ----message=FALSE, warning=FALSE---------------------------------------------
effectsize::cohens_d(data$Outcome, data$Group)

## ----message=FALSE, warning=FALSE---------------------------------------------
effectsize::t_to_d(ttest_pars$t, df_error = ttest_pars$df)

## ----message=FALSE, warning=FALSE---------------------------------------------
model <- glm(Group ~ Outcome, data = data,
             family = "binomial")

parameters::parameters(model)

## ----message=FALSE, warning=FALSE---------------------------------------------
data_grid <- modelbased::estimate_link(model)

ggplot(data_grid, aes(x = Outcome, y = Predicted)) +
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high), alpha = 0.2) +
  geom_line(color = "red", size = 1) + 
  see::theme_modern()

## ----message=FALSE, warning=FALSE---------------------------------------------

ggplot(data, aes(x=Group, y=Outcome, fill=Group)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) + 
  # add vertical regression line
  geom_line(data = data_grid, 
            aes(x = Predicted + 1, y = Outcome, fill = NA), 
            color = "red", size = 1) +
  see::theme_modern()

