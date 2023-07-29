
df_osurg = read.table(here::here("mess","orthosurg.txt"),
                      header = FALSE)
#colnames(df_osurg) = c("S", "T", "P", "Sl", "ESM", "S_2", "T_2", "P_2", "Sl_2", "ESM_2")

df_osurg1 = df_osurg[1:5]
df_osurg2 = df_osurg[6:10]
colnames(df_osurg2) = c("V1","V2","V3","V4","V5")

df_osurgf = rbind(df_osurg1,df_osurg2)
colnames(df_osurgf) = c("S", "T", "P", "Sl", "ESM")
df_osurgf$slice = as.factor(df_osurgf$Sl)
df_osurgf$surgeon = as.factor(df_osurgf$S)
df_osurg_1 = subset(df_osurgf, P == 1)
df_osurg_2 = subset(df_osurgf, P == 2)
mod_surg_1 = gls(ESM ~ slice,
               correlation = corAR1(form=~1|surgeon),
               data = df_osurg_1)
mod_surg_2 = gls(ESM ~ slice,
                 correlation = corCompSymm(form=~1|surgeon),
                 data = df_osurg_2)
lm_surg_1 = lmerTest::lmer(ESM ~ slice +(1|surgeon),
                 data = df_osurg_1)
lm_surg_2 = lmerTest::lmer(ESM ~ slice +(1|surgeon),
                           data = df_osurg_2)
# Method in tolerance_delta_gls matches that included in the text
emmeans(mod_surg_1,
        ~ slice) %>%
  as.data.frame() %>%
  rename(SEM = SE) %>%
  mutate(SEP = sqrt(sigma(mod_surg_1)^2+SEM^2)) %>%
  mutate(lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
         upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP,
         lower.TL = emmean - qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df)),
         upper.TL = emmean + qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df)))
# Assay Validation Study, data from Hoffman and Kringle (2005)
# Estimate Mixed Model and calculate the different statistical intervals
# Bernard G FRANCQ
#rm(list = ls())
#library(varComp) # Archived R package
run = rep(1:6, each = 3)
rep = rep(1:3, length.out = 18)
y = c(0.969, 0.976, 0.938, 0.952, 0.993, 0.956, 0.989, 0.883, 0.981, 1, 0.969, 0.954, 0.959, 0.989, 1.02,
      1.02, 1.09, 1.02)
dat = data.frame(run = run, rep = rep, y = y)
dat$run = as.factor(dat$run)

library(nlme)

mod1 = gls(y ~ 1,
           correlation = corCompSymm(form=~1|run),
           data = dat)

mod1 = lmerTest::lmer(y ~ (1|run),
                data = dat)
VarCorr(mod1)
library(tidyverse)
library(emmeans)
alpha = .05
alpha.pred = .05
emmeans(mod1,
        ~ 1) %>%
  as.data.frame() %>%
  rename(SEM = SE) %>%
  mutate(SEP = sqrt(sigma(mod1)^2+SEM^2)) %>%
  mutate(lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
         upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP,
         lower.TL = emmean - qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df)),
         upper.TL = emmean + qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df))) %>%
  rename(bias = emmean)
