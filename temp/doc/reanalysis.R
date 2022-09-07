## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE,warning=FALSE---------------------------------------
library(SimplyAgree)
library(tidyverse)
data("temps")
df_temps = temps

## ---- fig.cap="Example of the Line of Identity"-------------------------------
qplot(1,1) + geom_abline(intercept = 0, slope = 1)

## ----pltsrec,fig.cap="Concordance Plots of Rectal Temperature",echo=FALSE,fig.width=8.5,fig.height=5----
df_rec.delta = df_temps %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,trec_delta,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = trec_delta) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

df_rec.post = df_temps %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,trec_post,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = trec_post) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

p_rec.delta <- ggplot(df_rec.delta, aes(x=AM, y=PM)) +
  stat_smooth(aes(color=trial_condition), 
              method = "lm",
              formula = 'y ~ x',
              fullrange = TRUE,
              geom='line', alpha=0.5, se=FALSE)+
  scale_x_continuous("AM - Trec (delta)",
                     limits = c(.15,1.1))+
  scale_y_continuous("PM - Trec (delta)",
                     limits = c(.15,1.1))+
  geom_abline(intercept = 0, slope = 1, size = .85,
              alpha = .75) +
  geom_point(aes(color=trial_condition),
             alpha = .75) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_viridis_d()

p_rec.post <- ggplot(df_rec.post, aes(x=AM, y=PM)) +
  stat_smooth(aes(color=trial_condition), 
              method = "lm",
              formula = 'y ~ x',
              fullrange = TRUE,
              geom='line', alpha=0.75, se=FALSE)+
  scale_x_continuous("AM - Trec (post)",
                     limits = c(36.4,38.5))+
  scale_y_continuous("PM - Trec (post)",
                     limits = c(36.4,38.5))+
  geom_abline(intercept = 0, slope = 1, size = .85,
              alpha = .75) +
  geom_point(aes(color=trial_condition),
             alpha = .75) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_viridis_d()
p_rec.post
p_rec.delta

## ----pltseso,fig.cap="Concordance Plots of Esophageal Temperature",echo=FALSE,fig.width=8.5,fig.height=5----
df_eso.delta = df_temps %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,teso_delta,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = teso_delta) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

df_eso.post = df_temps %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,teso_post,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = teso_post) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

p_eso.delta <- ggplot(df_eso.delta, aes(x=AM, y=PM)) +
  stat_smooth(aes(color=trial_condition), 
              method = "lm",
              formula = 'y ~ x',
              fullrange = TRUE,
              geom='line', alpha=0.5, se=FALSE)+
  scale_x_continuous("AM - Teso (delta)",
                     limits = c(.15,1.1))+
  scale_y_continuous("PM - Teso (delta)",
                     limits = c(.15,1.1))+
  geom_abline(intercept = 0, slope = 1, size = .85,
              alpha = .75) +
  geom_point(aes(color=trial_condition),
             alpha = .75) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_viridis_d()

p_eso.post <- ggplot(df_eso.post, aes(x=AM, y=PM)) +
  stat_smooth(aes(color=trial_condition), 
              method = "lm",
              formula = 'y ~ x',
              fullrange = TRUE,
              geom='line', alpha=0.75, se=FALSE)+
  scale_x_continuous("AM - Teso (post)",
                     limits = c(36.4,38.5))+
  scale_y_continuous("PM - Teso (post)",
                     limits = c(36.4,38.5))+
  geom_abline(intercept = 0, slope = 1, size = .85,
              alpha = .75) +
  geom_point(aes(color=trial_condition),
             alpha = .75) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_viridis_d()

p_eso.post

p_eso.delta

## ---- echo=FALSE--------------------------------------------------------------
rec.post_loa = readr::read_rds("rec_post_loa.rds")
rec.delta_loa = readr::read_rds("rec_delta_loa.rds")

eso.post_loa = readr::read_rds("eso_post_loa.rds")
eso.delta_loa = readr::read_rds("eso_delta_loa.rds")


## ---- eval=FALSE--------------------------------------------------------------
#  rec.post_loa = SimplyAgree::loa_lme(diff = "diff",
#                                      condition = "trial_condition",
#                                      id = "id",
#                                      avg = "Average",
#                                      data = df_rec.post,
#                                      conf.level = .95,
#                                      agree.level = .95,
#                                      replicates = 199,
#                                      type = "perc")

## -----------------------------------------------------------------------------
knitr::kable(rec.post_loa$loa,
             caption = "LoA: Trec Post Exercise")

## ---- fig.cap="Limits of Agreement for Trec Post Exercise",fig.width=7,fig.height=5----
plot(rec.post_loa)

## -----------------------------------------------------------------------------
knitr::kable(rec.delta_loa$loa,
             caption = "LoA: Delta Trec")

## ---- fig.cap="Limits of Agreement for Delta Trec",fig.width=7,fig.height=5----
plot(rec.delta_loa)

## ---- eval=FALSE--------------------------------------------------------------
#  eso.post_loa = SimplyAgree::loa_mixed(diff = "diff",
#                                       condition = "trial_condition",
#                                       id = "id",
#                                       data = df_eso.post,
#                                       conf.level = .95,
#                                       agree.level = .95,
#                                       replicates = 199,
#                                       type = "bca")

## -----------------------------------------------------------------------------
knitr::kable(eso.post_loa$loa,
             caption = "LoA: Teso Post Exercise")

## ---- fig.cap="Limits of Agreement for Teso Post Exercise",fig.width=7,fig.height=5----
plot(eso.post_loa)

## -----------------------------------------------------------------------------
knitr::kable(eso.delta_loa$loa,
             caption = "LoA: Delta Teso")

## ---- fig.cap="Limits of Agreement for Delta Teso",fig.width=7,fig.height=5----
plot(eso.delta_loa)

