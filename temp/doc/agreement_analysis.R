## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(SimplyAgree)

## -----------------------------------------------------------------------------
a1 = agree_test(x = reps$x,
                y = reps$y,
                agree.level = .8)

## -----------------------------------------------------------------------------
print(a1)

## ---- fig.width=6, fig.height=6-----------------------------------------------
plot(a1, type = 1)

plot(a1, type = 2) 

## -----------------------------------------------------------------------------
a2 = agree_reps(x = "x",
                y = "y",
                id = "id",
                data = reps,
                agree.level = .8)

## -----------------------------------------------------------------------------
print(a2)

## ---- fig.width=6, fig.height=6-----------------------------------------------
plot(a2, type = 1)

plot(a2, type = 2)

## -----------------------------------------------------------------------------
a3 = agree_nest(x = "x",
                y = "y",
                id = "id",
                data = reps,
                agree.level = .8)

## -----------------------------------------------------------------------------
print(a3)

## ---- fig.width=6, fig.height=6-----------------------------------------------
plot(a3, type = 1)

plot(a3, type = 2)

## -----------------------------------------------------------------------------
a1 = agree_test(x = reps$x,
                y = reps$y,
                agree.level = .8)

check(a1)

## -----------------------------------------------------------------------------
a1 = agree_test(x = reps$x,
                y = reps$y,
                prop_bias = TRUE,
                agree.level = .8)
print(a1)

plot(a1)

## -----------------------------------------------------------------------------
a1 = agree_np(x = "x",
              y = "y",
              data = reps,
              delta = 2,
              prop_bias = FALSE,
              agree.level = .8)
print(a1)

plot(a1)

## -----------------------------------------------------------------------------
a1 = agree_np(x = "x",
              y = "y",
              data = reps,
              delta = 2,
              prop_bias = TRUE,
              agree.level = .8)
print(a1)

plot(a1)

## -----------------------------------------------------------------------------
set.seed(81346)
x = rnorm(750, 100, 10)
diff = rnorm(750, 0, 1)
y = x + diff

df = data.frame(x = x,
                y = y)

a1 = agree_test(x = df$x,
                y = df$y,
                agree.level = .95)

plot(a1,
     geom = "geom_point")

plot(a1,
     geom = "geom_bin2d")

plot(a1,
     geom = "geom_density_2d")

plot(a1,
     geom = "geom_density_2d_filled")

plot(a1,
     geom = "stat_density_2d")

## ----warning=FALSE,eval=FALSE-------------------------------------------------
#  
#  recpre_long$avg = (recpre_long$PM + recpre_long$PM)/2
#  a4 = loa_mixed(data = recpre_long,
#                 diff = "diff",
#                 avg = "avg",
#                 #condition = "trial_condition",
#                 id = "id",
#                 #plot.xaxis = "AM",
#                 replicates = 199,
#                 type = "perc")

## -----------------------------------------------------------------------------
power_res <- blandPowerCurve(
  samplesizes = seq(10, 100, 1),
  mu = 0.5,
  SD = 2.5,
  delta = c(6,7),
  conf.level = c(.90,.95),
  agree.level = c(.8,.9)
)

head(power_res)

## -----------------------------------------------------------------------------
find_n(power_res, power = .8)

## ----fig.width=6, fig.height=6------------------------------------------------
plot(power_res)

