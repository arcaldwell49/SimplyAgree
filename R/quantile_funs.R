


process_rq <- function(rq_obj, se.type = "boot",
                       conf.int = FALSE,
                       conf.level = 0.95) {
  nn <- c("estimate", "std.error", "statistic", "p.value")
  co <- as.data.frame(rq_obj[["coefficients"]])
  if (se.type == "rank") {
    # if only a single predictor is included, confidence interval not calculated
    # set to NA to preserve dimensions of object
    if (1 == ncol(co)) {
      co <- cbind(co, NA, NA)
    }
    co <- setNames(co, c("estimate", "conf.low", "conf.high"))
    conf.int <- FALSE
  } else {
    co <- setNames(co, nn)
  }
  if (conf.int) {
    a <- (1 - conf.level) / 2
    cv <- qt(c(a, 1 - a), df = rq_obj[["rdf"]])
    co[["conf.low"]] <- co[["estimate"]] + (cv[1] * co[["std.error"]])
    co[["conf.high"]] <- co[["estimate"]] + (cv[2] * co[["std.error"]])
  }
  co[["tau"]] <- rq_obj[["tau"]]
  as.data.frame(co)
}


get_qemm = function(quan_mod,
                    df,
                    agree.l,
                    agree.u,
                    conf.level){
  ref_med = ref_grid(quan_mod, se = "boot",
                     tau = .5,
                     at = list(mean = seq(min(df$mean, na.rm =TRUE),
                                          max(df$mean, na.rm= TRUE),
                                          length.out=100)))
  quan_emm_med = as.data.frame(confint(emmeans(ref_med,
                                               ~ mean), level = conf.level))
  df_coef_med = data.frame(at = quan_emm_med$mean,
                           estimate = quan_emm_med$emmean,
                           lower.ci = quan_emm_med$lower.CL,
                           upper.ci = quan_emm_med$upper.CL,
                           text = "Bias")

  ref_lloa = ref_grid(quan_mod, se = "boot",
                      tau = agree.u,
                      at = list(mean = seq(min(df$mean, na.rm =TRUE),
                                           max(df$mean, na.rm= TRUE),
                                           length.out=100)))
  quan_emm_lloa = as.data.frame(confint(emmeans(ref_lloa,
                                                ~ mean), level = conf.level))
  df_coef_lloa = data.frame(at = quan_emm_lloa$mean,
                            estimate = quan_emm_lloa$emmean,
                            lower.ci = quan_emm_lloa$lower.CL,
                            upper.ci = quan_emm_lloa$upper.CL,
                            text = "Lower LoA")

  ref_uloa = ref_grid(quan_mod, se = "boot",
                      tau = agree.l,
                      at = list(mean = seq(min(df$mean, na.rm =TRUE),
                                           max(df$mean, na.rm= TRUE),
                                           length.out=100)))
  quan_emm_uloa = as.data.frame(confint(emmeans(ref_uloa,
                                                ~ mean), level = conf.level))
  df_coef_uloa = data.frame(at = quan_emm_uloa$mean,
                            estimate = quan_emm_uloa$emmean,
                            lower.ci = quan_emm_uloa$lower.CL,
                            upper.ci = quan_emm_uloa$upper.CL,
                            text = "Upper LoA")

  df_emm = rbind(df_coef_uloa,
                 df_coef_med,
                 df_coef_lloa)
  df_emm$text = factor(df_emm$text,
                       levels = c("Upper LoA", "Bias", "Lower LoA"))
  return(df_emm)
}



