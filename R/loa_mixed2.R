loa_lmer = function(diff,
                    avg,
                    condition = NULL,
                    id,
                    data,
                    conf.level = .95,
                    agree.level = .95,
                    replicates = 1999,
                    prop_bias = FALSE){
  if (conf.level >= 1 || conf.level <= 0) {
    stop("conf.level must be a value between 0 and 1")
  }
  if(is.null(condition) || missing(condition)){
    condition = 1
  }

  if (agree.level >= 1 || agree.level <= 0) {
    stop("agree.level must be a value between 0 and 1")
  }

  if(is.null(condition)){
    df = data[c(diff,avg,id)]
    colnames(df) = c("diff", "avg", "id")
  } else {
    df = data[c(diff,avg,id,condition)]
    colnames(df) = c("diff", "avg", "id", "condition")
  }

  avg_vals = c(min(df$avg, na.rm = TRUE),
               median(df$avg, na.rm = TRUE),
               max(df$avg, na.rm = TRUE))

  if(is.null(condition)){
    if (prop_bias == FALSE) {
      formula1 = as.formula("diff ~ 1 +(1| id )")
      newdat = expand.grid(1) %>%
        as.data.frame() %>%
        rename(condition = Var1)
    } else {
      formula1 = as.formula("diff~avg+(1| id )")
      newdat = expand.grid(avg_vals) %>%
        as.data.frame() %>%
        rename(avg = Var1)
    }
  } else{
    if (prop_bias == FALSE) {
      formula1 = as.formula("diff ~ condition +(1| id )")
      newdat = expand.grid(unique(df$condition)) %>%
        as.data.frame() %>%
        rename(condition = Var1)
    } else {
      formula1 = as.formula("diff~avg+condition+(1| id )")
      newdat = expand.grid(avg_vals, unique(df$condition)) %>%
        as.data.frame() %>%
        rename(condition = Var2,
               avg = Var1)
      newdat2 = expand.grid(avg_vals, unique(df$condition),c("Bias", "Lower LoA", "Upper LoA")) %>%
        as.data.frame() %>%
        rename(condition = Var2,
               avg = Var1,
               value = Var3) %>%
        select(value, avg, condition)
    }
  }

  res_lmer = lmer(
    formula = formula1,
    data = df,
    weights = NULL,
    subset = NULL,
    offset = NULL,
    na.action = na.omit
  )

  boot_sd <- function(.) {
    c(sd_within = sigma(.),
      sd_between = sqrt(unlist(VarCorr(.))),
      sd_total = sigma(.) + sqrt(unlist(VarCorr(.))))
  }

  boot_loa <- function(.) {
    c(bias = pred_bias(.,newdata = newdat),
      lower = pred_lloa(.,newdata = newdat, agree.level = agree.level),
      upper = pred_uloa(.,newdata = newdat, agree.level = agree.level))
  }

  boo1 <- bootMer(res_lmer, boot_sd, nsim = replicates,
                  type = "parametric", use.u = FALSE)
  boo2 <- bootMer(res_lmer, boot_loa, nsim = replicates,
                  type = "parametric", use.u = FALSE)
  boo2_tab = tidy_boot(boo2,
                       conf.int = TRUE,
                       conf.level = conf.level,
                       conf.method = type) %>%
    bind_cols(newdat2 ,.) %>%
    select(-term) %>%
    rename(se = std.error,
           lower.ci = conf.low,
           upper.ci = conf.high) %>%
    as_tibble()
  df_loa = res_tab[1:3,]
  var_comp = res_tab[4:6,]

  mc = match.call()

  mc$agree.level = agree.level
  mc$conf.level = conf.level
  if (condition != 1) {
    df_plt = data %>%
      select(all_of(diff),
             all_of(id),
             all_of(condition),
             all_of(avg)) %>%
      rename(
        diff = all_of(diff),
        id = all_of(id),
        condition = all_of(condition),
        avg = all_of(avg)
      )
  } else{
    df_plt = data %>%
      select(all_of(diff),
             all_of(id),
             all_of(avg)) %>%
      rename(
        diff = all_of(diff),
        id = all_of(id),
        avg = all_of(avg)
      )
  }
  if(condition == 1){
    lm_mod = list(call = list(formula = as.formula(df_plt$diff~df_plt$avg+df_plt$id)))
  } else {
    lm_mod = list(call = list(formula = as.formula(df_plt$diff~df_plt$avg+df_plt$condition+df_plt$id)))
  }

  mc$lm_mod = lm_mod

  structure(list(loa = df_loa,
                 var_comp = var_comp,
                 h0_test = rej_text,
                 lmer = res_lmer,
                 agree.level = agree.level,
                 conf.level = conf.level,
                 type = type,
                 call = mc),
            class = "loa_mixed_bs")
}
