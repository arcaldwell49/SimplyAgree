#' Calculate the Tolerance Limits from a Agreement Study
#' @description A function for calculating tolerance limits for the difference between two measurements. Approximately the same procedure as Bland-Altman limits of agreement.
#' @param x Name of column with first measurement
#' @param y Name of other column with the other measurement to compare to the first.
#' @param id Column with subject identifier. Default is "id" if no entry is provided.
#' @param condition Column name indicating different conditions subjects were tested under. This can be left missing if there are no differing conditions to be tested.
#' @param time A column naming/numbering the time point. Only necessary if the data is from time series collection.
#' @param data Data frame with all data.
#' @param pred_level The prediction level. Default is 95\%.
#' @param tol_level The tolerance level related to the prediction level. Default is 95\%.
#' @param prop_bias Logical indicator (TRUE/FALSE) of whether proportional bias should be considered for the prediction/tolerance limits calculations.
#' @param log_tf Calculate limits of agreement using log-transformed data.
#' @param cor_type The type of correlation structure. "sym" is for Compound Symmetry, "car1" is for continuous autocorrelation structure of order 1, or "ar1" for autocorrelation structure of order 1.
#' @param correlation an optional corStruct object describing the within-group correlation structure that overrides the default setting. See the documentation of corClasses for a description of the available corStruct classes. If a grouping variable is to be used, it must be specified in the form argument to the corStruct constructor. Defaults to NULL
#' @param weights an optional varFunc object or one-sided formula describing the within-group heteroscedasticity structure that overrides the default setting. If given as a formula, it is used as the argument to varFixed, corresponding to fixed variance weights. See the documentation on varClasses for a description of the available varFunc classes.
#' @param keep_model Logical indicator to retain the GLS model. Useful when working with large datasets and the model is very large.
#'
#' @return Returns single tolerance_delta class object with the results of the agreement analysis with prediction/tolerance limits..
#'
#' \describe{
#'   \item{\code{"limits"}}{A data frame containing the prediction/tolerance limits.}
#'   \item{\code{"model"}}{The GLS model; NULL if keep_model set to FALSE.}
#'   \item{\code{"call"}}{The matched call.}
#' }

#' @examples
#' data('reps')
#'
#' @section References:
#'
#' @importFrom nlme gls  corCompSymm corAR1 corCAR1 varIdent
#' @importFrom dplyr inner_join join_by
#' @export

tolerance_delta_gls = function(data,
                               x,
                               y,
                               id = NULL,
                               condition = NULL,
                               time = NULL,
                               pred_level = 0.95,
                               tol_level = 0.95,
                               tol_method = c("approx","perc"),
                               prop_bias = FALSE,
                               log_tf = FALSE,
                               cor_type = c("sym", "car1", "ar1", "none"),
                               correlation = NULL,
                               weights = NULL,
                               keep_model = TRUE){
  alpha = 1 - tol_level
  alpha.pred=1-pred_level
  # match args -----
  cor_type = match.arg(cor_type)
  tol_method = match.arg(tol_method)
  # set call ----
  call2 = match.call()
  call2$id = id
  call2$condition = condition
  call2$pred_level = pred_level
  call2$tol_level = tol_level
  call2$prop_bias = prop_bias
  call2$log_tf = log_tf
  call2$cor_type = cor_type
  call2$correlation = correlation
  call2$weights = weights

  # organize data -----
  temp_frame = data[c(x,y,id,condition,time)]
  names(temp_frame)[names(temp_frame) == x] <- "x"
  names(temp_frame)[names(temp_frame) == y] <- "y"
  names(temp_frame)[names(temp_frame) == id] <- "id"
  names(temp_frame)[names(temp_frame) == condition] <- "condition"
  names(temp_frame)[names(temp_frame) == time] <- "time"
  #colnames(temp_frame) = c(x,y,id,condition,time)

  temp_frame = na.omit(temp_frame)
  temp_frame$avg = (temp_frame$x + temp_frame$y)/2
  if(log_tf){
    temp_frame$x = log(temp_frame$x)
    temp_frame$y = log(temp_frame$y)
  }
  temp_frame$delta = temp_frame$x - temp_frame$y
  avg_vals = c(min(temp_frame$delta),
               median(temp_frame$delta),
               max(temp_frame$delta))
  if(!("id" %in% colnames(temp_frame))){
    temp_frame$id = 1:nrow(temp_frame)
  }
  deg_of_freedom = length(unique(temp_frame$id)) -1
  # MODEL ----
  model = gls(delta ~ 1, data = temp_frame)

  if(!is.null(condition)){
    model = update(model,
                   . ~ . + condition,
                   weights = varIdent(form=~1|condition))

  }

  if(prop_bias){
    model = update(model,
                   . ~ . + avg)
  }

  if(!is.null(id) && cor_type != "none"){

    if(!is.null(time)){
      cor1 = switch(cor_type,
                    sym = corCompSymm(form = ~time|id),
                    car1 = corCAR1(form = ~time|id),
                    ar1 = corAR1(form= ~time|id))
    } else {
      cor1 = switch(cor_type,
                    sym = corCompSymm(form = ~1|id),
                    car1 = corCAR1(form = ~1|id),
                    ar1 = corAR1(form= ~1|id))
    }

    model = update(model,
                   correlation = cor1)
  }

  if(!is.null(weights)){
    model = update(model,
           weights = weights)
  }

  if(!is.null(correlation)){
    model = update(model,
           correlation = correlation)
  }

  # EMMEANS ----

  ## Ref Grid then Marginal Means ----
  # Need to have condition and/or avg in ref grid
  # otherwise just 1
  res_emm = gls_emm_delta(model = model,
                          temp_frame = temp_frame,
                          avg_vals = avg_vals)

  if(tol_method == "approx"){
    emm_df = as.data.frame(res_emm) %>%
      rename(SEM = SE) %>%
      mutate(SEP = sqrt(sigma(model)^2+SEM^2)) %>%
      mutate(lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
             upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP,
             lower.TL = emmean - qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df)),
             upper.TL = emmean + qnorm(1-alpha.pred/2) * SEP * sqrt(df/qchisq(alpha,df))) %>%
      rename(bias = emmean)
  } else {
    emm_df = as.data.frame(res_emm) %>%
      rename(SEM = SE) %>%
      mutate(SEP = sqrt(sigma(model)^2+SEM^2)) %>%
      mutate(lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
             upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP)
    res_sum_df = boot_delta_gls(model = model,
                                temp_frame = temp_frame,
                                avg_vals = avg_vals,
                                res_emm = res_emm,
                                tol_level = tol_level,
                                alpha.pred = alpha.pred,
                                replicates = replicates)

    emm_df = full_join(emm_df, res_sum_df) %>%
      rename(bias = emmean)
  }


  # Save model -----
  model <- if(keep_model){
    model
  } else{
    NULL
  }

  if(!("condition" %in% colnames(emm_df))){
    emm_df$condition = NA
  }

  if(!("avg" %in% colnames(emm_df))){
    emm_df$avg = NA
  }

  if(!("condition" %in% colnames(temp_frame))){
    temp_frame$condition = 1:nrow(temp_frame)
  }

  if(!("id" %in% colnames(temp_frame))){
    temp_frame$id = 1:nrow(temp_frame)
  }

  if(!("time" %in% colnames(temp_frame))){
    temp_frame$time = 1:nrow(temp_frame)
  }

  df = as.data.frame(temp_frame)
  lm_mod = list(call = list(formula = as.formula(df$y ~ df$x + df$id + df$avg + df$delta + df$condition + df$time)))
  call2$lm_mod = lm_mod

  res = structure(list(limits = emm_df,
             emmeans = res_emm,
             model = model,
             call = call2),
             class = "tolerance_delta")

  return(res)
}


# get emmeans -----

gls_emm_delta = function(model,
                         temp_frame,
                         avg_vals){
  if("avg" %in% paste0(nlme::getCovariateFormula(model))){
    if("condition" %in% paste0(nlme::getCovariateFormula(model))){

      res_emm = emmeans(ref_grid(model,
                                 at = list(avg = avg_vals)),
                        ~ condition + avg,
                        mode = "satterthwaite")
    } else{
      res_emm = emmeans(ref_grid(model,
                                 at = list(avg = avg_vals)),
                        ~ avg,
                        mode = "satterthwaite")
    }

  } else {
    if("condition" %in% paste0(nlme::getCovariateFormula(model))){
      res_emm = emmeans(model,
                        ~ condition ,
                        mode = "satterthwaite")
    } else{
      res_emm = emmeans(model,
                        ~ 1 ,
                        mode = "satterthwaite")
    }
  }
  return(res_emm)
}

# bootstrap ----
boot_delta_gls = function(model,
                          temp_frame,
                          avg_vals,
                          res_emm,
                          tol_level,
                          alpha.pred,
                          replicates){
  emm_df = as.data.frame(res_emm) %>%
    rename(SEM = SE) %>%
    mutate(SEP = sqrt(sigma(model)^2+SEM^2)) %>%
    mutate(lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
           upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP)

  res_df = data.frame()

  for(i in 1:replicates){
    dat2 = r_gen(dat = temp_frame,
                 mle = model)
    res_i = update(model, data = dat2)

    emm1 = gls_emm_delta(model = res_i,
                         temp_frame = dat2,
                         avg_vals = avg_vals)
    emm_df1 = as.data.frame(emm1) %>%
      rename(SEM = SE) %>%
      mutate(SEP = sqrt(sigma(res_i)^2+SEM^2)) %>%
      mutate(lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
             upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP)
    class(emm_df1) = "data.frame"
    emm_df1$boot_n = i

    res_df = rbind(res_df,emm_df1)
  }

  if("avg" %in% paste0(nlme::getCovariateFormula(model))){
    if("condition" %in% paste0(nlme::getCovariateFormula(model))){
      sum_res_df = res_df %>%
        group_by(avg, condition) %>%
        summarize(
          lower.TL = quantile(lower.PL, 1 - tol_level),
          upper.TL = quantile(upper.PL, tol_level)
        ) %>%
        inner_join(emm_df,
                   .,
                   by = join_by(condition, avg))
    } else{
      sum_res_df = res_df %>%
        group_by(avg) %>%
        summarize(
          lower.TL = quantile(lower.PL, 1 - tol_level),
          upper.TL = quantile(upper.PL, tol_level)
        ) %>%
        inner_join(emm_df,
                   .,
                   by = join_by(avg))
    }

  } else {
    if("condition" %in% paste0(nlme::getCovariateFormula(model))){
      sum_res_df = res_df %>%
        group_by(condition) %>%
        summarize(
          lower.TL = quantile(lower.PL, 1 - tol_level),
          upper.TL = quantile(upper.PL, tol_level)
        ) %>%
        inner_join(emm_df,
                   .,
                   by = join_by(condition))
    } else{
      sum_res_df = res_df %>%
        summarize(
          lower.TL = quantile(lower.PL, 1 - tol_level),
          upper.TL = quantile(upper.PL, tol_level)
        ) %>% cbind(emm_df, .)
    }
  }

  return(sum_res_df)

}

r_gen <- function(dat, mle) {
  out   <- dat
  out$delta <- sim_gls(mle)
  return(out)
}

# Adapted from nlraa below -----
sim_gls = function (object, psim = 2, na.action = na.fail, naPattern = NULL,
                    data = NULL, ...)
{
  if (!inherits(object, "gls"))
    stop("This function is only for 'gls' objects")
  args <- list(...)
  if (!is.null(args$newdata)) {
      stop("At this point 'newdata' is not compatible.",
           call. = FALSE)
  }
  else {
    if (is.null(data)) {
      newdata <- try(nlme::getData(object), silent = TRUE)
      if (inherits(newdata, "try-error") || is.null(newdata))
        stop("'data' argument is required. It is likely you are using sim_gls inside another function")
    }
    else {
      if (object$dims$N != nrow(data)) {
        stop("Number of rows in data argument does not match the original data \n\n The data argument should only be used to pass the same data.frame \n \n  used to fit the model",
             call. = FALSE)
      }
      newdata <- data
    }
  }
  form <- nlme::getCovariateFormula(object)
  mfArgs <- list(formula = form, data = newdata, na.action = na.action)
  mfArgs$drop.unused.levels <- TRUE
  dataMod <- do.call(model.frame, mfArgs)
  contr <- object$contrasts
  for (i in names(dataMod)) {
    if (inherits(dataMod[, i], "factor") && !is.null(contr[[i]])) {
      levs <- levels(dataMod[, i])
      levsC <- dimnames(contr[[i]])[[1]]
      if (any(wch <- is.na(match(levs, levsC)))) {
        stop(sprintf(ngettext(sum(wch), "level %s not allowed for %s",
                              "levels %s not allowed for %s"), paste(levs[wch],
                                                                     collapse = ",")), domain = NA)
      }
      attr(dataMod[, i], "contrasts") <- contr[[i]][levs,
                                                    , drop = FALSE]
    }
  }
  N <- nrow(dataMod)
  if (length(all.vars(form)) > 0) {
    X <- model.matrix(form, dataMod)
  }
  else {
    X <- array(1, c(N, 1), list(row.names(dataMod), "(Intercept)"))
  }
  if (psim == 0) {
    cf <- coef(object)
  }
  if (psim == 1) {
    cf <- MASS::mvrnorm(n = 1, mu = coef(object), Sigma = vcov(object))
  }
  if (psim == 2) {
    cf <- MASS::mvrnorm(n = 1, mu = coef(object), Sigma = vcov(object))
    if (is.null(object$modelStruct$corStruct)) {
      if (is.null(args$newdata) || is.null(object$modelStruct$varStruct)) {
        rsds.std <- stats::rnorm(N, 0, 1)
        rsds <- rsds.std * attr(residuals(object), "std")
      }
      else {
        rsds.std <- stats::rnorm(nrow(newdata), 0, 1)
        rsds <- rsds.std * predict_varFunc(object, newdata = newdata)
      }
    }
    else {
      var.cov.err <- var_cov(object, sparse = TRUE, data = newdata)
      chol.var.cov.err <- Matrix::chol(var.cov.err)
      rsds <- chol.var.cov.err %*% rnorm(nrow(chol.var.cov.err))
    }
  }
  val <- c(X[, names(cf), drop = FALSE] %*% cf)
  if (psim == 2)
    val <- as.vector(val + rsds)
  lab <- "Predicted values"
  if (!is.null(aux <- attr(object, "units")$y)) {
    lab <- paste(lab, aux)
  }
  structure(val, label = lab)
}


predict_varFunc = function (object, newdata)
{
  fttd <- predict(object, newdata = newdata)
  if (is.null(object$modelStruct$varStruct))
    stop("varStruct should not be null for this function",
         call. = TRUE)
  stds <- sigma(object)/nlme::varWeights(object$modelStruct$varStruct)
  vrSt <- object$modelStruct$varStruct
  if (!inherits(vrSt, c("varIdent", "varFixed", "varExp",
                        "varPower"))) {
    stop("Only varIdent, varFixed, varExp, varPower classes are supported at this time",
         call. = FALSE)
  }
  if (inherits(vrSt, "varFixed")) {
    ans <- sigma(object) * sqrt(newdata[[as.character(formula(vrSt)[2])]])
  }
  if (inherits(vrSt, "varIdent")) {
    if (is.null(getGroups(vrSt)))
      stop("Groups should be present for varIdent", call. = FALSE)
    ans <- numeric(nrow(newdata))
    grp.nm <- as.character(getGroupsFormula(vrSt)[[2]])
    if (!grp.nm %in% names(newdata))
      stop("Grouping factor should be present in 'newdata' object",
           call. = FALSE)
    if (grepl("*", grp.nm, fixed = TRUE))
      stop("This is not supported yet. Please submit this as an issue to github if you need it.")
    for (i in 1:nrow(newdata)) {
      crr.grp <- newdata[i, grp.nm]
      wch.grp.nm <- which(names(nlme::varWeights(vrSt)) == crr.grp)[1]
      ans[i] <- sigma(object) * (1/nlme::varWeights(vrSt))[wch.grp.nm]
    }
  }
  if (inherits(vrSt, "varExp")) {
    var_exp_fun <- function(x, t) exp(2 * t * x)
    if (is.null(getGroups(vrSt))) {
      if (any(grepl("fitted", as.character(formula(vrSt))))) {
        cvrt <- fttd
      }
      else {
        cvrt.nm <- as.character(nlme::getCovariateFormula(vrSt))[2]
        if (!grepl(cvrt.nm, names(newdata)))
          stop("Variance covariate should be present in 'newdata' object",
               call. = FALSE)
        cvrt <- newdata[[cvrt.nm]]
      }
      ans <- sigma(object) * sqrt(var_exp_fun(cvrt, coef(vrSt)))
    }
    else {
      ans <- numeric(nrow(newdata))
      grp.nm <- as.character(nlme::getGroupsFormula(vrSt)[[2]])
      if (!grp.nm %in% names(newdata))
        stop("Grouping factor should be present in 'newdata' object",
             call. = FALSE)
      if (grepl("*", grp.nm, fixed = TRUE))
        stop("This is not supported yet. Please submit this as an issue to github if you need it.")
      for (i in unique(newdata[[grp.nm]])) {
        wch.crr.grp <- which(newdata[[grp.nm]] == i)
        grp.coef <- coef(vrSt)[which(attr(vrSt, "groupNames") ==
                                       i)]
        if (any(grepl("fitted", as.character(formula(vrSt))))) {
          cvrt <- fttd[wch.crr.grp]
        }
        else {
          cvrt <- newdata[wch.crr.grp, as.character(nlme::getCovariateFormula(vrSt))[[2]]]
        }
        ans[wch.crr.grp] <- sigma(object) * sqrt(var_exp_fun(cvrt,
                                                             grp.coef))
      }
    }
  }
  if (inherits(vrSt, "varPower")) {
    var_power_fun <- function(x, delta) abs(x)^(2 * delta)
    if (is.null(nlme::getGroups(vrSt))) {
      if (any(grepl("fitted", as.character(formula(vrSt))))) {
        cvrt <- fttd
      }
      else {
        cvrt.nm <- as.character(nlme::getCovariateFormula(vrSt))[2]
        if (!grepl(cvrt.nm, names(newdata)))
          stop("Variance covariate should be present in 'newdata' object",
               call. = FALSE)
        cvrt <- newdata[[cvrt.nm]]
      }
      ans <- sigma(object) * sqrt(var_power_fun(cvrt,
                                                coef(vrSt)))
    }
    else {
      ans <- numeric(nrow(newdata))
      grp.nm <- as.character(nlme::getGroupsFormula(vrSt)[[2]])
      if (!grp.nm %in% names(newdata))
        stop("Grouping factor should be present in 'newdata' object",
             call. = FALSE)
      if (grepl("*", grp.nm, fixed = TRUE))
        stop("This is not supported yet. Please submit this as an issue to github if you need it.",
             call. = FALSE)
      for (i in unique(newdata[[grp.nm]])) {
        wch.crr.grp <- which(newdata[[grp.nm]] == i)
        grp.coef <- coef(vrSt)[which(attr(vrSt, "groupNames") ==
                                       i)]
        if (any(grepl("fitted", as.character(formula(vrSt))))) {
          cvrt <- fttd[wch.crr.grp]
        }
        else {
          cvrt <- newdata[wch.crr.grp, as.character(nlme::getCovariateFormula(vrSt))[[2]]]
        }
        ans[wch.crr.grp] <- sigma(object) * sqrt(var_power_fun(cvrt,
                                                               grp.coef))
      }
    }
  }
  ans <- c(as.vector(ans))
  return(ans)
}

var_cov = function (object, type = c("residual", "random", "all", "conditional",
                                      "marginal"), aug = FALSE, sparse = FALSE, data = NULL)
{
  type <- match.arg(type)
  if (type == "conditional")
    type <- "residual"
  if (type == "marginal")
    type <- "all"
  if (type == "random" && inherits(object, c("lm", "nls",
                                             "gls")))
    stop("The variance-covariance of the random effects is only available for \n\n          objects which inherit class 'lme' ")
  if (isTRUE(sparse)) {
    if (!requireNamespace("Matrix", quietly = TRUE)) {
      warning("Matrix package is required for this option")
      return(NULL)
    }
  }
  if (inherits(object, c("lm", "nls"))) {
    ans <- diag(nrow = length(fitted(object))) * sigma(object)^2
    if (sparse)
      ans <- Matrix::Matrix(ans, sparse = TRUE)
  }
  if (inherits(object, c("gls", "lme"))) {
    if (type == "residual") {
      ans <- var_cov_lme_resid(object, sparse = sparse,
                               data = data)
    }
    if (type == "random") {
      ans <- var_cov_lme_ranef(object, aug = aug, sparse = sparse,
                               data = data)
    }
    if (type == "all") {
      ans <- var_cov_lme_resid(object, sparse = sparse,
                               data = data) + var_cov_lme_ranef(object, aug = TRUE,
                                                                sparse = sparse, data = data)
    }
  }
  return(ans)
}


var_cov_lme_resid = function (object, sparse = FALSE, data = data)
{
  if (!inherits(object, c("gls", "lme")))
    stop("Only for objects which inherit the 'gls' or 'lme' class")
  if (inherits(object, "gls")) {
    sgms <- attr(residuals(object), "std")
  }
  else {
    sgms <- attr(object[["residuals"]], "std")
  }
  if (is.null(object$modelStruct$corStruct)) {
    Lambda <- diag(sgms^2)
  }
  else {
    if (is.null(object$groups)) {
      Lambda <- t(sgms * nlme::corMatrix(object$modelStruct$corStruct)) *
        sgms
    }
    else {
      corrMat <- nlme::corMatrix(object$modelStruct$corStruct)
      if (is.list(corrMat)) {
        grp.nm <- deparse(nlme::getGroupsFormula(object$modelStruct$corStruct)[[2]])
        if (is.null(data)) {
          gdat <- nlme::getData(object)
        }
        else {
          gdat <- data
        }
        ogrpo <- unique(gdat[[grp.nm]])
        corrMat <- corrMat[ogrpo]
      }
      Lambda <- t(sgms * as.matrix(Matrix::bdiag(corrMat))) *
        sgms
    }
  }
  if (sparse) {
    Lambda <- Matrix::Matrix(Lambda, sparse = TRUE)
  }
  return(Lambda)
}

var_cov_lme_ranef = function (object, aug = FALSE, sparse = FALSE, data = NULL)
{
  if (!inherits(object, c("gls", "lme")))
    stop("Only for objects which inherit the 'gls' or 'lme' class")
  if (inherits(object, "nlme") && aug)
    stop("Don't know how to augment nlme random effects.")
  lreg <- length(names(object$modelStruct$reStruct))
  if (lreg == 1L && aug == FALSE) {
    ans <- as.matrix(object$modelStruct$reStruct[[1]]) *
      sigma(object)^2
  }
  else {
    if (!inherits(object, "nlme")) {
      V <- mgcv::extract.lme.cov(object, data = data)
      ans <- V - var_cov_lme_resid(object, data = data)
    }
    else {
      ans <- vector("list", lreg)
      names(ans) <- names(object$modelStruct$reStruct)
      for (i in 1:lreg) {
        tm <- as.matrix(object$modelStruct$reStruct[[i]]) *
          sigma(object)^2
        if (sparse)
          tm <- Matrix::Matrix(tm, sparse = TRUE)
        ans[[i]] <- tm
      }
    }
  }
  if (sparse && !is.list(ans))
    ans <- Matrix::Matrix(ans, sparse = TRUE)
  return(ans)
}
