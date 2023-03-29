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
    if (is.null(nlme::getGroups(vrSt)))
      stop("Groups should be present for varIdent", call. = FALSE)
    ans <- numeric(nrow(newdata))
    grp.nm <- as.character(nlme::getGroupsFormula(vrSt)[[2]])
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
