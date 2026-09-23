# Parametric simulation from a fitted gls model ----
# Simulates new responses from the point estimates of the fitted model:
# fitted values plus residual errors with the model's marginal covariance
# (variance function and within-group correlation). The Cholesky factors of
# the within-group covariance blocks are computed once in gls_sim_setup() so
# each draw in gls_sim_draw() is cheap.

gls_sim_setup = function(model, data){
  if (!inherits(model, "gls"))
    stop("This function is only for 'gls' objects")
  N = model$dims$N
  if (nrow(data) != N)
    stop("Number of rows in data does not match the data used to fit the model")

  mu = as.vector(fitted(model))
  # residual SD for each row (sigma scaled by the variance function)
  sds = as.vector(attr(residuals(model), "std"))
  cs = model$modelStruct$corStruct

  if (is.null(cs)) {
    return(list(mu = mu, sds = sds, blocks = NULL))
  }

  cor_mat = nlme::corMatrix(cs)
  if (!is.list(cor_mat)) {
    # correlation without a grouping factor: one block for all rows
    rows = list(seq_len(N))
    cor_mat = list(cor_mat)
  } else {
    # corMatrix blocks are named by group; rows within a group are in data
    # order (gls sorts by group with a stable sort)
    grp = nlme::getGroups(data, nlme::getGroupsFormula(cs))
    rows = split(seq_len(N), grp)[names(cor_mat)]
  }

  blocks = mapply(function(i, R) {
    S = sds[i] * t(sds[i] * R)
    list(rows = i, L = t(chol(S)))
  }, rows, cor_mat, SIMPLIFY = FALSE)

  list(mu = mu, sds = sds, blocks = blocks)
}

gls_sim_draw = function(setup){
  if (is.null(setup$blocks)) {
    return(setup$mu + stats::rnorm(length(setup$mu)) * setup$sds)
  }
  err = numeric(length(setup$mu))
  for (b in setup$blocks) {
    err[b$rows] = b$L %*% stats::rnorm(length(b$rows))
  }
  setup$mu + err
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
        if (!cvrt.nm %in% names(newdata))
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
        if (!cvrt.nm %in% names(newdata))
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
