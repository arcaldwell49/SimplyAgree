para_boot1 <- function(model, B){
  ystar <- simulateY(model, nsim = B)
  row.names(ystar) <- 1:model$dims$N
  ystar <- data.frame(ystar)
  return(ystar)
}
simulateY <- function (object, nsim = 1, seed = as.integer(runif(1, 0, .Machine$integer.max)), ...,
                       verbose = FALSE, sigma)
{
  # Data with one level of grouping only.
  .functionName <- "simulateY.lme"
  .traceR <- if (is.null(options()$traceR)) function(...){} else options()$.traceR

  .traceR(1, lbl = "-> simulateY.lme STARTS")

  if (verbose) print(paste("nsim = ", nsim ,", seed = ", seed, sep = ""))

  if (!inherits(object, "lme"))  stop("Object must inherit from class \"lme\" ")

  .traceR(110, lbl = "missing(sigma)")

  if (!missing(sigma))  object$sigma <- sigma
  .traceR(115, lbl = "after !missing(sigma) ")

  groups <- object$groups[[1]]
  .traceR(120, lbl = "groups")

  ugroups <- unique(groups)
  individuals <- as.matrix(ugroups)
  if (is.numeric(individuals))
    individuals <- ugroups[individuals]
  .traceR(130, lbl = "individuals")
  Vlist <- nlme::getVarCov(object, individuals, type="marginal")
  fitd0 <- fitted(object, level = 0)
  chVlist <- lapply(Vlist, chol)

  nx <- nsim * length(fitd0)

  set.seed(seed)
  noise.mtx <- matrix(rnorm(nx), nrow = length(fitd0), ncol = nsim)

  .traceR(150, lbl = "lapply STARTS here ***")
  dimN   <-   sapply(chVlist, ncol)  # Number of records per subject
  cdimN1 <- cumsum(dimN)
  cdimN0 <- cumsum(dimN) - dimN + 1
  cdimN  <- cbind(cdimN0, cdimN1)
  tList <- vector(length(dimN), mode = "list")
  tList[] <- 1:length(dimN)
  auxF1 <-
    function(el){ # 1,2, ...
      .traceR(1001, lbl = paste("Local auxF1() STARTS. el =", el) , store = FALSE)

      chV <- chVlist[[el]]
      ix <- cdimN[el,]
      i0 <- ix[1]
      i1 <- ix[2]
      noisex <- noise.mtx[i0:i1, ]
      tx <-t(chV) %*% noisex   # Check transpose here
      .traceR(1002, lbl = paste("el=", el))
      .traceR(1001,lbl = paste("Local auxF1() ENDS. el =", el) , store = FALSE)
      tx
    }
  res <- lapply(tList, FUN= auxF1)
  .traceR(150, lbl = "lapply ENDS here ***")

  .traceR(160, lbl = "resAll STARTS***", store = FALSE)
  resAll <- NULL
  for (i in 1:length(dimN)) resAll <- rbind(resAll, res[[i]])
  .traceR(160, lbl = "resAll ENDS***")
  .traceR(1, lbl = "simulateY.lme ENDS->")
  return(resAll + fitd0)
}

factory = function (fun, debug = FALSE, errval = "An error occurred in the factory function",
          types = c("message", "warning", "error"))
{
  function(...) {
    errorOccurred <- FALSE
    warn <- err <- msg <- NULL
    res <- withCallingHandlers(tryCatch(fun(...), error = function(e) {
      if (debug)
        cat("error: ", conditionMessage(e), "\n")
      err <<- conditionMessage(e)
      errorOccurred <<- TRUE
      NULL
    }), warning = function(w) {
      if (!"warning" %in% types) {
        warning(conditionMessage(w))
      }
      else {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    }, message = function(m) {
      if (debug)
        cat("message: ", conditionMessage(m), "\n")
      if (!"message" %in% types) {
        message(conditionMessage(m))
      }
      else {
        msg <<- append(msg, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    })
    if (errorOccurred) {
      if (!"error" %in% types)
        stop(err)
      res <- errval
    }
    setattr <- function(x, attrib, value) {
      attr(x, attrib) <- value
      x
    }
    attr_fun <- function(x, str, msg) {
      setattr(x, paste0("factory-", str), if (is.character(msg))
        msg
        else NULL)
    }
    res <- attr_fun(res, "message", msg)
    res <- attr_fun(res, "warning", warn)
    res <- attr_fun(res, "error", err)
    return(res)
  }
}

update_mod = function (model, new.y = NULL, new.data = NULL,
                       formula1 = formula1)
{
  mod.fixd <- formula1
  mod.rand <- model$call$random
  mod.data <- model$data
  mod.weights <- model$call$weights
  if (!is.null(model$na.action) && model$na.action == 8) {
    mod.data <- na.omit(mod.data)
  }

  if (is.null(new.data)) {
    mod.data[, as.character(mod.fixd[[2]])] <- unname(new.y)
  }
  ctrl <- nlme::lmeControl(opt = "optim", returnObject = TRUE)
  if (is.null(mod.rand)) {
    f1 <- factory(function(mod.fixd, mod.data, ctrl) do.call("lme",
                                                             args = list(fixed = mod.fixd,
                                                                         data = mod.data,
                                                                         control = ctrl,
                                                                         weights = mod.weights)))
    out.lme <- f1(mod.fixd, mod.data, ctrl)
  }
  else {
    mod.rand <- as.formula(mod.rand)
    f1 <- factory(function(mod.fixd, mod.data, mod.rand,
                           ctrl) do.call("lme", args = list(fixed = mod.fixd,
                                                            data = mod.data, random = mod.rand,
                                                            control = ctrl,
                                                            weights = mod.weights)))
    out.lme <- f1(mod.fixd, mod.data, mod.rand, ctrl)
  }
  out.lme
}

para_boot2 = function(model, specs1, at_list = NULL, prop_bias = FALSE,
                      agree.lim){
  #at_list2 = list(avg = at_list)
  var_comp1 = model$modelStruct$varStruct %>%
    coef(unconstrained = FALSE, allCoef = TRUE) %>%
    data.frame("structure" = .) %>%
    #rownames_to_column(var = "condition") %>%
    mutate(condition = rownames(.)) %>%
    mutate(sigma = model$sigma) %>%
    mutate(sd_within = sigma * structure) %>%
    mutate(sd_between = as.numeric(VarCorr(model)[1,2])) %>%
    mutate(sd_total = sqrt(sd_within^2 + sd_between^2)) %>%
    select(condition, structure, sd_within, sd_between, sd_total)
  rownames(var_comp1) = 1:nrow(var_comp1)
  if(prop_bias == TRUE) {
    at_list2 = list(avg = at_list)
    ref = ref_grid(model,
                   specs = "condition",
                   at = at_list2,
                   cov.reduce = FALSE,
                   cov.keep = "avg")
    emm_tab = emmeans(ref, ~ condition | avg,
                      at = at_list) %>%
      as.data.frame()
    colnames(emm_tab) = c("condition", "avg", "mean", "se", "df", "lower.CL", "upper.CL")
    #emm_tab$avg = avg_vals
    emm_tab = emm_tab %>%
      select(avg, condition, mean) %>%
      merge(var_comp1) %>%
      mutate(
        low = mean - agree.lim * sd_total,
        high = mean + agree.lim * sd_total)
  } else {
    emm_tab = emmeans(model,
                      specs=specs1) %>%
      as.data.frame()
    colnames(emm_tab) = c("condition", "mean", "se", "df", "lower.CL", "upper.CL")

    emm_tab = emm_tab %>%
      select(condition, mean) %>%
      merge(var_comp1) %>%
      mutate(low = mean - agree.lim * sd_total,
             high = mean + agree.lim * sd_total)
  }
  return(emm_tab)
}
