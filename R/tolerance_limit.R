#' @title Tolerance Limits from an Agreement Study
#'
#' @description `r lifecycle::badge('maturing')`
#'
#' A function for calculating tolerance limits for the difference between two measurements (difference = x-y).
#' This is a procedure that should produce results similar to the Bland-Altman limits of agreement.
#' See vignettes for more details.
#'
#' @param data A data frame containing the variables.
#' @param x Name of the column for the first measurement.
#' @param y Name of the column for the second measurement.
#' @param id Name of the column for the subject ID. With `model = "lme"`, two column names can be given for nested random intercepts, outer level first (e.g., `c("golfer", "club")` for clubs within golfers); see "Model assumptions" in details.
#' @param condition Name of the column indicating different conditions subjects were tested under. This can be left missing if there are no differing conditions to be tested. Supplying a condition adds a separate residual variance for each condition (`nlme::varIdent`); see "Model assumptions" in details.
#' @param time Name of the column indicating the time points. Only necessary if the data is from time series or repeated measures collection.
#' @param pred_level Prediction level for the prediction interval, which is also the content (the proportion of differences to be covered, beta) for the tolerance limits. Default is 95%.
#' @param tol_level Confidence level (gamma) for the tolerance limits. Default is 95%. See `bound_type` for what this confidence refers to.
#' @param conf_level Confidence level for the confidence interval of the bias (`lower.CL`/`upper.CL`). Default is 95%. This does not affect the prediction or tolerance limits. For a two one-sided tests (TOST) procedure on the bias at level alpha, use `conf_level = 1 - 2 * alpha` (e.g., 0.90).
#' @param tol_method Method for calculating the tolerance limits. Options are "analytic" (default) for closed-form limits and "boot_cal" (experimental) for the closed-form limits calibrated by a parametric bootstrap. The analytic limits are recommended; see details. Both target the interval set by `bound_type`. The previous names, "approx" and "perc", are deprecated aliases for "analytic" and "boot_cal".
#' @param bound_type Which claim the tolerance limits (`lower.TL`/`upper.TL`) support. "joint" (default) gives a beta-content, gamma-confidence tolerance interval: with confidence `tol_level`, at least `pred_level` of all differences lie within the limits. "iu" gives equal-tailed bounds: a one-sided `tol_level` confidence bound on each of the (1 - `pred_level`)/2 and (1 + `pred_level`)/2 percentiles of the differences. The "iu" bounds are intended for an intersection-union test of agreement against a maximal allowable difference and are not a joint `tol_level` interval. See details.
#' @param prop_bias Whether to include a proportional bias term in the model. Determines whether proportional bias should be considered for the prediction/tolerance limits calculations. Note that a slope of the differences on the average can appear without any true proportional bias when the two methods have unequal measurement error variances; see "Model assumptions" in details.
#' @param log_tf Calculate limits of agreement using log-transformed data.
#' @param log_tf_display The type of presentation for log-transformed results. The differences between methods can be displayed as a "ratio" or "sympercent".
#' @param model The type of model for the differences. "gls" (default) is a marginal generalized least squares model ([nlme::gls()]) with the correlation structure set by `cor_type`. "lme" is a linear mixed model ([nlme::lme()]) with a random intercept for each `id`, which requires `id`; with two `id` columns it has nested random intercepts (outer level, and inner level within outer). With compound symmetry and no variance function both give the same fit; "lme" differs when serial correlation (`cor_type = "ar1"` or `"car1"`) or a variance function (`condition` or `weights`) is added, because it keeps a persistent subject effect and applies the variance function to the residuals only. See "Model assumptions" in details.
#' @param cor_type The type of correlation structure. "sym" is for Compound Symmetry, "car1" is for continuous autocorrelation structure of order 1, or "ar1" for autocorrelation structure of order 1. The autoregressive options ("ar1" and "car1") have no persistent subject effect; see "Model assumptions" in details. With `model = "lme"`, "sym" fits the random intercept alone, "ar1" and "car1" add serial correlation of the residuals within `id` on top of the random intercept, and "none" is not allowed.
#' @param correlation an optional corStruct object describing the within-group correlation structure that overrides the default setting. See the documentation of corClasses for a description of the available corStruct classes. If a grouping variable is to be used, it must be specified in the form argument to the corStruct constructor. Defaults to NULL. With `model = "lme"`, this is the correlation of the residuals within the innermost level of `id` (on top of the random intercepts) and must be grouped by it using the internal names: `id` for one level (e.g., `nlme::corAR1(form = ~ time | id)`) or `id/id_2` for two (e.g., `nlme::corAR1(form = ~ time | id/id_2)`).
#' @param weights an optional varFunc object or one-sided formula describing the within-group heteroskedasticity structure that overrides the default setting. If given as a formula, it is used as the argument to varFixed, corresponding to fixed variance weights. See the documentation on varClasses for a description of the available varFunc classes. Variance covariates must use the internal column names (`avg`, `condition`, `x`, `y`, `time`) or `fitted(.)`. Currently `varIdent`, `varFixed`, `varExp`, and `varPower` are supported. If the variance depends on `avg`, the limits are reported at the minimum, median, and maximum of `avg` even when `prop_bias = FALSE`. With `model = "lme"`, the variance function applies to the residuals only; the random-intercept variance is common to all observations.
#' @param keep_model Logical indicator to retain the fitted model (`gls` or `lme`). Useful when working with large data and the model is very large.
#' @inheritParams loa_lme
#' @details The tolerance limits calculated in this function are based on the papers by Francq & Govaerts (2016), Francq, et al. (2019), and Francq, et al. (2020). The formulas, including how the degrees of freedom are determined for clustered data, are given in `vignette("agreement_analysis", package = "SimplyAgree")`.
#'
#' The output contains three kinds of interval, which support different claims:
#'
#'   - **Prediction interval** (`lower.PL`/`upper.PL`): a beta-expectation tolerance interval. It is expected to contain a single future difference, from a new subject, with probability `pred_level`, averaged over repeated studies. It is not a confidence statement: any single prediction interval may cover less than `pred_level` of the differences.
#'   - **Tolerance limits, `bound_type = "joint"`** (`lower.TL`/`upper.TL`): a beta-content, gamma-confidence tolerance interval. With confidence `tol_level`, at least `pred_level` of all differences lie within the limits. If the limits lie within a maximal allowable difference of plus or minus delta, one can conclude (at level 1 - `tol_level`) that at least `pred_level` of the differences lie within plus or minus delta (Lin, 2000). The tails do not need to be split equally.
#'   - **Tolerance limits, `bound_type = "iu"`**: a one-sided `tol_level` bound on the lower and on the upper (1 -/+ `pred_level`)/2 percentile of the differences, i.e., on the limits of agreement. If both bounds lie within plus or minus delta, one can reject, at level 1 - `tol_level`, that either limit of agreement lies outside plus or minus delta (intersection-union test). Both bounds hold together only about 1 - 2 * (1 - `tol_level`) of the time, so they should not be reported as a joint `tol_level` interval. These target the same quantities as the default confidence bounds from [agreement_limit()] (numerically close but not identical, because the models differ).
#'
#' The "joint" and "iu" limits test different hypotheses (coverage versus each tail), so they do not nest: the "iu" bounds can be wider than the "joint" limits, and a skewed distribution can pass one test and fail the other.
#'
#' With `tol_method = "analytic"`:
#'
#'   - "joint" uses the approximation of Howe (1969) as described by Francq et al. (2020), with the standard error of prediction (SEP) in place of the sample standard deviation. For independent data this matches Howe's approximation exactly.
#'   - "iu" uses the exact noncentral t bound for independent data, and otherwise the MOVER bound of Zou (2013).
#'   - Both use a one-sided upper confidence bound for the residual standard deviation (SD). With independent data this is the usual chi-square bound. With compound symmetry (`cor_type = "sym"`), the between- and within-subject variance components are combined with the MOVER, as in [agreement_limit()] with `data_type = "nest"`. With other correlation structures, the bound uses an effective degrees of freedom (`SD.df`) from the approximate covariance of the variance parameters.
#'
#' The analytic limits are exact for independent data (without a variance function). In simulations (95% target), they were close to nominal with compound symmetry (about 0.95, with 10 to 20 subjects) and with AR(1) correlation (about 0.95). With `condition` (a separate residual variance per condition) combined with compound symmetry, the variance components for each condition are bounded using that condition's cluster sizes and degrees of freedom; coverage was then about 0.96. Other variance functions (`weights`) use the cluster sizes of the whole data set and have not been checked by simulation.
#'
#' With `tol_method = "boot_cal"`, the analytic limits are calibrated by a parametric bootstrap (Loh, 1987; Beran, 1987). New data are simulated from the fitted model (Francq et al., 2019), the model is refit to each replicate, and the analytic limits are computed for each replicate over a range of nominal confidence levels. The level is chosen at which the replicates achieve the target confidence for the fitted model:
#'
#'   - "joint": a proportion `tol_level` of the replicate intervals contain at least `pred_level` of the fitted distribution of the differences.
#'   - "iu": separately for each bound, a proportion `tol_level` of the replicate bounds lie beyond the fitted limit of agreement.
#'
#' The limits for the observed data are the analytic limits at the calibrated levels, which are returned as `lower.TL.level` and `upper.TL.level`. `tol_method = "boot_cal"` is **experimental**. The calibration is judged against the fitted model, so it treats the estimated correlation as known. With few subjects this adds its own error: in simulations with 10 to 15 subjects (95% target), coverage ranged from about 0.93 (compound symmetry, with or without `condition`) to about 0.96 (AR(1) correlation), while the analytic limits were 0.95 to 0.965 in the same settings. The analytic limits are therefore recommended; the bootstrap may be useful as a check for models whose analytic limits have not been evaluated (e.g., other variance functions or `prop_bias = TRUE`).
#'
#' For clustered data (`id` supplied), all limits refer to a new difference from a new subject: the SD includes both the between- and within-subject variance.
#'
#' ## Model assumptions
#'
#' With `model = "gls"` (default), the model is a marginal (generalized least squares) model fit with [nlme::gls()]. For the default setup (`cor_type = "sym"` without a variance function), compound symmetry with a non-negative correlation gives the same likelihood as a random-intercept model, and targets the marginal distribution of a single difference from a randomly chosen subject. With `model = "lme"`, the model is a linear mixed model fit with [nlme::lme()] with a random intercept for each `id`; with compound symmetry and no variance function it gives the same fit and limits. With `model = "lme"`, the degrees of freedom for the bias are the containment degrees of freedom (subjects - 1 for the models fit here), because the Satterthwaite approximation for `lme` models in emmeans can fail. The other options carry assumptions that should be checked:
#'
#'   - **Autoregressive correlation** (`cor_type = "ar1"` or `"car1"`): with `model = "gls"`, the correlation between two measurements from the same subject decays towards zero as they get further apart in time. A persistent subject-specific bias (e.g., a subject by method interaction) instead makes all measurements from that subject equally correlated. When such an effect exists, the autoregressive structures miss most of the long-range correlation, so the standard error of the bias is too small and the limits are too narrow; the bootstrap does not correct this, because it simulates from the same model. In one simulation (20 subjects with 19 measurements each, and a random subject effect), the 95% confidence interval for the bias covered the true value 74% of the time with AR(1), versus 96% with compound symmetry. With `model = "lme"`, the serial correlation is added to the residuals on top of a random intercept, which keeps the persistent subject effect. In simulations with a random subject effect and AR(1) residuals (15 subjects, 8 measurements each; 95% target), coverage of the joint tolerance limits was about 0.89 with `model = "gls", cor_type = "ar1"` and about 0.95 with `model = "lme", cor_type = "ar1"`. Use the autoregressive options with `model = "gls"` only when no persistent subject effect is expected. The fits can be compared with `AIC()` on the returned models (a REML comparison is valid because the fixed effects are the same).
#'   - **Variance functions with compound symmetry** (`condition`, or `weights` together with `cor_type = "sym"`): in `gls`, the correlation applies to the standardized residuals, so the covariance between two measurements from the same subject is rho * sigma_i * sigma_j. The between-subject variance therefore scales with the variance function (e.g., it is larger in a condition with a larger residual SD), rather than being common to all conditions. The marginal variance of each condition, which drives the limits, is not directly affected, but the standard error of each condition's bias and the reported variance components are. With `model = "lme"`, the variance function applies to the residuals only, and the between-subject variance is common to all conditions. In simulations where that was true (12 subjects; 95% target), the joint tolerance limits had coverage of about 0.96 with `model = "gls"` and about 0.97 with `model = "lme"`.
#'   - **Clustering levels**: `model = "gls"` has a single grouping factor (`id`). With `model = "lme"`, `id` can name two nested levels (e.g., `id = c("golfer", "club")` for shots with several clubs per golfer), giving random intercepts for the outer level and for the inner level within the outer level. Setting `id` to the inner level alone (e.g., a golfer-by-club identifier) should be avoided: it drops the correlation across inner levels within the same outer level, which understates the uncertainty. In simulations (15 subjects with 2 to 4 settings each; 95% target), the joint tolerance limits had coverage of about 0.88 to 0.91 with the inner level as `id`, versus about 0.96 with nested random intercepts. Grouping by the outer level only (`id = "golfer"`) also gave close to nominal coverage (about 0.96), because the inner-level variance is then absorbed into the residual; the nested model additionally reports the inner-level variance component (`SD.nested`). More than two levels are not supported.
#'   - **Variance components**: the SD of a single difference splits into between-subject (`SD.between`), inner-level (`SD.nested`, with two `id` columns), and within-subject (`SD.within`) components. With `model = "gls"` and compound symmetry these are sqrt(rho) * SD and sqrt(1 - rho) * SD; with `model = "lme"`, `SD.between` is the random-intercept SD and `SD.within` the residual SD. They can be used, for example, for the point estimate of limits for the mean of m measurements per subject, bias +/- z * sqrt(SD.between^2 + SD.within^2 / m) (without residual correlation), or for the between-subject floor of agreement. They are point estimates only.
#'   - **Proportional bias** (`prop_bias = TRUE`): a non-zero slope of the differences on the average can appear without any true proportional bias. Whenever the two methods have unequal measurement error variances, cov(difference, average) = (var(x) - var(y)) / 2 is not zero (Bland & Altman, 1999), so the slope should not be read as proportional bias on its own. Errors-in-variables methods such as [dem_reg()] or [pb_reg()] are better suited to assessing proportional bias.
#'
#' @return Returns single `tolerance_delta` class object with the results of the agreement analysis with a prediction interval and tolerance limits.
#'
#'   - `limits`: A data frame containing the prediction/tolerance limits. Columns include `bias` (estimated mean difference), `SEM` (standard error of the bias), `SD` (residual standard deviation of a single difference at that row, from the variance function if one is in the model), `SEP` (standard error of prediction, `sqrt(SD^2 + SEM^2)`), `SD.df` (degrees of freedom of the residual variance), `SD.upper` (one-sided `tol_level` upper confidence bound for SD), `SD.between`/`SD.nested`/`SD.within` (between-subject, nested (inner level), and within-subject components of SD, under compound symmetry or with `model = "lme"`; `NA` otherwise, and `SD.nested` is `NA` without nesting), `lower.CL`/`upper.CL` (confidence limits for the bias), `lower.PL`/`upper.PL` (prediction limits), `lower.TL`/`upper.TL` (tolerance limits), and, for `tol_method = "boot_cal"`, `lower.TL.level`/`upper.TL.level` (the calibrated nominal levels at which the analytic limits were computed).
#'   - `model`: The fitted `gls` or `lme` model; NULL if keep_model set to FALSE. The data (with the internal column names `x`, `y`, `delta`, `avg`, `id` (the row number if not supplied), and, if supplied, `condition` and `time`), correlation structure, and variance function are stored with the model, so `update()` and `nlme::getData()` can be used on it directly.
#'   - `call`: The matched call.
#' @examples
#' data('reps')
#'
#' # Simple
#' tolerance_limit(x = "x", y ="y", data = reps)
#'
#' # Nested
#' tolerance_limit(x = "x", y ="y", data = reps, id = "id")
#'
#' # Nested, random intercept (mixed) model
#' tolerance_limit(x = "x", y ="y", data = reps, id = "id", model = "lme")
#'
#' # Nested random intercepts (e.g., measurements within settings within subjects)
#' \donttest{
#' reps2 = reps
#' reps2$setting = rep(1:2, length.out = nrow(reps2))
#' tolerance_limit(x = "x", y = "y", data = reps2, id = c("id", "setting"),
#'                 model = "lme")
#' }
#'
#' @references
#'
#' Francq, B. G., & Govaerts, B. (2016). How to regress and predict in a Bland–Altman plot? Review and contribution based on tolerance intervals and correlated‐errors‐in‐variables models. Statistics in mMdicine, 35(14), 2328-2358.
#'
#' Francq, B. G., Lin, D., & Hoyer, W. (2019). Confidence, prediction, and tolerance in linear mixed models. Statistics in Medicine, 38(30), 5603-5622.
#'
#' Francq, B. G., Berger, M., & Boachie, C. (2020). To tolerate or to agree: A tutorial on tolerance intervals in method comparison studies with BivRegBLS R Package. Statistics in Medicine, 39(28), 4334-4349.
#'
#' Howe, W. G. (1969). Two-sided tolerance limits for normal populations—some improvements. Journal of the American Statistical Association, 64(326), 610-620.
#'
#' Lin, L. I. (2000). Total deviation index for measuring individual agreement with applications in laboratory performance and bioequivalence. Statistics in Medicine, 19(2), 255-270.
#'
#' Loh, W. Y. (1987). Calibrating confidence coefficients. Journal of the American Statistical Association, 82(397), 155-162.
#'
#' Beran, R. (1987). Prepivoting to reduce level error of confidence sets. Biometrika, 74(3), 457-468.
#'
#' Zou, G. Y. (2013). Confidence interval estimation for the Bland–Altman limits of agreement with multiple observations per individual. Statistical Methods in Medical Research, 22(6), 630-642.
#'
#' @importFrom nlme gls  corCompSymm corAR1 corCAR1 varIdent
#' @importFrom stats vcov model.matrix formula na.fail update
#' @importFrom emmeans ref_grid
#' @importFrom dplyr inner_join join_by
#' @export

tolerance_limit = function(data,
                               x,
                               y,
                               id = NULL,
                               condition = NULL,
                               time = NULL,
                               pred_level = 0.95,
                               tol_level = 0.95,
                               conf_level = 0.95,
                               tol_method = c("analytic", "boot_cal"),
                               bound_type = c("joint", "iu"),
                               prop_bias = FALSE,
                               log_tf = FALSE,
                           log_tf_display = c("ratio", "sympercent"),
                               model = c("gls", "lme"),
                               cor_type = c("sym", "car1", "ar1", "none"),
                               correlation = NULL,
                               weights = NULL,
                               keep_model = TRUE,
                               replicates = 999){
  alpha = 1 - tol_level
  alpha.pred=1-pred_level
  # match args -----
  model_type = match.arg(model)
  cor_type = match.arg(cor_type)
  tol_method = tol_method_arg(tol_method)
  bound_type = match.arg(bound_type)
  log_tf_display = match.arg(log_tf_display)
  # id: one column, or two columns (outer, inner) for nested random intercepts
  if(length(id) > 2){
    stop("`id` can have at most two columns (outer level first, e.g., ",
         "c(\"subject\", \"setting\")).", call. = FALSE)
  }
  if(anyDuplicated(id)){
    stop("The columns in `id` must be different.", call. = FALSE)
  }
  nested = length(id) == 2
  if(nested && model_type == "gls"){
    stop("Nested grouping (two `id` columns) requires model = \"lme\".",
         call. = FALSE)
  }
  if(model_type == "lme"){
    if(is.null(id)){
      stop("model = \"lme\" requires `id`: the random intercept is for each level of `id`.",
           call. = FALSE)
    }
    if(cor_type == "none"){
      stop("With model = \"lme\", the random intercept already correlates ",
           "measurements within `id`; use cor_type = \"sym\" for the random ",
           "intercept alone, or \"ar1\"/\"car1\" to add serial correlation.",
           call. = FALSE)
    }
    # residual correlation must be grouped by the innermost random effect
    cor_groups = if(nested) c("id", "id_2") else "id"
    if(!is.null(correlation) &&
       !identical(all.vars(nlme::getGroupsFormula(correlation)), cor_groups)){
      stop("With model = \"lme\", `correlation` must be grouped by the ",
           "innermost level of `id`, using the internal names (e.g., ",
           if(nested) "nlme::corAR1(form = ~ time | id/id_2)" else
             "nlme::corAR1(form = ~ time | id)",
           ").", call. = FALSE)
    }
  }
  # set call ----
  call2 = match.call()
  call2$model = model_type
  call2$id = id
  call2$condition = condition
  call2$pred_level = pred_level
  call2$tol_level = tol_level
  call2$conf_level = conf_level
  call2$tol_method = tol_method
  call2$bound_type = bound_type
  call2$prop_bias = prop_bias
  call2$log_tf = log_tf
  call2$cor_type = cor_type
  call2$correlation = correlation
  call2$weights = weights
  call2$log_tf_display = log_tf_display
  # organize data -----
  temp_frame = data[c(x,y,id,condition,time)]
  names(temp_frame)[names(temp_frame) == x] <- "x"
  names(temp_frame)[names(temp_frame) == y] <- "y"
  names(temp_frame)[names(temp_frame) == id[1]] <- "id"
  if(nested){
    names(temp_frame)[names(temp_frame) == id[2]] <- "id_2"
  }
  names(temp_frame)[names(temp_frame) == condition] <- "condition"
  names(temp_frame)[names(temp_frame) == time] <- "time"
  #colnames(temp_frame) = c(x,y,id,condition,time)

  temp_frame = na.omit(temp_frame)
  temp_frame$avg = (temp_frame$x + temp_frame$y)/2
  avg_vals = c(min(temp_frame$avg),
               median(temp_frame$avg),
               max(temp_frame$avg))
  if(log_tf){
    temp_frame$x = log(temp_frame$x)
    temp_frame$y = log(temp_frame$y)
  }
  temp_frame$delta = temp_frame$x - temp_frame$y

  if(!("id" %in% colnames(temp_frame))){
    temp_frame$id = 1:nrow(temp_frame)
  }
  # MODEL ----
  # Set to null for when not used
  var1 = NULL
  cor1 = NULL
  fixed = NULL
  random = NULL

  if(model_type == "lme"){
    ## Random intercept model ----
    # the variance function applies to the residuals only, so the
    # between-subject variance is common to all conditions
    if(!is.null(condition)){
      var1 = varIdent(form = ~1|condition)
    }
    # residual serial correlation within the innermost level, on top of the
    # random intercept(s)
    if(cor_type %in% c("ar1", "car1")){
      cor_form = if(nested){
        if(!is.null(time)) ~time|id/id_2 else ~1|id/id_2
      } else {
        if(!is.null(time)) ~time|id else ~1|id
      }
      cor1 = switch(cor_type,
                    car1 = nlme::corCAR1(form = cor_form),
                    ar1 = nlme::corAR1(form = cor_form))
    }
    if(!is.null(weights)){
      var1 = weights
    }
    if(!is.null(correlation)){
      cor1 = correlation
    }
    fixed = stats::reformulate(c("1",
                                 if(!is.null(condition)) "condition",
                                 if(prop_bias) "avg"),
                               response = "delta")
    random = if(nested) ~ 1 | id/id_2 else ~ 1 | id
    model = nlme::lme(fixed = fixed,
                      random = random,
                      data = temp_frame,
                      weights = var1,
                      correlation = cor1)
  } else {

    model = gls(delta ~ 1, data = temp_frame)
    ## Update model with condition -----
    if(!is.null(condition)){
      var1 = varIdent(form=~1|condition)
      model = update(model,
                     . ~ . + condition,
                     weights = var1)

    }

    ## Update model for prop bias ----
    if(prop_bias){
      model = update(model,
                     . ~ . + avg)
    }

    ## Correlation -----
    if(!is.null(id) && cor_type != "none"){

      if(!is.null(time)){
        cor1 = switch(cor_type,
                      sym = nlme::corCompSymm(form = ~time|id),
                      car1 = nlme::corCAR1(form = ~time|id),
                      ar1 = nlme::corAR1(form= ~time|id))
      } else {
        cor1 = switch(cor_type,
                      sym = nlme::corCompSymm(form = ~1|id),
                      car1 = nlme::corCAR1(form = ~1|id),
                      ar1 = nlme::corAR1(form= ~1|id))
      }

      model = update(model,
                     correlation = cor1)
    }

    ## Custom model input -----

    if(!is.null(weights)){
      var1 = weights
      model = update(model,
             weights = var1)

    }

    if(!is.null(correlation)){
      cor1 = correlation
      model = update(model,
             correlation = cor1)

    }
  }

  model = model_self_contained(model = model,
                               data = temp_frame,
                               cor1 = cor1,
                               var1 = var1,
                               fixed = fixed,
                               random = random)

  # EMMEANS ----

  ## Ref Grid then Marginal Means ----
  # Need to have condition and/or avg in ref grid
  # otherwise just 1

  res_emm = gls_emm_delta(model = model,
                          temp_frame = temp_frame,
                          avg_vals = avg_vals,
                          conf_level = conf_level)

  emm_df = tol_grid(res_emm = res_emm,
                    model = model,
                    avg_vals = avg_vals) %>%
    add_pred_limits(model = model,
                    alpha.pred = alpha.pred)
  # df and one-sided upper confidence bound of the residual SD for each row
  sd_info = sd_bound_info(model = model,
                          grid = emm_df,
                          data = temp_frame)
  emm_df$SD.df = sd_df_at(emm_df$SD, sd_info)
  emm_df$SD.upper = sd_upper_at(emm_df$SD, tol_level, sd_info)
  # variance components: the random-intercept SD for lme, or
  # sigma_b^2 = rho * sigma^2 and sigma_w^2 = (1 - rho) * sigma^2 under
  # compound symmetry
  if(sd_info$type == "comp"){
    emm_df$SD.between = sd_info$between
    emm_df$SD.nested = sd_info$nested
    nested_var = ifelse(is.na(sd_info$nested), 0, sd_info$nested^2)
    emm_df$SD.within = sqrt(emm_df$SD^2 - sd_info$between^2 - nested_var)
  } else {
    emm_df$SD.between = NA_real_
    emm_df$SD.nested = NA_real_
    emm_df$SD.within = NA_real_
  }
  # a random intercept correlates measurements within id even without a
  # corStruct
  independent = !inherits(model, "lme") && is.null(model$modelStruct$corStruct)

  if(tol_method == "analytic"){
    emm_df = tol_approx(emm_df = emm_df,
                        sd_info = sd_info,
                        pred_level = pred_level,
                        tol_level = tol_level,
                        bound_type = bound_type,
                        independent = independent) %>%
      rename(bias = emmean)
  }

  if(tol_method == "boot_cal"){
    boot_df = boot_delta(model = model,
                             temp_frame = temp_frame,
                             avg_vals = avg_vals,
                             res_emm = res_emm,
                             sd_info = sd_info,
                             replicates = replicates)
    emm_df = tol_boot(emm_df = emm_df,
                      boot_df = boot_df,
                      sd_info = sd_info,
                      pred_level = pred_level,
                      tol_level = tol_level,
                      bound_type = bound_type,
                      independent = independent) %>%
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
  lm_mod = plot_frame(y = df$y, x = df$x, id = df$id,
                      avg = df$avg, delta = df$delta,
                      condition = df$condition, time = df$time)
  call2$lm_mod = lm_mod

  res = structure(list(limits = emm_df,
             emmeans = res_emm,
             model = model,
             call = call2),
             class = "tolerance_delta")

  return(res)
}


# tol_method argument ----
# "approx" and "perc" were renamed to "analytic" and "boot_cal" in 0.3.1
# ("perc" was never a percentile bootstrap after the calibration was added).
# The old names, including partial matches, still work with a notice.
tol_method_arg = function(tol_method){
  new = tryCatch(match.arg(tol_method, c("analytic", "boot_cal")),
                 error = function(e) NULL)
  if(!is.null(new)){
    return(new)
  }
  old = match.arg(tol_method, c("approx", "perc"))
  new = switch(old,
               approx = "analytic",
               perc = "boot_cal")
  lifecycle::deprecate_soft(
    "0.3.1",
    I(paste0('tolerance_limit(tol_method = "', old, '")')),
    I(paste0('tolerance_limit(tol_method = "', new, '")'))
  )
  new
}

# get emmeans -----

gls_emm_delta = function(model,
                         temp_frame,
                         avg_vals,
                         conf_level = 0.95){
  # emmeans passes the call's weights (a varFunc) to model.frame when
  # recovering the data, which errors for intercept-only models. The varFunc
  # is not needed from the call (vcov and apVar come from the fitted object),
  # so drop it from this local copy.
  model$call$weights = NULL

  # For lme, emmeans' Satterthwaite df can fail (e.g., intercept only) or not
  # terminate (seen with a varIdent residual variance), so the containment df
  # are used (subjects - 1 for the random intercept models fit here).
  mode = if(inherits(model, "lme")) "containment" else "satterthwaite"
  res_emm = emm_delta_mode(model, temp_frame, avg_vals, mode)
  res_emm = update(res_emm, level = conf_level)
  return(res_emm)
}

emm_delta_mode = function(model,
                          temp_frame,
                          avg_vals,
                          mode){
  covs = paste0(nlme::getCovariateFormula(model))[2]

  if(grepl("avg", covs)){
    if(grepl("condition", covs)){
      res_emm = emmeans(ref_grid(model,
                                 at = list(avg = avg_vals),
                                 data = temp_frame),
                        ~ condition + avg,
                        mode = mode,
                        data = temp_frame)
    } else{
      res_emm = emmeans(ref_grid(model,
                                 at = list(avg = avg_vals),
                                 data = temp_frame),
                        ~ avg,
                        mode = mode,
                        data = temp_frame)
    }

  } else {
    if(grepl("condition", covs)){
      res_emm = emmeans(model,
                        ~ condition ,
                        mode = mode,
                        data = temp_frame)
    } else{
      res_emm = emmeans(model,
                        ~ 1 ,
                        mode = mode,
                        data = temp_frame)
    }
  }
  res_emm
}

# self-contained model ----
# The gls/lme call built above refers to objects that only exist inside
# tolerance_limit() (temp_frame, cor1, var1, and fixed for lme), so update()
# and getData() on the returned model would fail. Replace them with the
# objects themselves. The data are stored in a small environment rather than
# inline so that print(model) does not deparse the whole data frame.
model_self_contained = function(model,
                                data,
                                cor1,
                                var1,
                                fixed = NULL,
                                random = NULL){
  data_env = new.env(parent = baseenv())
  assign("tol_data", data, envir = data_env)

  if(inherits(model, "lme")){
    model$call[[1]] = quote(nlme::lme)
    model$call$fixed = fixed
    model$call$random = random
  } else {
    model$call[[1]] = quote(nlme::gls)
  }
  model$call$data = call("get", "tol_data", envir = data_env)
  # assigning NULL removes the argument from the call
  model$call$correlation = cor1
  model$call$weights = var1

  model
}

# random-intercept variances ----
# re_vars(): one variance per level, named "id" (outer) and, if nested,
# "id_2" (inner); re_var(): their sum (zero for gls)
re_vars = function(model){
  if(!inherits(model, "lme")){
    return(c(id = 0))
  }
  mats = as.matrix(model$modelStruct$reStruct)
  vapply(mats, function(m) m[1, 1], numeric(1)) * sigma(model)^2
}

re_var = function(model){
  sum(re_vars(model))
}

# fixed-effect coefficients ----
fixed_coef = function(model){
  if(inherits(model, "lme")) nlme::fixef(model) else stats::coef(model)
}

# bootstrap ----
boot_delta = function(model,
                      temp_frame,
                      avg_vals,
                      res_emm,
                      sd_info,
                      replicates){
  # Simulate from the point estimates of the fitted model; the covariance
  # factorization is fixed across replicates so it is computed once.
  sim_setup = gls_sim_setup(model, data = temp_frame)

  # The emmeans grid is a linear function of the fixed effects, so each
  # replicate's bias and SEM come from the coefficients and vcov() directly
  # rather than re-running emmeans.
  L = res_emm@linfct
  emm_base = as.data.frame(res_emm)
  class(emm_base) = "data.frame"
  n_row = nrow(tol_grid(res_emm = emm_base,
                        model = model,
                        avg_vals = avg_vals))

  dat2 = temp_frame
  # replicates x grid rows; rows stay NA for replicates whose refit failed
  b_star = s_star = sem_star = nu_star =
    matrix(NA_real_, nrow = replicates, ncol = n_row)
  # shares of the variance pieces: replicates x grid rows x pieces
  sh_star = if(sd_info$type == "comp"){
    array(NA_real_, c(replicates, n_row, ncol(sd_info$shares)))
  } else NULL

  for(i in seq_len(replicates)){
    dat2$delta = gls_sim_draw(sim_setup)
    res_i = tryCatch(update(model, data = dat2),
                     error = function(e) NULL)
    if(is.null(res_i)){
      next
    }

    grid_i = emm_base
    grid_i$emmean = as.vector(L %*% fixed_coef(res_i))
    grid_i$SE = sqrt(rowSums((L %*% vcov(res_i)) * L))
    # rows come back in the same order as the original grid
    grid_i = tol_grid(res_emm = grid_i,
                      model = res_i,
                      avg_vals = avg_vals)

    b_star[i, ] = grid_i$emmean
    s_star[i, ] = marginal_sd_grid(res_i, grid_i)
    sem_star[i, ] = grid_i$SE
    # variance-component information for the analytic limits in this replicate
    if(sd_info$type == "comp"){
      grid_i$SD = s_star[i, ]
      sh_star[i, , ] = sd_bound_info(res_i, grid_i, dat2)$shares
    } else {
      nu_star[i, ] = suppressWarnings(tol_sd_df(res_i, grid_i))
    }
  }

  failed = is.na(b_star[, 1])
  if(mean(failed) > 0.05){
    warning(sum(failed), " of ", replicates, " bootstrap refits failed ",
            "and were dropped.", call. = FALSE)
  }
  keep = !failed

  list(bias = b_star[keep, , drop = FALSE],
       SD = s_star[keep, , drop = FALSE],
       SEM = sem_star[keep, , drop = FALSE],
       shares = if(is.null(sh_star)) NULL else sh_star[keep, , , drop = FALSE],
       nu = nu_star[keep, , drop = FALSE])
}

# tolerance limits ----
# Both use the content (pred_level, beta) and confidence (tol_level, gamma).
# "joint": beta-content, gamma-confidence tolerance interval; with confidence
#   gamma at least beta of the differences lie within [lower.TL, upper.TL].
# "iu": equal-tailed; one-sided gamma confidence bounds on the (1 - beta)/2
#   and (1 + beta)/2 percentiles of the differences (the limits of agreement).
#   Each bound is a one-sided tolerance bound with content (1 + beta)/2; the
#   pair is not a joint gamma-confidence interval.

# upper confidence bound of the residual SD ----
# How the one-sided upper confidence bound for the SD of a single difference
# is formed:
# - Variance components ("comp"): gls with compound symmetry, or lme with
#   random intercept(s). For each row the variance s^2 is split into pieces
#   a_j = shares_j * s^2, each estimated with its own df, and the bound
#   combines them with the MOVER (Zou, 2013), as in
#   agreement_limit(data_type = "nest"); a single Satterthwaite df
#   understates the skewness of the between-subject part.
#     gls, compound symmetry (2 pieces):
#       a1 = (rho + (1 - rho) / mh) s^2          df = subjects - 1
#       a2 = the rest                             df = N - subjects
#     lme, random intercept(s) (3 pieces; the middle one is empty without
#     nesting): with sigma_1^2 (subject), sigma_2^2 (setting within
#     subject), and sigma_e^2 (residual, for the row),
#       a1 = sigma_1^2 + tau sigma_2^2 + kappa_sub sigma_e^2
#                                                 df = subjects - 1
#       a2 = (1 - tau) sigma_2^2 + (kappa_set - kappa_sub) sigma_e^2
#                                                 df = settings - subjects
#       a3 = (1 - kappa_set) sigma_e^2            df = N - settings
#     where tau = mean over subjects of sum_c m_ic^2 / m_i^2 (the setting
#     share of the variance of a subject mean), and kappa_sub and kappa_set
#     are the means of 1'R 1 / m^2 over subjects and over settings for the
#     residual correlation matrix R (1 / m without residual correlation).
#     This is the nested ANOVA decomposition
#     s^2 = MS_A / (sk) + MS_B (1/k - 1/(sk)) + MS_W (1 - 1/k) for balanced
#     data. Without nesting, a2 is empty and a1, a3 are the random-intercept
#     pieces.
# - Otherwise ("df"): chi-square bound with the effective df from
#   tol_sd_df() (N - p with no correlation or variance function, which makes
#   the limits exact; approximate for other structures).
sd_bound_info = function(model,
                         grid,
                         data){
  cs = model$modelStruct$corStruct
  is_lme = inherits(model, "lme")

  if(!is_lme && !inherits(cs, "corCompSymm")){
    return(list(type = "df",
                nu = tol_sd_df(model = model,
                               grid = grid)))
  }

  # With a separate residual variance per condition (varIdent by condition),
  # each condition's variance components are estimated from the measurements
  # in that condition: the subject means in a condition are based on that
  # subject's measurements in the condition only. The cluster sizes and df
  # are therefore taken per condition for each grid row. Otherwise they come
  # from the whole data set.
  vs = model$modelStruct$varStruct
  per_cond = inherits(vs, "varIdent") &&
    !is.null(nlme::getGroupsFormula(vs)) &&
    identical(all.vars(nlme::getGroupsFormula(vs)), "condition") &&
    "condition" %in% names(grid) &&
    "condition" %in% names(data)

  row_sets = if(per_cond){
    lapply(as.character(grid$condition), function(cond){
      as.character(data$condition) == cond
    })
  } else {
    rep(list(rep(TRUE, nrow(data))), nrow(grid))
  }
  s2 = grid$SD^2

  if(is_lme){
    vars = re_vars(model)
    s1 = vars[["id"]]
    s2n = if("id_2" %in% names(vars)) vars[["id_2"]] else 0
    se2 = s2 - s1 - s2n
    parts = lapply(row_sets, function(rows) lme_structure(model, data, rows))

    tau = vapply(parts, `[[`, numeric(1), "tau")
    k_sub = vapply(parts, `[[`, numeric(1), "kappa_sub")
    k_set = vapply(parts, `[[`, numeric(1), "kappa_set")
    a1 = s1 + tau * s2n + k_sub * se2
    a2 = (1 - tau) * s2n + (k_set - k_sub) * se2
    a3 = (1 - k_set) * se2
    shares = cbind(a1, a2, a3) / s2
    df = cbind(vapply(parts, `[[`, numeric(1), "n_sub") - 1,
               vapply(parts, `[[`, numeric(1), "n_set") -
                 vapply(parts, `[[`, numeric(1), "n_sub"),
               vapply(parts, `[[`, numeric(1), "N") -
                 vapply(parts, `[[`, numeric(1), "n_set"))
    between = rep(sqrt(s1), nrow(grid))
    nested = if("id_2" %in% names(vars)) rep(sqrt(s2n), nrow(grid)) else
      rep(NA_real_, nrow(grid))
  } else {
    grp = nlme::getGroups(data, nlme::getGroupsFormula(cs))
    counts = lapply(row_sets, function(rows) cs_counts(grp[rows]))
    rho = cs_rho(model)
    mh = vapply(counts, `[[`, numeric(1), "mh")
    k_b = rho + (1 - rho) / mh
    shares = cbind(k_b, 1 - k_b)
    df = cbind(vapply(counts, `[[`, numeric(1), "df_b"),
               vapply(counts, `[[`, numeric(1), "df_w"))
    between = sqrt(rho) * grid$SD
    nested = rep(NA_real_, nrow(grid))
  }

  dimnames(shares) = dimnames(df) = NULL
  list(type = "comp",
       shares = shares,
       df = df,
       between = between,
       nested = nested)
}

# cluster sizes for the MOVER bound: harmonic mean number of measurements per
# subject, and the between- and within-subject df
cs_counts = function(grp){
  m_i = as.vector(table(grp))
  m_i = m_i[m_i > 0]
  n_sub = length(m_i)
  list(mh = n_sub / sum(1 / m_i),
       df_b = n_sub - 1,
       df_w = sum(m_i) - n_sub)
}

# Structure of an lme random intercept model over the data rows in `rows`:
# the numbers of subjects, settings (the inner level if nested, otherwise the
# subjects), and measurements; tau; and kappa for subject and setting means.
# The residual correlation is block diagonal across the innermost groups, so
# 1'R 1 for a subject is the sum over its settings.
lme_structure = function(model,
                         data,
                         rows){
  sub = as.character(data$id)
  set = if("id_2" %in% names(data)){
    paste(sub, as.character(data$id_2), sep = "/")
  } else {
    sub
  }
  sub_r = sub[rows]
  set_r = set[rows]

  # sum of the residual correlation matrix over each setting's rows in `rows`
  # (corMatrix blocks are named by the innermost group, "outer/inner" when
  # nested, and are in data order within each group)
  cs = model$modelStruct$corStruct
  set_ids = unique(set_r)
  sum_R = if(is.null(cs)){
    as.vector(table(set_r)[set_ids])
  } else {
    cor_mat = nlme::corMatrix(cs)
    vapply(set_ids, function(g){
      in_g = which(set == g)
      keep = rows[in_g]
      sum(as.matrix(cor_mat[[g]])[keep, keep, drop = FALSE])
    }, numeric(1))
  }
  names(sum_R) = set_ids

  m_set = as.vector(table(set_r)[set_ids])
  sub_of_set = sub_r[match(set_ids, set_r)]
  sub_ids = unique(sub_r)
  m_sub = vapply(sub_ids, function(i) sum(m_set[sub_of_set == i]), numeric(1))

  list(n_sub = length(sub_ids),
       n_set = length(set_ids),
       N = sum(rows),
       tau = mean(vapply(sub_ids, function(i){
         sum(m_set[sub_of_set == i]^2) / m_sub[[i]]^2
       }, numeric(1))),
       kappa_sub = mean(vapply(sub_ids, function(i){
         sum(sum_R[sub_of_set == i]) / m_sub[[i]]^2
       }, numeric(1))),
       kappa_set = mean(sum_R / m_set^2))
}

# correlation of a compound symmetry model, truncated at zero
cs_rho = function(model){
  rho = stats::coef(model$modelStruct$corStruct, unconstrained = FALSE)
  max(unname(rho), 0)
}

# One-sided upper confidence bound, at `level`, for the SD. Vectorized over s
# and over the rows of shares and df (for "comp") or nu (for "df"). Pieces
# with zero df are empty and are dropped.
sd_upper_at = function(s,
                       level,
                       info){
  if(info$type == "df"){
    return(s * sqrt(info$nu / qchisq(1 - level, info$nu)))
  }
  s2 = s^2
  shares = comp_rows(info$shares, length(s))
  df = comp_rows(info$df, length(s))
  total = 0
  for(j in seq_len(ncol(shares))){
    d = df[, j]
    move = shares[, j] * s2 * (d / qchisq(1 - level, d) - 1)
    move[!(d > 0)] = 0
    total = total + move^2
  }
  sqrt(s2 + sqrt(total))
}

# degrees of freedom of the residual variance
sd_df_at = function(s,
                    info){
  if(info$type == "df"){
    return(info$nu)
  }
  s2 = s^2
  shares = comp_rows(info$shares, length(s))
  df = comp_rows(info$df, length(s))
  total = 0
  for(j in seq_len(ncol(shares))){
    d = df[, j]
    v = (shares[, j] * s2)^2 / d
    v[!(d > 0)] = 0
    total = total + v
  }
  # Satterthwaite df of s2 (reported only; the bound uses the MOVER)
  s2^2 / total
}

# recycle a one-row shares/df matrix to n rows
comp_rows = function(m, n){
  m = as.matrix(m)
  if(nrow(m) == 1 && n > 1){
    m = m[rep(1, n), , drop = FALSE]
  }
  m
}

# one grid row of an info list (nu, or the rows of shares and df, have one
# value per grid row)
sd_info_row = function(info, j){
  if(info$type == "df"){
    info$nu = info$nu[j]
  } else {
    info$shares = info$shares[j, , drop = FALSE]
    info$df = info$df[j, , drop = FALSE]
  }
  info
}

# analytic limits ----
# Closed-form tolerance limits at nominal confidence `level`, vectorized over
# the inputs (grid rows, or bootstrap replicates of one row).
# - "joint": the approximation of Howe (1969), z * SEP * (upper bound of
#   sigma / sigma). With independent data the ratio is
#   sqrt(nu / qchisq(1 - level, nu)) and SEP = s * sqrt(1 + 1/n), which is
#   exactly Howe's k.
# - "iu": with independent data the exact noncentral t bound, since
#   (bias - percentile) / SEM scaled by s / sigma is noncentral t with nu df;
#   otherwise (or when the noncentral t is numerically unreliable) the MOVER
#   bound of Zou (2013).
analytic_bounds = function(b,
                           s,
                           sem,
                           df,
                           level,
                           info,
                           pred_level,
                           bound_type,
                           independent){
  zp = qnorm((1 + pred_level) / 2)
  u_s = sd_upper_at(s, level, info)

  if(bound_type == "joint"){
    k_sep = zp * u_s / s * sqrt(s^2 + sem^2)
    return(list(lower = b - k_sep,
                upper = b + k_sep))
  }

  ncp = zp * s / sem
  use_nct = independent & ncp < 37
  k_nct = if(any(use_nct)){
    suppressWarnings(qt(level, df = sd_df_at(s, info), ncp = ncp)) * sem
  } else 0

  h_b = qt(level, df) * sem
  lower_mover = (b - zp * s) - sqrt(h_b^2 + zp^2 * (u_s - s)^2)
  upper_mover = (b + zp * s) + sqrt(h_b^2 + zp^2 * (u_s - s)^2)

  list(lower = ifelse(use_nct, b - k_nct, lower_mover),
       upper = ifelse(use_nct, b + k_nct, upper_mover))
}

tol_approx = function(emm_df,
                      sd_info,
                      pred_level,
                      tol_level,
                      bound_type,
                      independent){
  lim = analytic_bounds(b = emm_df$emmean,
                        s = emm_df$SD,
                        sem = emm_df$SEM,
                        df = emm_df$df,
                        level = tol_level,
                        info = sd_info,
                        pred_level = pred_level,
                        bound_type = bound_type,
                        independent = independent)
  emm_df$lower.TL = lim$lower
  emm_df$upper.TL = lim$upper
  emm_df
}

# bootstrap calibration ----
# The analytic limits are computed in each bootstrap replicate at a range of
# nominal levels. The level is then chosen so that, across replicates, the
# limits achieve the target confidence (tol_level) for the fitted model, and
# the analytic limits for the observed data are reported at that level
# (bootstrap calibration; Loh, 1987; Beran, 1987). The analytic limits
# already account for most of the uncertainty in the variance components, so
# the calibration only corrects what remains, and depends much less on the
# estimated correlation than calibrating the raw limits would.
# - "joint": the level at which a proportion tol_level of replicates have
#   content >= pred_level under the fitted distribution of the differences.
# - "iu": separately for each bound, the level at which a proportion tol_level
#   of replicates lie beyond the fitted limit of agreement.
#
# TODO: With few subjects the calibration still treats the estimated
# correlation (rho) as known. In simulations with 10-15 subjects its coverage
# was off by 2-3 points in either direction (about 0.93 with compound symmetry,
# with or without condition; about 0.96 with AR(1)), while the analytic limits
# were 0.95-0.965, which is why "boot_cal" is marked experimental. A possible
# improvement is to draw the variance parameters for each replicate from their
# approximate sampling distribution (model$apVar, on the unconstrained scale)
# before simulating, and to judge each replicate against the parameters it was
# generated from rather than the fitted model. This must be checked by
# simulation: the normal approximation to apVar is poor with ~10 subjects and
# could overshoot into conservative limits.
tol_boot = function(emm_df,
                    boot_df,
                    sd_info,
                    pred_level,
                    tol_level,
                    bound_type,
                    independent){
  zp = qnorm((1 + pred_level) / 2)
  n_row = nrow(emm_df)
  lower.TL = upper.TL = level_l = level_u = numeric(n_row)

  for(j in seq_len(n_row)){
    b = emm_df$emmean[j]
    s = emm_df$SD[j]
    info_j = sd_info_row(sd_info, j)

    info_star = if(sd_info$type == "comp"){
      utils::modifyList(info_j,
                        list(shares = matrix(boot_df$shares[, j, ],
                                             ncol = dim(boot_df$shares)[3])))
    } else {
      list(type = "df", nu = boot_df$nu[, j])
    }
    bounds_star = function(level){
      analytic_bounds(b = boot_df$bias[, j],
                      s = boot_df$SD[, j],
                      sem = boot_df$SEM[, j],
                      df = emm_df$df[j],
                      level = level,
                      info = info_star,
                      pred_level = pred_level,
                      bound_type = bound_type,
                      independent = independent)
    }
    bounds_obs = function(level){
      analytic_bounds(b = b,
                      s = s,
                      sem = emm_df$SEM[j],
                      df = emm_df$df[j],
                      level = level,
                      info = info_j,
                      pred_level = pred_level,
                      bound_type = bound_type,
                      independent = independent)
    }

    if(bound_type == "joint"){
      covered = function(level){
        lim = bounds_star(level)
        content = pnorm((lim$upper - b) / s) - pnorm((lim$lower - b) / s)
        mean(content >= pred_level)
      }
      level_l[j] = level_u[j] = calibrate_level(covered, tol_level)
    }

    if(bound_type == "iu"){
      level_l[j] = calibrate_level(function(level){
        mean(bounds_star(level)$lower <= b - zp * s)
      }, tol_level)
      level_u[j] = calibrate_level(function(level){
        mean(bounds_star(level)$upper >= b + zp * s)
      }, tol_level)
    }

    lower.TL[j] = bounds_obs(level_l[j])$lower
    upper.TL[j] = bounds_obs(level_u[j])$upper
  }

  emm_df$lower.TL = lower.TL
  emm_df$upper.TL = upper.TL
  emm_df$lower.TL.level = level_l
  emm_df$upper.TL.level = level_u
  emm_df
}

# Smallest nominal level at which the bootstrap coverage reaches the target.
# Coverage is non-decreasing in the level, so bisection is used.
calibrate_level = function(coverage,
                           target,
                           lower = 0.5,
                           upper = 1 - 1e-6){
  if(coverage(lower) >= target){
    return(lower)
  }
  if(coverage(upper) < target){
    warning("Bootstrap calibration did not reach the target confidence; ",
            "consider more replicates.", call. = FALSE)
    return(upper)
  }
  for(iter in 1:40){
    mid = (lower + upper) / 2
    if(coverage(mid) >= target){
      upper = mid
    } else {
      lower = mid
    }
  }
  upper
}

# effective df of the residual variance ----
# Degrees of freedom for the residual variance (SD^2) of each grid row, from a
# Satterthwaite-type approximation: df = 2 * SD^4 / var(SD^2), with var(SD^2)
# from the delta method on the approximate covariance of the variance
# parameters (apVar). With no correlation or variance function this is the
# residual df, N - p. Falls back to the df of the bias if apVar is unavailable.
tol_sd_df = function(model,
                     grid){
  N = model$dims$N
  p = model$dims$p
  ms = model$modelStruct

  if(is.null(ms$corStruct) && is.null(ms$varStruct)){
    return(rep(N - p, nrow(grid)))
  }

  av = model$apVar
  if(is.null(av) || is.character(av)){
    warning("Approximate covariance of the variance parameters is not available; ",
            "using the degrees of freedom of the bias for the residual variance.",
            call. = FALSE)
    return(grid$df)
  }

  pars = attr(av, "Pars")
  var_pars = grepl("^varStruct", names(pars))

  sd2 = function(pp){
    m = model
    m$sigma = exp(pp[["lSigma"]])
    if(any(var_pars)){
      m$modelStruct$varStruct = nlme::`coef<-`(m$modelStruct$varStruct,
                                               value = pp[var_pars])
    }
    resid_sd_grid(m, grid)^2
  }

  f0 = sd2(pars)
  G = matrix(0, nrow = nrow(grid), ncol = length(pars))
  for(j in seq_along(pars)){
    # the correlation parameters do not affect the marginal variance
    if(grepl("^corStruct", names(pars)[j])) next
    h = 1e-5 * max(1, abs(pars[j]))
    up = dn = pars
    up[j] = up[j] + h
    dn[j] = dn[j] - h
    G[, j] = (sd2(up) - sd2(dn)) / (2 * h)
  }

  v = rowSums((G %*% av) * G)
  2 * f0^2 / v
}

# prediction grid ----
# Data frame of the emmeans grid. If the variance function depends on avg
# but the mean model does not, the grid is expanded over avg_vals so that
# the limits can vary with avg even though the bias does not.
tol_grid = function(res_emm,
                    model,
                    avg_vals){
  grid = as.data.frame(res_emm)
  class(grid) = "data.frame"
  vrSt = model$modelStruct$varStruct

  if(!is.null(vrSt) && !("avg" %in% colnames(grid))){
    var_covs = all.vars(nlme::getCovariateFormula(vrSt))
    if("avg" %in% var_covs){
      grid = merge(grid,
                   data.frame(avg = avg_vals),
                   by = NULL)
    }
  }

  return(grid)
}

# SD of a single difference for each row of the prediction grid ----
# sigma(model) is only the residual SD at the reference level of the variance
# function, so the variance function has to be evaluated at each grid row
# (resid_sd_grid). For lme the random-intercept variance is added, giving the
# marginal SD of a difference from a new subject.
marginal_sd_grid = function(model,
                            grid){
  sqrt(re_var(model) + resid_sd_grid(model, grid)^2)
}

resid_sd_grid = function(model,
                         grid){
  vrSt = model$modelStruct$varStruct

  if(is.null(vrSt)){
    return(rep(sigma(model), nrow(grid)))
  }

  # varIdent without a grouping factor is a constant variance
  if(inherits(vrSt, "varIdent") && is.null(nlme::getGroupsFormula(vrSt))){
    return(rep(sigma(model), nrow(grid)))
  }

  predict_varFunc(model, newdata = grid)
}

# prediction limits ----
add_pred_limits = function(grid,
                           model,
                           alpha.pred){
  grid %>%
    rename(SEM = SE) %>%
    mutate(SD = marginal_sd_grid(model, grid),
           SEP = sqrt(SD^2 + SEM^2),
           lower.PL = emmean - qt(1-alpha.pred/2,df) * SEP,
           upper.PL = emmean + qt(1-alpha.pred/2,df) * SEP)
}

