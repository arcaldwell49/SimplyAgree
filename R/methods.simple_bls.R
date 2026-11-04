#' Methods for simple_bls objects
#'
#' Methods for the results of [bls_reg()]. The results are also
#' `simple_eiv` objects, so [coef()], [vcov()], [confint()], [joint_test()],
#' and [plot_joint()] work as for [dem_reg()]; the methods here add the CBLS
#' fit in the Bland-Altman plot and prediction intervals.
#'
#' @param x,object A `simple_bls` object from [bls_reg()].
#' @param type `"xy"` (default) for the BLS line in the plot of `y` against
#'   `x`, or `"md"` for the CBLS line in the Bland-Altman plot (differences
#'   `x - y` against means).
#' @param newdata Values at which to predict: of `x` for `type = "xy"`, or of
#'   the mean of the two methods for `type = "md"`. A numeric vector, or a
#'   data frame with a column named as `x` (or `avg`). If omitted, the
#'   subjects' values are used.
#' @param interval `"none"` (default), `"confidence"` (pointwise, for the
#'   line at each value), `"band"` (simultaneous confidence band for the
#'   whole line), or `"prediction"` (for a new measurement by `y`, or a new
#'   difference, given a new measurement by `x`, or a new mean). The band uses
#'   the Working-Hotelling multiplier `sqrt(2 F(2, n - 2))`, so it contains
#'   exactly the lines (intercept, slope) that [joint_test()] does not
#'   reject; BivRegBLS's band also multiplies by `sqrt((n - 1) / (n - 2))`.
#'   `"tolerance"` (experimental) gives tolerance limits: with confidence
#'   `tol_level`, they contain at least a proportion `level` of new
#'   measurements (see [bls_reg()]).
#' @param level Level of the intervals. Default is the `conf.level` of the fit
#'   for confidence intervals and the band, and its `pred_level` for
#'   prediction intervals and tolerance limits (the proportion of new
#'   measurements they contain).
#' @param tol_level,bound_type For tolerance limits: the confidence level, and
#'   `"joint"` or `"iu"` as in [tolerance_limit()]. Default: the values given
#'   to [bls_reg()].
#' @param reps_x,reps_y For prediction intervals: the new values are means of
#'   this many replicates of each method. Default is 1.
#' @param se.fit For `predict()`: if TRUE, also return the standard errors of
#'   the fitted line (on the log scale for a fit with `transform = "log"`).
#' @param back_transform For a fit with `transform = "log"`: if TRUE
#'   (default), `newdata` is on the original scale (`x`, or the geometric
#'   mean of the two methods for `type = "md"`) and the results are
#'   back-transformed (`y`, or the ratio `x / y`); if FALSE, everything is on
#'   the log scale, as are `fitted()`, `residuals()` and `check()`.
#' @param x_name,y_name Axis labels for `plot()`.
#' @param n_points Number of points for the interval bands in `plot()`.
#' @param ... Not used.
#'
#' @return `print()` and `summary()` print the fit; `predict()` returns the
#'   fitted values, or a data frame with `fit`, `lwr`, and `upr` (and, with
#'   `se.fit`, a list with the standard errors and df); `plot()` and `check()`
#'   return ggplot2 objects; `fitted()` and `residuals()` return vectors.
#'
#' @examples
#' data("reps")
#' fit <- bls_reg(y ~ x, data = reps, id = "id")
#' predict(fit, newdata = c(4, 6), interval = "prediction")
#' predict(fit, newdata = c(4, 6), type = "md", interval = "confidence")
#' plot(fit, type = "md", interval = "prediction")
#' check(fit)
#'
#' @name simple_bls-methods
NULL

#' @rdname simple_bls-methods
#' @method print simple_bls
#' @export
print.simple_bls <- function(x, ...) {
  cat("Bivariate Least Squares (BLS) Regression with ",
      x$conf.level * 100, "% C.I.\n\n", sep = "")
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients (BLS, y on x):\n")
  print(x$coefficients, digits = 4)
  cat("\nBland-Altman plot (CBLS, x - y on mean):\n")
  print(x$cbls$coefficients, digits = 4)
  .bls_log_note(x)
  cat("\n")
  invisible(x)
}

#' @rdname simple_bls-methods
#' @method summary simple_bls
#' @export
summary.simple_bls <- function(object, ...) {
  e <- object$errors
  cat("Bivariate Least Squares (BLS) Regression with ",
      object$conf.level * 100, "% C.I.\n\n", sep = "")
  cat("Call:\n")
  print(object$call)
  .bls_log_note(object)
  cat("\nBLS, y on x (H0: intercept = 0, slope = 1):\n")
  print(object$model_table, digits = 4)
  cat("\nCBLS, Bland-Altman plot, x - y on mean (H0: intercept = 0, slope = 0):\n")
  print(object$cbls$model_table, digits = 4)
  cat(sprintf("\n%d degrees of freedom\n", object$df.residual))
  if (e$known) {
    fmt_df <- function(d) if (is.finite(d)) format(round(d, 1)) else "known"
    describe <- function(model, v, p) {
      switch(model,
             constant = sprintf("constant, variance %.4g", v),
             proportional = sprintf("proportional, CV %.4g", p[["cv"]]),
             power = sprintf("power, variance %.4g * level^(2 * %.3f)",
                             p[["sigma"]]^2, p[["theta"]]),
             subject = sprintf("one per subject, mean %.4g", p[["mean"]]),
             "given function of the level")
    }
    cat(sprintf("Measurement error variance (single measurement), %s:\n",
                switch(e$source,
                       known = "given",
                       replicates = "estimated from the replicates",
                       "given / estimated from the replicates")))
    cat(sprintf("  x: %s (df %s)\n", describe(e$model_x, e$var_x, e$params_x),
                fmt_df(e$df_var_x)))
    cat(sprintf("  y: %s (df %s)\n", describe(e$model_y, e$var_y, e$params_y),
                fmt_df(e$df_var_y)))
  } else {
    cat("Measurement error variances: only their ratio is given; ",
        "prediction intervals are not available\n", sep = "")
  }
  constant <- e$model_x == "constant" && e$model_y == "constant"
  cat(sprintf("Error variance ratio var(x)/var(y): %.4f%s\n", object$error.ratio,
              if (constant) "" else " (at the mean level)"))
  if (identical(e$replicates, "linked")) {
    cat(sprintf("Linked replicates: error correlation of x and y on the same occasion %.3f (df %d)\n",
                e$rho, as.integer(e$df_rho)))
  }
  d <- object$dispersion
  if (!is.null(d) && is.finite(d$p.value)) {
    test <- if (is.finite(d$df2)) {
      sprintf("F(%d, %.1f)", as.integer(d$df1), d$df2)
    } else {
      sprintf("chi-squared(%d) / %d", as.integer(d$df1), as.integer(d$df1))
    }
    cat(sprintf("Dispersion (1 if the measurement errors explain the scatter): %.3f; %s, p = %.4g\n",
                d$s2, test, d$p.value))
    if (is.numeric(object$equation_error)) {
      cat(sprintf("Equation error (given): SD %.4g for x and %.4g for y (each on its own scale); included in the fit and the intervals\n",
                  sqrt(d$tau2_x), sqrt(d$tau2_y)))
    } else if (isTRUE(d$tau2 > 0)) {
      cat(sprintf("Equation error: SD %.4g of each method's deviations from the line (true-value scale, common to both); included in the fit and the intervals\n",
                  sqrt(d$tau2)))
    } else if (!isTRUE(object$equation_error) && d$p.value < 0.05) {
      cat("Equation error is not modeled (equation_error = FALSE); with a dispersion above 1 the prediction intervals are too narrow\n")
    }
  }
  invisible(object)
}

#' @rdname simple_bls-methods
#' @method predict simple_bls
#' @export
predict.simple_bls <- function(object,
                               newdata = NULL,
                               type = c("xy", "md"),
                               interval = c("none", "confidence", "band",
                                            "prediction", "tolerance"),
                               level = NULL,
                               tol_level = NULL,
                               bound_type = NULL,
                               reps_x = 1,
                               reps_y = 1,
                               se.fit = FALSE,
                               back_transform = TRUE,
                               ...) {
  type <- match.arg(type)
  interval <- match.arg(interval)
  if (identical(object$transform, "log") && isTRUE(back_transform)) {
    # predict on the log scale, then back-transform
    col <- if (type == "xy") attr(object$terms, "term.labels")[1] else "avg"
    if (!is.null(newdata)) {
      v <- if (is.data.frame(newdata)) newdata[[col]] else newdata
      if (is.null(v)) stop("Variable '", col, "' not found in newdata.")
      if (any(v <= 0, na.rm = TRUE)) {
        stop("newdata must be positive (transform = 'log').")
      }
      if (is.data.frame(newdata)) newdata[[col]] <- log(v) else
        newdata <- log(as.numeric(v))
    }
    res <- predict.simple_bls(object, newdata = newdata, type = type,
                              interval = interval, level = level,
                              tol_level = tol_level, bound_type = bound_type,
                              reps_x = reps_x, reps_y = reps_y,
                              se.fit = se.fit, back_transform = FALSE)
    if (is.list(res) && !is.data.frame(res)) {
      res$fit <- exp(res$fit)
      return(res)
    }
    return(exp(res))
  }
  if (is.null(level)) {
    level <- if (interval %in% c("prediction", "tolerance")) {
      object$pred_level
    } else {
      object$conf.level
    }
  }
  if (is.null(tol_level)) {
    tol_level <- if (is.null(object$tol_level)) 0.95 else object$tol_level
  }
  if (is.null(bound_type)) {
    bound_type <- if (is.null(object$bound_type)) "joint" else object$bound_type
  }
  bound_type <- match.arg(bound_type, c("joint", "iu"))
  x_name <- attr(object$terms, "term.labels")[1]
  if (type == "xy") {
    cf <- object$coefficients
    vc <- object$vcov
    col <- x_name
  } else {
    cf <- object$cbls$coefficients
    vc <- object$cbls$vcov
    col <- "avg"
  }
  if (is.null(newdata)) {
    df3 <- .bls_model(object)
    u <- if (type == "xy") df3$x else (df3$x + df3$y) / 2
  } else if (is.data.frame(newdata)) {
    if (!col %in% names(newdata)) {
      stop("Variable '", col, "' not found in newdata.")
    }
    u <- newdata[[col]]
  } else {
    u <- as.numeric(newdata)
  }
  fit <- unname(cf[1] + cf[2] * u)
  if (interval == "none" && !se.fit) {
    return(fit)
  }
  var_line <- vc[1, 1] + u^2 * vc[2, 2] + 2 * u * vc[1, 2]
  df_res <- object$df.residual
  if (interval == "none") {
    return(list(fit = fit, se.fit = sqrt(var_line), df = df_res))
  }
  if (interval == "confidence") {
    q <- stats::qt(1 - (1 - level) / 2, df_res)
    half <- q * sqrt(var_line)
  } else if (interval == "band") {
    half <- sqrt(2 * stats::qf(level, 2, df_res)) * sqrt(var_line)
  } else {
    e <- object$errors
    if (!e$known) {
      stop("Prediction and tolerance intervals need the measurement error ",
           "variances; only their ratio was given. Refit with var_x and ",
           "var_y, or with replicates.")
    }
    if (!is.numeric(reps_x) || !is.numeric(reps_y) || reps_x < 1 || reps_y < 1) {
      stop("reps_x and reps_y must be at least 1.")
    }
    # error variances of the new measurements, at their levels: x and the
    # fitted y, or (Bland-Altman plot) x = M + D / 2 and y = M - D / 2
    lev_x <- if (type == "xy") u else u + fit / 2
    lev_y <- if (type == "xy") fit else u - fit / 2
    vx <- .bls_eval(e$fun_x, lev_x, "var_x") / reps_x
    vy <- .bls_eval(e$fun_y, lev_y, "var_y") / reps_y
    b1 <- unname(cf[2])
    # linked replicates: the new measurements of x and y are taken on the
    # same occasions (min(reps_x, reps_y) of them), so their errors share
    # the occasion effects, with covariance rho sqrt(var_x var_y) per shared
    # occasion
    rho <- if (identical(e$replicates, "linked")) e$rho else 0
    vc_xy <- rho * sqrt(vx * reps_x * vy * reps_y) * min(reps_x, reps_y) /
      (reps_x * reps_y)
    # the error variance of a new point, as a_x vx + a_y vy + a_c vc_xy
    if (type == "xy") {
      a_x <- b1^2
      a_y <- 1
      a_c <- -2 * b1
    } else {
      # D = x - y, M = (x + y) / 2: var(D - g1 M error) =
      # vx (1 - g1/2)^2 + vy (1 + g1/2)^2 - 2 (1 - g1/2) (1 + g1/2) vc_xy
      a_x <- (1 - b1 / 2)^2
      a_y <- (1 + b1 / 2)^2
      a_c <- -2 * (1 - b1 / 2) * (1 + b1 / 2)
    }
    v_c <- a_c * vc_xy
    # the measurement error part and its df: with linked replicates it is
    # the variance of one combination of the paired errors, estimated from
    # the paired within-subject deviations on df_rho df (Satterthwaite over
    # pieces that partly cancel would give far too few df); otherwise the
    # two variances on their own df
    v_meas <- a_x * vx + a_y * vy + v_c
    den_meas <- if (rho != 0) v_meas^2 / e$df_rho else
      (a_x * vx)^2 / e$df_var_x + (a_y * vy)^2 / e$df_var_y
    # equation error (common tau): the new subject's true value deviates
    # from the line by tau2 for x and b^2 tau2 for y (b the BLS slope),
    # estimated on n - 2 df
    # (or, given, method-specific: tau2_x for x and tau2_y for y)
    d_eq <- object$dispersion
    tau2_x <- if (is.null(d_eq$tau2_x)) d_eq$tau2 else d_eq$tau2_x
    tau2_y <- if (is.null(d_eq$tau2_y)) {
      unname(object$coefficients[2])^2 * d_eq$tau2
    } else d_eq$tau2_y
    if (!isTRUE(tau2_x > 0)) tau2_x <- 0
    if (!isTRUE(tau2_y > 0)) tau2_y <- 0
    tau2 <- tau2_x + tau2_y
    v_eq <- a_x * tau2_x + a_y * tau2_y
    if (interval == "prediction") {
      var_pred <- var_line + v_meas + v_eq
      df_pred <- var_pred^2 /
        (var_line^2 / df_res + den_meas + v_eq^2 / df_res)
      q <- stats::qt(1 - (1 - level) / 2, df_pred)
      half <- q * sqrt(var_pred)
    } else {
      # tolerance limits (experimental). New values are N(line + delta, s^2)
      # about the fitted line, delta the error of the fitted line and s^2 =
      # a_x vx + a_y vy + v_eq the spread about the true line.
      if (!is.numeric(tol_level) || length(tol_level) != 1 ||
          tol_level <= 0 || tol_level >= 1) {
        stop("tol_level must be a single number between 0 and 1.")
      }
      pieces <- cbind(a_x * vx, a_y * vy, v_eq + 0 * u)
      s2 <- rowSums(pieces) + v_c
      if (tau2 > 0) {
        # With equation error, the residuals estimate, on n - 2 df, the
        # spread of the subject means about the line: s2_means, their
        # measurement error (with the average numbers of replicates) plus
        # the equation error. The single-measurement error variances are
        # known from the replicates, so only s2_means is uncertain:
        # s^2 is bounded by s^2 + s2_means ((n - 2) / chi2 - 1), and the line
        # (whose variance scales with the dispersion) by the same factor.
        # With one measurement per subject this is the textbook regression
        # tolerance factor (Wallis).
        mod <- object$model
        m_x <- if (is.null(mod)) 1 else mean(mod$n_x)
        m_y <- if (is.null(mod)) 1 else mean(mod$n_y)
        k_xy <- if (is.null(mod$n_xy)) 0 else mean(mod$n_xy)
        s2_means <- a_x * vx * reps_x / m_x + a_y * vy * reps_y / m_y +
          a_c * rho * sqrt(vx * reps_x * vy * reps_y) * k_xy / (m_x * m_y) +
          v_eq
        r2 <- df_res / stats::qchisq(1 - tol_level, df_res)
        u_s <- sqrt(s2 + s2_means * (r2 - 1))
        c_line <- sqrt(var_line * r2)
        if (bound_type == "iu") {
          # MOVER bound on each prediction limit, as in tolerance_limit()
          zp <- stats::qnorm((1 + level) / 2)
          h_b <- stats::qt(tol_level, df_res) * sqrt(var_line)
          move <- sqrt(h_b^2 + zp^2 * (u_s - sqrt(s2))^2)
          lim <- list(lower = fit - zp * sqrt(s2) - move,
                      upper = fit + zp * sqrt(s2) + move)
        } else {
          half <- tdi_exact(c_line, u_s, level)
          lim <- list(lower = fit - half, upper = fit + half)
        }
      } else {
        # Only measurement error: the pieces have the df of the error
        # variances (a known piece, infinite df, adds no uncertainty;
        # sd_upper_at() drops pieces with 0 df), and the line its own
        # uncertainty on n - 2 df.
        cap <- function(d) if (is.finite(d)) d else 0
        info <- if (rho == 0) {
          list(type = "comp",
               shares = pieces[, 1:2, drop = FALSE] / s2,
               df = matrix(c(cap(e$df_var_x), cap(e$df_var_y)),
                           nrow = length(u), ncol = 2, byrow = TRUE))
        } else {
          # linked replicates: the measurement error part as one piece on
          # the paired df (see v_meas above)
          list(type = "comp", shares = matrix(1, length(u), 1),
               df = matrix(e$df_rho, length(u), 1))
        }
        if (bound_type == "iu") {
          lim <- analytic_bounds(b = fit, s = sqrt(s2), sem = sqrt(var_line),
                                 df = df_res, level = tol_level, info = info,
                                 pred_level = level, bound_type = "iu",
                                 independent = FALSE)
        } else {
          # with |delta| <= c (a t bound for the line) and the SD at most
          # u_s, the `level` quantile of |N(c, u_s^2)| contains at least that
          # proportion of N(delta, s^2) (tdi_exact())
          u_s <- sd_upper_at(sqrt(s2), tol_level, info)
          c_line <- stats::qt((1 + tol_level) / 2, df_res) * sqrt(var_line)
          half <- tdi_exact(c_line, u_s, level)
          lim <- list(lower = fit - half, upper = fit + half)
        }
      }
      res <- data.frame(fit = fit, lwr = lim$lower, upr = lim$upper)
      if (se.fit) {
        return(list(fit = res, se.fit = sqrt(var_line), df = df_res))
      }
      return(res)
    }
  }
  res <- data.frame(fit = fit, lwr = fit - half, upr = fit + half)
  if (se.fit) {
    return(list(fit = res, se.fit = sqrt(var_line), df = df_res))
  }
  res
}

#' @rdname simple_bls-methods
#' @method plot simple_bls
#' @export
plot.simple_bls <- function(x,
                            type = c("xy", "md"),
                            interval = c("none", "confidence", "band",
                                         "prediction", "tolerance"),
                            level = NULL,
                            x_name = NULL,
                            y_name = NULL,
                            n_points = 100,
                            ...) {
  type <- match.arg(type)
  interval <- match.arg(interval)
  if (is.null(level)) {
    level <- if (interval %in% c("prediction", "tolerance")) x$pred_level else x$conf.level
  }
  if (is.null(x_name)) {
    x_name <- attr(x$terms, "term.labels")[1]
  }
  if (is.null(y_name)) {
    y_name <- deparse(attr(x$terms, "variables")[[2]])
  }
  df <- .bls_model(x)
  logt <- identical(x$transform, "log")
  if (logt) {
    # back to the original scale: x and y, or geometric mean and ratio x / y
    df$x <- exp(df$x)
    df$y <- exp(df$y)
  }
  if (type == "xy") {
    pd <- data.frame(u = df$x, v = df$y)
    cf <- x$coefficients
    ref <- geom_abline(intercept = 0, slope = 1, colour = "black")
    labs_xy <- labs(x = paste0("Method: ", x_name),
                    y = paste0("Method: ", y_name))
  } else {
    pd <- if (logt) {
      data.frame(u = sqrt(df$x * df$y), v = df$x / df$y)
    } else {
      data.frame(u = (df$x + df$y) / 2, v = df$x - df$y)
    }
    cf <- x$cbls$coefficients
    ref <- geom_hline(yintercept = if (logt) 1 else 0, colour = "black")
    labs_xy <- if (logt) {
      labs(x = "Geometric Mean of Both Methods",
           y = paste0("Ratio of Methods (", x_name, " / ", y_name, ")"))
    } else {
      labs(x = "Average of Both Methods",
           y = paste0("Difference between Methods (", x_name,
                      " - ", y_name, ")"))
    }
  }
  # the fitted line (a curve on the original scale for log fits)
  fit_line <- if (logt) {
    u_fit <- exp(seq(log(min(pd$u)), log(max(pd$u)), length.out = n_points))
    geom_line(data = data.frame(u = u_fit,
                                v = predict(x, newdata = u_fit, type = type)),
              linetype = "dashed", colour = "red")
  } else {
    geom_abline(intercept = cf[1], slope = cf[2], linetype = "dashed",
                colour = "red")
  }
  p <- ggplot(pd, aes(x = u, y = v)) +
    ref +
    geom_point(na.rm = TRUE) +
    fit_line +
    labs_xy +
    theme_bw()
  band <- NULL
  if (interval != "none") {
    u_seq <- if (logt) {
      exp(seq(log(min(pd$u)), log(max(pd$u)), length.out = n_points))
    } else {
      seq(min(pd$u), max(pd$u), length.out = n_points)
    }
    band <- predict(x, newdata = u_seq, type = type, interval = interval,
                    level = level)
    band$u <- u_seq
    p <- p +
      geom_ribbon(data = band, aes(x = u, ymin = lwr, ymax = upr),
                  fill = "red", alpha = 0.2, inherit.aes = FALSE) +
      labs(caption = sprintf("%.0f%% %s shown", level * 100,
                             switch(interval,
                                    confidence = "confidence interval",
                                    band = "simultaneous confidence band",
                                    tolerance = "tolerance limits (experimental)",
                                    prediction = "prediction interval")))
  }
  if (type == "xy") {
    lims <- range(c(pd$u, pd$v, band$lwr, band$upr), na.rm = TRUE)
    p <- p + coord_fixed(ratio = 1, xlim = lims, ylim = lims)
  }
  p
}

#' @rdname simple_bls-methods
#' @method fitted simple_bls
#' @export
fitted.simple_bls <- function(object, ...) {
  df3 <- .bls_model(object)
  unname(object$coefficients[1] + object$coefficients[2] * df3$x)
}

#' @rdname simple_bls-methods
#' @param resid_type For `residuals()`: `"standardized"` (default), the
#'   residuals divided by their BLS standard deviation
#'   `sqrt(var_yi + b1^2 var_xi)`, or `"raw"`, `y - fitted`.
#' @method residuals simple_bls
#' @export
residuals.simple_bls <- function(object,
                                 resid_type = c("standardized", "raw"),
                                 ...) {
  resid_type <- match.arg(resid_type)
  df3 <- .bls_model(object)
  e <- df3$y - fitted(object)
  if (resid_type == "raw") {
    return(e)
  }
  b1 <- unname(object$coefficients[2])
  e / sqrt(df3$var_y + b1^2 * df3$var_x - 2 * b1 * .bls_cov(df3))
}

#' @rdname simple_bls-methods
#' @method check simple_bls
#' @export
check.simple_bls <- function(x, ...) {
  df3 <- .bls_model(x)
  b0 <- unname(x$coefficients[1])
  b1 <- unname(x$coefficients[2])
  e <- df3$y - (b0 + b1 * df3$x)
  c_xy <- .bls_cov(df3)
  s2 <- df3$var_y + b1^2 * df3$var_x - 2 * b1 * c_xy
  r <- e / sqrt(s2)
  # estimated true values
  x_hat <- df3$x + (b1 * df3$var_x - c_xy) * e / s2
  y_hat <- df3$y - (df3$var_y - b1 * c_xy) * e / s2
  avg_both <- (x_hat + y_hat) / 2
  mod <- lm(abs(r) ~ avg_both)
  ss <- anova(mod)$"Sum Sq"
  p_het <- pchisq((sum(ss) - ss[length(ss)]) / 2, df = 1, lower.tail = FALSE)
  clean <- theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent")
  )
  p1 <- ggplot(data.frame(x = avg_both, y = abs(r)), aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(se = TRUE, method = "loess", linewidth = .8,
                color = "#3aaf85", formula = y ~ x) +
    labs(y = "|Standardized Residuals|",
         x = "Average of Both Estimated Values",
         title = "Homogeneity of Residuals",
         subtitle = "Reference line should be flat and horizontal",
         caption = paste0("Heteroskedasticity", " \n",
                          "Breusch-Pagan Test: p = ", signif(p_het, 4))) +
    theme_bw() +
    clean
  norm_test <- shapiro.test(r)
  p2 <- plot_qq(x = data.frame(y = r)) +
    labs(caption = paste0("Normality", " \n", "Shapiro-Wilk Test: p = ",
                          signif(norm_test$p.value, 4))) +
    clean
  wrap_plots(p2, p1, ncol = 2) & plot_annotation(theme = clean)
}

utils::globalVariables(c("u", "v", "lwr", "upr"))

.bls_log_note <- function(x) {
  if (identical(x$transform, "log")) {
    cat("\nFitted on the log scale: log(y) = b0 + b1 log(x), and log(x / y) ",
        "on the mean of the logs (y = ",
        format(exp(unname(x$coefficients[1])), digits = 4), " x^",
        format(unname(x$coefficients[2]), digits = 4), ")\n", sep = "")
  }
}

# subject-level data stored by bls_reg() (with the error variances of the
# means, which cannot be rebuilt from the raw data and formula alone)
.bls_model <- function(object) {
  if (is.null(object$model)) {
    stop("The subject-level data were not stored (model = FALSE); refit ",
         "bls_reg() with model = TRUE.")
  }
  object$model
}

# error covariance of the subject means (linked replicates; 0 otherwise)
.bls_cov <- function(df3) {
  if (is.null(df3$cov_xy)) 0 else df3$cov_xy
}
