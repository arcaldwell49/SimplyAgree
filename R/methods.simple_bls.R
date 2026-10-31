#' Methods for simple_bls objects
#'
#' Methods for the results of [cbls_reg()]. The results are also
#' `simple_eiv` objects, so [coef()], [vcov()], [confint()], [joint_test()],
#' and [plot_joint()] work as for [dem_reg()]; the methods here add the CBLS
#' fit in the Bland-Altman plot and prediction intervals.
#'
#' @param x,object A `simple_bls` object from [cbls_reg()].
#' @param type `"xy"` (default) for the BLS line in the plot of `y` against
#'   `x`, or `"md"` for the CBLS line in the Bland-Altman plot (differences
#'   `x - y` against means).
#' @param newdata Values at which to predict: of `x` for `type = "xy"`, or of
#'   the mean of the two methods for `type = "md"`. A numeric vector, or a
#'   data frame with a column named as `x` (or `avg`). If omitted, the
#'   subjects' values are used.
#' @param interval `"none"` (default), `"confidence"` (for the line), or
#'   `"prediction"` (for a new measurement by `y`, or a new difference, given
#'   a new measurement by `x`, or a new mean).
#' @param level Level of the intervals. Default is the `conf.level` of the fit
#'   for confidence intervals and its `pred_level` for prediction intervals.
#' @param reps_x,reps_y For prediction intervals: the new values are means of
#'   this many replicates of each method. Default is 1.
#' @param se.fit For `predict()`: if TRUE, also return the standard errors of
#'   the fitted line.
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
#' fit <- cbls_reg(y ~ x, data = reps, id = "id")
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
  cat("\nBLS, y on x (H0: intercept = 0, slope = 1):\n")
  print(object$model_table, digits = 4)
  cat("\nCBLS, Bland-Altman plot, x - y on mean (H0: intercept = 0, slope = 0):\n")
  print(object$cbls$model_table, digits = 4)
  cat(sprintf("\n%d degrees of freedom\n", object$df.residual))
  if (e$known) {
    fmt_df <- function(d) if (is.finite(d)) format(d) else "known"
    cat(sprintf("Measurement error variances (single measurement): x %.4g (df %s), y %.4g (df %s); %s\n",
                e$var_x, fmt_df(e$df_var_x), e$var_y, fmt_df(e$df_var_y),
                switch(e$source,
                       known = "given",
                       replicates = "estimated from the replicates",
                       "given / estimated from the replicates")))
  } else {
    cat("Measurement error variances: only their ratio is given; ",
        "prediction intervals are not available\n", sep = "")
  }
  cat(sprintf("Error variance ratio var(x)/var(y): %.4f\n", object$error.ratio))
  invisible(object)
}

#' @rdname simple_bls-methods
#' @method predict simple_bls
#' @export
predict.simple_bls <- function(object,
                               newdata = NULL,
                               type = c("xy", "md"),
                               interval = c("none", "confidence", "prediction"),
                               level = NULL,
                               reps_x = 1,
                               reps_y = 1,
                               se.fit = FALSE,
                               ...) {
  type <- match.arg(type)
  interval <- match.arg(interval)
  if (is.null(level)) {
    level <- if (interval == "prediction") object$pred_level else object$conf.level
  }
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
  } else {
    e <- object$errors
    if (!e$known) {
      stop("Prediction intervals need the measurement error variances; ",
           "only their ratio was given. Refit with var_x and var_y, or with ",
           "replicates.")
    }
    if (!is.numeric(reps_x) || !is.numeric(reps_y) || reps_x < 1 || reps_y < 1) {
      stop("reps_x and reps_y must be at least 1.")
    }
    vx <- e$var_x / reps_x
    vy <- e$var_y / reps_y
    b1 <- unname(cf[2])
    # the error variance of a new point, as a_x vx + a_y vy
    if (type == "xy") {
      a_x <- b1^2
      a_y <- 1
    } else {
      # D = x - y, M = (x + y) / 2: var(D - g1 M error) =
      # vx (1 - g1/2)^2 + vy (1 + g1/2)^2
      a_x <- (1 - b1 / 2)^2
      a_y <- (1 + b1 / 2)^2
    }
    var_pred <- var_line + a_x * vx + a_y * vy
    df_pred <- var_pred^2 /
      (var_line^2 / df_res + (a_x * vx)^2 / e$df_var_x +
         (a_y * vy)^2 / e$df_var_y)
    q <- stats::qt(1 - (1 - level) / 2, df_pred)
    half <- q * sqrt(var_pred)
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
                            interval = c("none", "confidence", "prediction"),
                            level = NULL,
                            x_name = NULL,
                            y_name = NULL,
                            n_points = 100,
                            ...) {
  type <- match.arg(type)
  interval <- match.arg(interval)
  if (is.null(level)) {
    level <- if (interval == "prediction") x$pred_level else x$conf.level
  }
  if (is.null(x_name)) {
    x_name <- attr(x$terms, "term.labels")[1]
  }
  if (is.null(y_name)) {
    y_name <- deparse(attr(x$terms, "variables")[[2]])
  }
  df <- .bls_model(x)
  if (type == "xy") {
    pd <- data.frame(u = df$x, v = df$y)
    cf <- x$coefficients
    ref <- geom_abline(intercept = 0, slope = 1, colour = "black")
    labs_xy <- labs(x = paste0("Method: ", x_name),
                    y = paste0("Method: ", y_name))
  } else {
    pd <- data.frame(u = (df$x + df$y) / 2, v = df$x - df$y)
    cf <- x$cbls$coefficients
    ref <- geom_hline(yintercept = 0, colour = "black")
    labs_xy <- labs(x = "Average of Both Methods",
                    y = paste0("Difference between Methods (", x_name,
                               " - ", y_name, ")"))
  }
  p <- ggplot(pd, aes(x = u, y = v)) +
    ref +
    geom_point(na.rm = TRUE) +
    geom_abline(intercept = cf[1], slope = cf[2], linetype = "dashed",
                colour = "red") +
    labs_xy +
    theme_bw()
  band <- NULL
  if (interval != "none") {
    u_seq <- seq(min(pd$u), max(pd$u), length.out = n_points)
    band <- predict(x, newdata = u_seq, type = type, interval = interval,
                    level = level)
    band$u <- u_seq
    p <- p +
      geom_ribbon(data = band, aes(x = u, ymin = lwr, ymax = upr),
                  fill = "red", alpha = 0.2, inherit.aes = FALSE) +
      labs(caption = sprintf("%.0f%% %s interval shown", level * 100,
                             ifelse(interval == "confidence", "confidence",
                                    "prediction")))
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
  e / sqrt(df3$var_y + b1^2 * df3$var_x)
}

#' @rdname simple_bls-methods
#' @method check simple_bls
#' @export
check.simple_bls <- function(x, ...) {
  df3 <- .bls_model(x)
  b0 <- unname(x$coefficients[1])
  b1 <- unname(x$coefficients[2])
  e <- df3$y - (b0 + b1 * df3$x)
  s2 <- df3$var_y + b1^2 * df3$var_x
  r <- e / sqrt(s2)
  # estimated true values
  x_hat <- df3$x + b1 * df3$var_x * e / s2
  y_hat <- df3$y - df3$var_y * e / s2
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

# subject-level data stored by cbls_reg() (with the error variances of the
# means, which cannot be rebuilt from the raw data and formula alone)
.bls_model <- function(object) {
  if (is.null(object$model)) {
    stop("The subject-level data were not stored (model = FALSE); refit ",
         "cbls_reg() with model = TRUE.")
  }
  object$model
}
