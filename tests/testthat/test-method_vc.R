# MethComp uses a multiplier of 2 for the limits of agreement
pred_2sd = 2 * pnorm(2) - 1

fixture = function(name) {
  test_path("fixtures", paste0(name, ".rds"))
}

expect_close = function(object, expected, tol = 0.002) {
  expect_true(all(abs(object - expected) <= tol),
              info = paste0("got ", paste(signif(object, 6), collapse = ", "),
                            "; expected ",
                            paste(expected, collapse = ", ")))
}

# reference values: Carstensen (2010) / MethComp ----
test_that("oximetry, linked replicates, reproduces MethComp BA.est", {
  skip_if_not(file.exists(fixture("ox")))
  ox = readRDS(fixture("ox"))
  fit = method_vc(ox, y = "y", method = "meth", id = "item",
                  replicate = "repl", replicates = "linked",
                  pred_level = pred_2sd)
  expect_s3_class(fit, "simple_mvc")
  expect_equal(fit$design$model_type, "reps")

  expect_close(fit$bias$bias, 2.470)
  expect_close(fit$limits$sd, 6.169)
  expect_close(c(fit$limits$lower_loa, fit$limits$upper_loa),
               c(-9.867, 14.808))

  vc = fit$varcomp
  expect_close(vc$sd[vc$component == "omega"], 3.416)
  expect_close(vc$sd[vc$component == "tau"], 2.928)
  sig = vc$sd[vc$component == "sigma"]
  expect_close(sig, c(2.225, 3.994))
  # within-method prediction SD (MethComp's m-to-m conversion SD)
  expect_close(sqrt(2) * sig, c(3.146, 5.649))

  # containment df: (items - 1) * (methods - 1)
  expect_equal(fit$bias$df, 60)
  expect_true(all(is.finite(c(vc$lower, vc$upper, vc$df))))
  expect_true(all(vc$lower < vc$sd & vc$sd < vc$upper))
})

test_that("oximetry on the pctlogit scale reproduces the book (ch. 8)", {
  skip_if_not(file.exists(fixture("ox")))
  ox = readRDS(fixture("ox"))
  fit = method_vc(ox, y = "y", method = "meth", id = "item",
                  replicate = "repl", replicates = "linked",
                  transform = "pctlogit", pred_level = pred_2sd)
  # the book reports pulse - CO
  expect_close(-fit$bias$bias, -0.1564, tol = 0.0005)
  expect_close(fit$limits$sd, 0.327)
  expect_close(c(-fit$limits$upper_loa, -fit$limits$lower_loa),
               c(-0.811, 0.498))
  vc = fit$varcomp
  expect_close(vc$sd[vc$component == "tau"], 0.157)
  expect_close(vc$sd[vc$component == "omega"], 0.221)
  expect_close(vc$sd[vc$component == "sigma"], c(0.160, 0.179))
  expect_equal(fit$transform$name, "pctlogit")
  expect_equal(fit$data$y_t, qlogis(fit$data$y / 100))
})

test_that("fat data, exchangeable replicates, agrees with the book (ch. 5)", {
  skip_if_not(file.exists(fixture("fat")))
  fat = readRDS(fixture("fat"))
  fit = method_vc(fat, y = "Sub", method = "Obs", id = "Id",
                  replicate = "Rep", pred_level = pred_2sd)
  expect_equal(fit$design$replicates, "exchangeable")
  expect_close(fit$bias$bias, 0.045, tol = 0.001)
  expect_close(fit$limits$sd, 0.135, tol = 0.001)
  # empirical limits KL - SL: (-0.225, 0.315)
  expect_close(c(fit$limits$lower_loa, fit$limits$upper_loa),
               c(-0.225, 0.315), tol = 0.005)
  # within-item SDs (loose check)
  expect_close(fit$varcomp$sd[fit$varcomp$component == "sigma"],
               c(0.077, 0.072), tol = 0.005)
})

# equivalence with existing functions ----
equiv_data = function() {
  set.seed(1)
  data.frame(x = rnorm(30, 10), y = rnorm(30, 10.3))
}

test_that("two methods, one measurement: equals agreement_limit()", {
  d = equiv_data()
  dl = mc_long(d, methods = c("x", "y"))
  for (bt in c("iu", "joint")) {
    a = agreement_limit(x = "x", y = "y", data = d, bound_type = bt)$loa
    fit = method_vc(dl, y = "y", method = "method", id = "id",
                    bound_type = bt)
    m = fit$limits
    expect_equal(fit$design$model_type, "single_2")
    expect_equal(m$bias, a$bias, tolerance = 1e-6)
    expect_equal(m$sd, a$sd_delta, tolerance = 1e-6)
    expect_equal(m$df_sd, 29)
    expect_equal(c(m$lower_loa, m$upper_loa),
                 c(a$lower_loa, a$upper_loa), tolerance = 1e-6)
    expect_equal(c(m$lower_loa_ci, m$upper_loa_ci),
                 c(a$lower_loa_ci, a$upper_loa_ci), tolerance = 1e-6)
    expect_equal(c(fit$bias$lower.CL, fit$bias$upper.CL),
                 c(a$lower.CL, a$upper.CL), tolerance = 1e-6)
  }
})

test_that("two methods, one measurement: equals tolerance_limit()", {
  d = equiv_data()
  dl = mc_long(d, methods = c("x", "y"))
  m = list()
  for (bt in c("iu", "joint")) {
    tl = tolerance_limit(data = d, x = "x", y = "y", bound_type = bt)$limits
    m[[bt]] = method_vc(dl, y = "y", method = "method", id = "id",
                        interval = "tolerance", bound_type = bt)$limits
    expect_equal(c(m[[bt]]$lower.PL, m[[bt]]$upper.PL),
                 c(tl$lower.PL, tl$upper.PL), tolerance = 1e-6)
    expect_equal(m[[bt]]$sep, tl$SEP, tolerance = 1e-6)
    expect_equal(c(m[[bt]]$lower.TL, m[[bt]]$upper.TL),
                 c(tl$lower.TL, tl$upper.TL), tolerance = 1e-6)
  }
  # bound_type affects the tolerance limits only
  expect_equal(m$iu[c("lower.PL", "upper.PL")],
               m$joint[c("lower.PL", "upper.PL")])
  expect_false(isTRUE(all.equal(m$iu$lower.TL, m$joint$lower.TL)))
  # tolerance defaults to joint bounds
  fit = method_vc(dl, y = "y", method = "method", id = "id",
                  interval = "tolerance")
  expect_equal(fit$design$bound_type, "joint")
})

test_that("log transform equals agreement_limit(log_tf = TRUE)", {
  d = equiv_data()
  dl = mc_long(d, methods = c("x", "y"))
  a = agreement_limit(x = "x", y = "y", data = d, log_tf = TRUE)$loa
  m = method_vc(dl, y = "y", method = "method", id = "id",
                transform = "log")$limits
  expect_equal(m$bias, a$bias, tolerance = 1e-6)
  expect_equal(c(m$lower_loa, m$upper_loa, m$lower_loa_ci, m$upper_loa_ci),
               c(a$lower_loa, a$upper_loa, a$lower_loa_ci, a$upper_loa_ci),
               tolerance = 1e-6)
})

# structure and behavior ----
test_that("estimates recover the simulation truth (3 methods, linked)", {
  dat = sim_methods(n_items = 300, n_reps = 3, methods = c("A", "B", "C"),
                    alpha = c(0, 1, -0.5), tau = 0.4, omega = 0.3,
                    sigma = c(0.3, 0.5, 0.7), replicates = "linked",
                    seed = 11)
  truth = attr(dat, "truth")
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked")
  expect_equal(fit$bias$bias, truth$pairs$bias, tolerance = 0.1)
  expect_equal(fit$limits$sd, truth$pairs$sd, tolerance = 0.06)
  vc = fit$varcomp
  expect_equal(vc$sd[vc$component == "sigma"], c(0.3, 0.5, 0.7),
               tolerance = 0.06)
  expect_equal(vc$sd[vc$component == "omega"], 0.3, tolerance = 0.15)
  expect_equal(vc$sd[vc$component == "tau"], 0.4, tolerance = 0.1)
  expect_equal(nrow(fit$bias), 3)
  expect_equal(fit$bias$df, rep(2 * 299, 3))
})

test_that("method-specific tau and include_omega", {
  dat = sim_methods(n_items = 60, n_reps = 2, methods = c("A", "B", "C"),
                    tau = c(0.2, 0.4, 0.6), omega = 0.5, sigma = 0.3,
                    replicates = "linked", seed = 5)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked", tau = "method")
  vc = fit$varcomp
  expect_equal(sum(vc$component == "tau"), 3)
  expect_equal(vc$method[vc$component == "tau"], c("A", "B", "C"))
  v = setNames(vc$var, ifelse(is.na(vc$method), vc$component,
                              paste0(vc$component, vc$method)))
  expect_equal(fit$limits$sd[1],
               sqrt(v[["tauA"]] + v[["tauB"]] + v[["sigmaA"]] +
                      v[["sigmaB"]]))

  fit_o = method_vc(dat, y = "y", method = "method", id = "id",
                    replicate = "repl", replicates = "linked",
                    tau = "method", include_omega = TRUE)
  expect_equal(fit_o$limits$sd^2, fit$limits$sd^2 + 2 * v[["omega"]],
               tolerance = 1e-8)
  expect_true(all(fit_o$limits$df_sd > 0))

  expect_message(
    method_vc(dat, y = "y", method = "method", id = "id",
              replicate = "repl", include_omega = TRUE),
    "no effect"
  )
})

test_that("single measurement with three methods gives Grubbs-type SDs", {
  dat = sim_methods(n_items = 400, methods = c("A", "B", "C"),
                    sigma = c(0.3, 0.6, 0.9), seed = 8)
  expect_message(
    fit <- method_vc(dat, y = "y", method = "method", id = "id"),
    "absorbed"
  )
  expect_equal(fit$design$model_type, "single_m")
  expect_equal(fit$varcomp$component, rep("sigma", 3))
  expect_equal(fit$varcomp$sd, c(0.3, 0.6, 0.9), tolerance = 0.12)
  expect_equal(fit$bias$df, rep(2 * 399, 3))
  expect_true(all(is.finite(fit$limits$lower_loa_ci)))
})

test_that("exchangeable results do not depend on replicate labels", {
  dat = sim_methods(n_items = 25, n_reps = 3, methods = c("A", "B"),
                    tau = 0.5, sigma = c(0.4, 0.6), seed = 21)
  fit1 = method_vc(dat, y = "y", method = "method", id = "id",
                   replicate = "repl")
  set.seed(3)
  dat2 = dat %>%
    dplyr::group_by(method, id) %>%
    dplyr::mutate(repl = sample(repl)) %>%
    dplyr::ungroup()
  fit2 = method_vc(dat2, y = "y", method = "method", id = "id",
                   replicate = "repl")
  # point estimates are identical; intervals use apVar, a numerical Hessian
  expect_equal(fit1$limits$sd, fit2$limits$sd, tolerance = 1e-6)
  expect_equal(fit1$limits$lower_loa, fit2$limits$lower_loa, tolerance = 1e-6)
  expect_equal(fit1$limits, fit2$limits, tolerance = 1e-3)
  expect_equal(fit1$varcomp$sd, fit2$varcomp$sd, tolerance = 1e-6)
  expect_equal(fit1$varcomp, fit2$varcomp, tolerance = 1e-3)
})

test_that("reversing the method order flips the bias and swaps limits", {
  dat = sim_methods(n_items = 30, n_reps = 2, methods = c("A", "B"),
                    alpha = c(0, 0.5), tau = 0.3, sigma = 0.4, seed = 2)
  fit1 = method_vc(dat, y = "y", method = "method", id = "id",
                   replicate = "repl")
  dat$method = factor(dat$method, levels = c("B", "A"))
  fit2 = method_vc(dat, y = "y", method = "method", id = "id",
                   replicate = "repl")
  expect_equal(fit2$bias$method1, "B")
  expect_equal(fit2$bias$bias, -fit1$bias$bias, tolerance = 1e-6)
  expect_equal(fit2$limits$lower_loa, -fit1$limits$upper_loa,
               tolerance = 1e-6)
  expect_equal(fit2$limits$upper_loa_ci, -fit1$limits$lower_loa_ci,
               tolerance = 1e-4)
})

test_that("identifiability and input errors", {
  dat = sim_methods(n_items = 20, n_reps = 2, tau = 0.3, seed = 1)
  expect_error(method_vc(dat, y = "y", method = "method", id = "id",
                         replicate = "repl", tau = "method"),
               "not identifiable with two methods")
  expect_error(method_vc(dat, y = "y", method = "method", id = "id",
                         replicates = "linked"),
               "requires `replicate`")
  expect_error(method_vc(dat, y = "y", method = "method", id = "id",
                         replicate = "repl", pred_level = 1.2),
               "pred_level")
  expect_error(method_vc(dat, y = "y", method = "method", id = "id",
                         replicate = "repl", bound_type = "both"))
  expect_error(method_vc(dat, y = "y", method = "method", id = "id",
                         replicate = "repl", include_omega = NA),
               "include_omega")

  # one method without replicates
  partial = dat[!(dat$method == "B" & dat$repl == 2), ]
  expect_error(method_vc(partial, y = "y", method = "method", id = "id",
                         replicate = "repl"),
               "no replicate")
})

test_that("design warnings and messages", {
  # few replicate pairs
  dat = sim_methods(n_items = 20, n_reps = 1, methods = c("A", "B", "C"),
                    seed = 4)
  extra = dat[dat$id %in% 1:2, ]
  extra$repl = 2
  extra$y = extra$y + 0.1
  expect_warning(
    method_vc(rbind(dat, extra), y = "y", method = "method", id = "id",
              replicate = "repl"),
    "Fewer than 3 replicate pairs"
  )
  # linked without replicates
  dat2 = sim_methods(n_items = 20, seed = 4)
  expect_message(
    method_vc(dat2, y = "y", method = "method", id = "id",
              replicate = "repl", replicates = "linked"),
    "has no effect"
  )
})

test_that("unavailable apVar gives NA intervals with a warning", {
  dat = sim_methods(n_items = 20, n_reps = 2, tau = 0.3, seed = 1)
  df = mc_data_org(dat, y = "y", method = "method", id = "id",
                   replicate = "repl")
  des = mc_vc_design(df, replicates = "exchangeable", tau = "common")
  fit = mc_vc_fit(df, des, control = nlme::lmeControl(returnObject = TRUE))
  lv = levels(df$method)
  vc = mc_vc_extract(fit, des, lv)
  fit$apVar = "Non-positive definite approximate variance-covariance"
  expect_warning(res <- mc_vc_delta(fit, des, vc, lv, 0.95),
                 "unavailable")
  expect_true(all(is.na(res$varcomp$lower)))
  expect_true(all(is.na(res$cov)))
})

test_that("boundary estimates are flagged", {
  vc = dplyr::tibble(component = c("tau", "sigma", "sigma"),
                     method = c(NA, "A", "B"),
                     sd = c(1e-7, 0.5, 0.6),
                     var = c(1e-14, 0.25, 0.36))
  expect_warning(mc_vc_boundary(vc), "near zero")
  vc$sd[1] = 0.1
  expect_silent(mc_vc_boundary(vc))
})

test_that("delta method components match the model", {
  dat = sim_methods(n_items = 40, n_reps = 3, methods = c("A", "B"),
                    tau = 0.4, sigma = c(0.3, 0.5), seed = 9)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl")
  vc = fit$varcomp
  expect_equal(dim(fit$vc_cov), c(3, 3))
  # component df = 2 v^2 / var(v)
  expect_equal(vc$df, 2 * vc$var^2 / diag(fit$vc_cov))
  # SD df: Satterthwaite of 2 tau^2 + sigma_A^2 + sigma_B^2
  w = c(2, 1, 1)
  s2 = sum(w * vc$var)
  expect_equal(fit$limits$df_sd,
               2 * s2^2 / drop(t(w) %*% fit$vc_cov %*% w))
})

test_that("keep_model and the stored call", {
  dat = sim_methods(n_items = 20, n_reps = 2, tau = 0.3, seed = 1)
  lvl = 0.9
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", pred_level = lvl, keep_model = FALSE)
  expect_null(fit$model)
  expect_equal(fit$call$pred_level, 0.9)
  expect_equal(fit$call$transform, "none")

  fit2 = method_vc(dat, y = "y", method = "method", id = "id",
                   replicate = "repl")
  expect_s3_class(fit2$model, "lme")
  # self-contained model
  expect_s3_class(nlme::getData(fit2$model), "data.frame")
  expect_s3_class(stats::update(fit2$model), "lme")
})

# repeatability ----
test_that("oximetry repeatability coefficients match MethComp", {
  skip_if_not(file.exists(fixture("ox")))
  ox = readRDS(fixture("ox"))
  fit = method_vc(ox, y = "y", method = "meth", id = "item",
                  replicate = "repl", replicates = "linked",
                  pred_level = pred_2sd)
  rp = fit$repeatability
  expect_equal(rp$method, c("CO", "pulse"))
  # MethComp RepCoef: 2 * sqrt(2) * sigma
  expect_close(rp$rc, 2 * c(3.146, 5.649), tol = 0.005)
  sig = fit$varcomp$sd[fit$varcomp$component == "sigma"]
  om = fit$varcomp$sd[fit$varcomp$component == "omega"]
  expect_equal(rp$rpc, 2 * sqrt(2) * sqrt(sig^2 + om^2))
  expect_true(all(rp$rc_lower < rp$rc & rp$rc < rp$rc_upper))
  expect_true(all(rp$rpc_lower < rp$rpc & rp$rpc < rp$rpc_upper))
  # RC CI is the sigma CI scaled
  expect_equal(rp$rc_lower / rp$rc,
               fit$varcomp$lower[fit$varcomp$component == "sigma"] / sig)
})

test_that("repeatability is only reported with replicates", {
  dat = sim_methods(n_items = 30, methods = c("A", "B"), seed = 1)
  fit = method_vc(dat, y = "y", method = "method", id = "id")
  expect_null(fit$repeatability)
  dat2 = sim_methods(n_items = 30, n_reps = 2, tau = 0.3, seed = 1)
  fit2 = method_vc(dat2, y = "y", method = "method", id = "id",
                   replicate = "repl")
  expect_named(fit2$repeatability,
               c("method", "sigma", "rc", "rc_lower", "rc_upper"))
})

test_that("prediction limits lie within the tolerance limits", {
  dat = sim_methods(n_items = 30, n_reps = 3, methods = c("A", "B", "C"),
                    alpha = c(0, 0.5, -0.2), tau = 0.3, omega = 0.4,
                    sigma = c(0.3, 0.5, 0.4), replicates = "linked",
                    seed = 1)
  for (bt in c("iu", "joint")) {
    fit = method_vc(dat, y = "y", method = "method", id = "id",
                    replicate = "repl", replicates = "linked",
                    interval = "tolerance", bound_type = bt)
    l = fit$limits
    expect_equal(l$sep^2, l$sd^2 + fit$bias$se^2)
    expect_true(all(l$lower.TL < l$lower.PL & l$lower.PL < l$bias &
                      l$bias < l$upper.PL & l$upper.PL < l$upper.TL))
    t_p = qt(0.975, l$df_sd)
    expect_equal(l$upper.PL - l$bias, t_p * l$sep)
  }
})

# limit bounds: Satterthwaite vs MOVER over components ----
test_that("component MOVER option of mc_limits", {
  dat = sim_methods(n_items = 40, n_reps = 3, methods = c("A", "B", "C"),
                    tau = 0.4, omega = 0.3, sigma = c(0.3, 0.5, 0.4),
                    replicates = "linked", seed = 6)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked")
  args = list(bias_tab = fit$bias, varcomp = fit$varcomp,
              cov_v = fit$vc_cov, design = fit$design,
              interval = "agreement", pred_level = 0.95, conf_level = 0.95,
              bound_type = "iu", include_omega = FALSE)
  sat = do.call(mc_limits, args)
  comp = do.call(mc_limits, c(args, sd_df = "components"))
  expect_equal(sat, fit$limits)
  expect_equal(comp$lower_loa, sat$lower_loa)
  expect_false(isTRUE(all.equal(comp$lower_loa_ci, sat$lower_loa_ci)))
  expect_true(all(comp$lower_loa_ci < comp$lower_loa))

  # identical for the paired-difference model
  dat2 = sim_methods(n_items = 30, methods = c("A", "B"), seed = 1)
  fit2 = method_vc(dat2, y = "y", method = "method", id = "id")
  args2 = list(bias_tab = fit2$bias, varcomp = fit2$varcomp,
               cov_v = fit2$vc_cov, design = fit2$design,
               interval = "tolerance", pred_level = 0.95, conf_level = 0.95,
               bound_type = "joint", include_omega = FALSE)
  expect_equal(do.call(mc_limits, args2),
               do.call(mc_limits, c(args2, sd_df = "components")))
})

test_that("diagnostics are stored in the object", {
  dat = sim_methods(n_items = 20, n_reps = 2, methods = c("A", "B"),
                    tau = 0.3, omega = 0.3, replicates = "linked", seed = 1)
  fit = method_vc(dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked",
                  keep_model = FALSE)
  dg = fit$diagnostics
  expect_equal(nrow(dg$resid), nrow(fit$data))
  expect_setequal(unique(dg$ranef$effect),
                  c("Method-by-item", "Item-by-replicate"))
  dat1 = sim_methods(n_items = 20, methods = c("A", "B"), seed = 1)
  fit1 = method_vc(dat1, y = "y", method = "method", id = "id")
  expect_null(fit1$diagnostics$ranef)
})
