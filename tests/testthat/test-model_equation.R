testthat::local_edition(3)

# helpers ----
expect_balanced_latex <- function(eq) {
  ch <- strsplit(gsub("\\\\[{}]", "", eq), "")[[1]]
  expect_equal(sum(ch == "{"), sum(ch == "}"))
  count <- function(p) lengths(regmatches(eq, gregexpr(p, eq, fixed = TRUE)))
  expect_equal(count("\\left"), count("\\right"))
  expect_equal(count("\\begin{"), count("\\end{"))
  expect_false(grepl("+ -", eq, fixed = TRUE))
}
n_lines <- function(eq) length(strsplit(eq, " \\\\\n", fixed = TRUE)[[1]])
has <- function(eq, x) expect_true(grepl(x, eq, fixed = TRUE), label = x)

sf <- data.frame(J1 = c(9, 6, 8, 7, 10, 6), J2 = c(2, 1, 4, 1, 5, 2),
                 J3 = c(5, 3, 6, 2, 6, 4), J4 = c(8, 2, 8, 6, 9, 7))
data("temps", package = "SimplyAgree")
data("reps", package = "SimplyAgree")

# LaTeX helpers ----
test_that("names and numbers are formatted for LaTeX", {
  expect_equal(SimplyAgree:::.eq_name("trec_pre"), "\\mathrm{trec\\_pre}")
  expect_equal(SimplyAgree:::.eq_name("a b%"), "\\mathrm{a\\ b\\%}")
  expect_equal(SimplyAgree:::.eq_name("x^2~"), "\\mathrm{x2}")
  expect_equal(SimplyAgree:::.eq_num(1.23456, 2), "1.23")
  expect_equal(SimplyAgree:::.eq_num(-0.0001, 3), "-1.00 \\times 10^{-4}")
  expect_equal(SimplyAgree:::.eq_num(0, 2), "0.00")
  expect_equal(SimplyAgree:::.eq_num(NA), "\\mathrm{NA}")
  expect_equal(SimplyAgree:::.eq_base(-0.5, 1), "(-0.5)")
  expect_equal(SimplyAgree:::.eq_base(0.5, 1), "0.5")
})

test_that("sums handle signs when estimates are substituted", {
  s <- SimplyAgree:::.eq_sum(list(
    SimplyAgree:::.eq_term(SimplyAgree:::.eq_par("a", -1)),
    SimplyAgree:::.eq_term(SimplyAgree:::.eq_par("b", -2), "x"),
    SimplyAgree:::.eq_term(NULL, "e")))
  opt <- list(use_coefs = TRUE, digits = 1)
  expect_equal(SimplyAgree:::.eq_piece(s, opt), "-1.0 - 2.0\\,x + e")
  opt$use_coefs <- FALSE
  expect_equal(SimplyAgree:::.eq_piece(s, opt), "a + b\\,x + e")
})

# lm and lmer ----
test_that("lm: fixed effects, factors, interactions and estimates", {
  fit <- lm(mpg ~ wt * am + factor(cyl), data = mtcars)
  eq <- model_equation(fit)
  expect_s3_class(eq, "model_equation")
  expect_balanced_latex(eq)
  has(eq, "\\mathrm{mpg}_{i} &= \\beta_{0} + \\beta_{1}\\,\\mathrm{wt}_{i}")
  has(eq, "\\mathrm{I}(\\mathrm{cyl}_{i} = \\mathrm{6})")
  has(eq, "\\mathrm{wt}_{i} \\times \\mathrm{am}_{i}")
  has(eq, "\\varepsilon_{i} &\\sim N(0,\\, \\sigma^{2})")
  expect_equal(n_lines(eq), 2)

  eq2 <- model_equation(fit, use_coefs = TRUE)
  expect_balanced_latex(eq2)
  has(eq2, formatC(coef(fit)[[1]], format = "f", digits = 3))
  has(eq2, paste0("- ", formatC(-coef(fit)[["wt"]], format = "f", digits = 3)))
  has(eq2, paste0(formatC(sigma(fit), format = "f", digits = 3), "^{2}"))
  expect_false(identical(eq2, model_equation(fit, use_coefs = TRUE,
                                             coef_digits = 1)))
})

test_that("lm: no intercept, transformed response, many levels, weights", {
  eq <- model_equation(lm(log(mpg) ~ 0 + factor(cyl), data = mtcars))
  has(eq, "\\log \\mathrm{mpg}_{i} &= \\beta_{1}\\,")
  expect_false(grepl("beta_{0}", eq, fixed = TRUE))

  eq <- model_equation(lm(mpg ~ wt + factor(carb), data = mtcars),
                       use_coefs = TRUE)
  has(eq, "\\gamma_{\\mathrm{carb}[i]}")

  eq <- model_equation(lm(mpg ~ wt, data = mtcars, weights = hp))
  has(eq, "\\sigma^{2}/w_{i}")

  has(model_equation(lm(sqrt(mpg) ~ wt, data = mtcars)),
      "\\sqrt{\\mathrm{mpg}_{i}} &=")
  has(model_equation(lm(log10(mpg) ~ wt, data = mtcars)),
      "\\log_{10} \\mathrm{mpg}_{i} &=")
  has(model_equation(lm(I(mpg / 2) ~ wt, data = mtcars)),
      "\\mathrm{I(mpg/2)}_{i} &=")
  has(model_equation(lm(mpg ~ wt, data = data.frame(mpg = mtcars$mpg,
                                                   wt = mtcars$wt))),
      "\\mathrm{mpg}_{i} &=")
  expect_equal(SimplyAgree:::.eq_name("a b"), "\\mathrm{a\\ b}")
})

test_that("unsupported models give informative errors", {
  expect_error(model_equation(glm(am ~ wt, data = mtcars, family = binomial)),
               "linear models")
  expect_error(model_equation(1:3), "no method")
  fit <- lm(mpg ~ wt, data = mtcars)
  expect_error(model_equation(fit, use_coefs = NA), "use_coefs")
  expect_error(model_equation(fit, coef_digits = 1.5), "coef_digits")
})

test_that("lmer: random intercepts and correlated random slopes", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  eq <- model_equation(fit, use_coefs = TRUE)
  expect_balanced_latex(eq)
  has(eq, "+ u_{j[i]} +")
  has(eq, "u_{j} &\\sim N(0,\\, ")

  fit2 <- lme4::lmer(Reaction ~ Days + (Days | Subject),
                     data = lme4::sleepstudy)
  eq <- model_equation(fit2)
  has(eq, "u_{1j[i]}\\,\\mathrm{Days}_{i}")
  has(eq, "\\boldsymbol{\\Sigma}_{u}")
  eq <- model_equation(fit2, use_coefs = TRUE)
  expect_balanced_latex(eq)
  has(eq, "\\begin{pmatrix} 612.")
})

# gls and lme ----
test_that("gls and lme: variance functions and correlation structures", {
  o <- nlme::Orthodont
  eq <- model_equation(nlme::gls(distance ~ age, data = o,
                                 weights = nlme::varPower()),
                       use_coefs = TRUE)
  expect_balanced_latex(eq)
  has(eq, "\\left|\\hat{\\mu}_{i}\\right|^{2(")

  eq <- model_equation(nlme::gls(distance ~ age, data = o,
                                 weights = nlme::varExp(form = ~ age)))
  has(eq, "\\exp(2\\delta\\mathrm{age}_{i})")

  eq <- model_equation(nlme::gls(distance ~ age, data = o,
                                 weights = nlme::varFixed(~ age)))
  has(eq, "\\sigma^{2}\\mathrm{age}_{i})")

  eq <- model_equation(nlme::gls(distance ~ age, data = o,
                                 weights = nlme::varPower(form = ~ age | Sex)))
  has(eq, "g^{2}_{i}")

  o$older <- ifelse(o$age > 10, "yes", "no")
  eq <- model_equation(nlme::gls(distance ~ age, data = o,
                                 weights = nlme::varIdent(form = ~ 1 | Sex * older)))
  has(eq, "\\delta^{2}_{\\mathrm{Sex*older}_{i}}")

  eq <- model_equation(nlme::gls(distance ~ age, data = o,
                                 correlation = nlme::corAR1(form = ~ 1 | Subject)))
  has(eq, "\\phi^{|i - i'|}, \\quad i \\neq i',\\ \\text{same } \\mathrm{Subject}")

  eq <- model_equation(nlme::gls(distance ~ age, data = o,
                                 correlation = nlme::corSymm(form = ~ 1 | Subject)))
  has(eq, "\\text{corSymm}")

  eq <- model_equation(nlme::lme(distance ~ age, random = ~ age | Subject,
                                 data = o))
  has(eq, "\\boldsymbol{\\Sigma}_{u}")
  eq <- model_equation(nlme::lme(distance ~ age,
                                 random = list(Subject = nlme::pdDiag(~ age)),
                                 data = o), use_coefs = TRUE)
  expect_balanced_latex(eq)
  has(eq, "u_{0j} &\\sim N(0,\\, ")
  has(eq, "u_{1j} \\sim N(0,\\, ")
  eq <- model_equation(nlme::lme(distance ~ age,
                                 random = list(Subject = nlme::pdIdent(~ age)),
                                 data = o))
  has(eq, "u_{0j}, u_{1j} &\\sim N(0,\\, \\sigma^{2}_{u})")
  expect_error(model_equation(nlme::lme(
    distance ~ age, data = o,
    random = list(Subject = nlme::pdBlocked(list(~ 1, ~ age - 1))))),
    "pdBlocked")
})

# tolerance_limit ----
test_that("tolerance_limit: gls with condition and compound symmetry", {
  tl <- tolerance_limit(temps, x = "trec_pre", y = "teso_pre", id = "id",
                        condition = "tod")
  eq <- model_equation(tl)
  expect_balanced_latex(eq)
  has(eq, "d_{ij} &= \\beta_{0} + \\beta_{1}\\,\\mathrm{I}(\\mathrm{tod}_{ij} = \\mathrm{PM}) + e_{ij}")
  has(eq, "\\sigma^{2}\\delta^{2}_{\\mathrm{tod}_{ij}}\\right), \\quad \\delta_{\\mathrm{AM}} = 1")
  has(eq, "\\operatorname{Cor}(e_{ij}, e_{ij'}) &= \\rho, \\quad j \\neq j'")
  has(eq, "\\text{where } d_{ij} = \\mathrm{trec\\_pre}_{ij} - \\mathrm{teso\\_pre}_{ij}")
  expect_equal(n_lines(eq), 4)

  eq <- model_equation(tl, use_coefs = TRUE)
  expect_balanced_latex(eq)
  rho <- coef(tl$model$modelStruct$corStruct, unconstrained = FALSE)
  has(eq, paste0("&= ", formatC(rho, format = "f", digits = 3), ","))
  has(eq, "\\delta_{\\mathrm{PM}} = 0.810")
})

test_that("tolerance_limit: no id, prop_bias, log, ar1 with time, lme", {
  eq <- model_equation(tolerance_limit(reps, x = "x", y = "y"))
  has(eq, "d_{i} &= \\beta_{0} + e_{i}")
  expect_false(grepl("Cor", eq, fixed = TRUE))

  eq <- model_equation(tolerance_limit(temps, x = "trec_pre", y = "teso_pre",
                                       id = "id", prop_bias = TRUE,
                                       log_tf = TRUE, cor_type = "ar1"))
  has(eq, "\\beta_{1}\\,A_{ij}")
  has(eq, "\\phi^{|j - j'|}")
  has(eq, "\\log \\mathrm{trec\\_pre}_{ij} - \\log \\mathrm{teso\\_pre}_{ij},\\ A_{ij} = ")

  eq <- model_equation(tolerance_limit(temps, x = "trec_pre", y = "teso_pre",
                                       id = "id", time = "trial_num",
                                       cor_type = "car1"))
  has(eq, "\\phi^{|t_{ij} - t_{ij'}|}")

  tl <- tolerance_limit(temps, x = "trec_pre", y = "teso_pre", id = "id",
                        condition = "tod", model = "lme")
  eq <- model_equation(tl, use_coefs = TRUE)
  expect_balanced_latex(eq)
  has(eq, "+ u_{i} + e_{ij}")
  has(eq, "u_{i} &\\sim N(0,\\, ")
  # the stored model on its own uses the generic notation
  has(model_equation(tl$model), "\\mathrm{delta}_{i} &= \\beta_{0}")
  has(model_equation(tl$model), "u_{j[i]}")
})

test_that("tolerance_limit: nested random intercepts", {
  set.seed(12)
  d <- expand.grid(shot = 1:3, club = paste0("c", 1:3),
                   golfer = paste0("g", 1:8))
  d$x <- 100 + rnorm(nrow(d))
  d$y <- d$x - (0.8 + rnorm(8, 0, 0.7)[as.integer(d$golfer)] + rnorm(nrow(d)))
  tl <- tolerance_limit(d, x = "x", y = "y", id = c("golfer", "club"),
                        model = "lme")
  eq <- model_equation(tl)
  has(eq, "d_{ijk} &= \\beta_{0} + u_{i} + v_{ij} + e_{ijk}")
  has(eq, "u_{i} &\\sim N(0,\\, \\sigma^{2}_{1}), \\quad v_{ij} \\sim N(0,\\, \\sigma^{2}_{2})")
})

test_that("tolerance_limit: the model must be kept", {
  tl <- tolerance_limit(reps, x = "x", y = "y", keep_model = FALSE)
  expect_error(model_equation(tl), "keep_model = TRUE")
})

# reliability ----
test_that("icc_test, reli_err_test and mdc_test: two-way and one-way models", {
  icc <- icc_test(col.names = names(sf), data = sf, method = "reml")
  eq <- model_equation(icc)
  expect_balanced_latex(eq)
  has(eq, "y_{ij} &= \\mu + s_{i} + t_{j} + e_{ij}")
  has(eq, "s_{i} &\\sim N(0,\\, \\sigma^{2}_{s}), \\quad t_{j} \\sim N(0,\\, \\sigma^{2}_{t})")
  has(eq, "e_{ij} &\\sim N(0,\\, \\sigma^{2}_{e})")

  eq <- model_equation(icc, use_coefs = TRUE)
  has(eq, paste0(formatC(icc$fit$effects$mu, format = "f", digits = 3),
                 " + s_{i}"))
  has(eq, paste0(formatC(sqrt(icc$fit$vc[["id"]]), format = "f", digits = 3),
                 "^{2}"))

  eq <- model_equation(icc_test(col.names = names(sf), data = sf), use_coefs = TRUE)
  has(eq, paste0(formatC(mean(as.matrix(sf)), format = "f", digits = 3),
                 " + s_{i}"))

  eq <- model_equation(icc_test(col.names = names(sf), data = sf,
                                model = "oneway"))
  has(eq, "y_{ij} &= \\mu + s_{i} + e_{ij}")
  expect_false(grepl("t_{j}", eq, fixed = TRUE))

  err <- reli_err_test(col.names = names(sf), data = sf, scale = "log")
  has(model_equation(err), "\\log y_{ij} &=")
  has(model_equation(mdc_test(err)), "\\log y_{ij} &=")
})

test_that("reli_stats and reli_aov use the two-way model", {
  rs <- reli_stats(col.names = names(sf), data = sf, wide = TRUE)
  eq <- model_equation(rs, use_coefs = TRUE)
  has(eq, paste0(formatC(sqrt(rs$var_comp["ID", "variance"]), format = "f",
                         digits = 3), "^{2}"))
  ra <- reli_aov(col.names = names(sf), data = sf, wide = TRUE)
  eq <- model_equation(ra, use_coefs = TRUE)
  has(eq, paste0(formatC(mean(as.matrix(sf)), format = "f", digits = 3),
                 " + s_{i}"))
})

test_that("reli_facet_test: nested and crossed designs", {
  set.seed(1)
  d <- expand.grid(rep = 1:2, items = paste0("R", 1:3), id = 1:8)
  d$values <- 20 + rnorm(8)[d$id] + rnorm(nrow(d), 0, 0.5)
  d$id <- factor(d$id)
  ft <- reli_facet_test(values ~ items | id, data = d)
  eq <- model_equation(ft, use_coefs = TRUE)
  expect_balanced_latex(eq)
  has(eq, "y_{pir} &= ")
  has(eq, " + p + i + (pi) + e_{pir}")
  has(model_equation(ft), "(pi) \\sim N(0,\\, \\sigma^{2}_{pi})")
  has(model_equation(mdc_test(reli_facet_test(values ~ items | id, data = d,
                                              stat = "SEM"))),
      "y_{pir} &= \\mu")

  dc <- expand.grid(occ = paste0("d", 1:2), items = paste0("R", 1:3), id = 1:8)
  dc$values <- 20 + rnorm(8)[dc$id] + rnorm(nrow(dc), 0, 0.5)
  dc$id <- factor(dc$id)
  eq <- model_equation(suppressWarnings(
    reli_facet_test(values ~ items | id, data = dc, occasion = "occ",
                    method = "reml")), use_coefs = TRUE)
  expect_balanced_latex(eq)
  has(eq, "+ p + i + o + (pi) + (po) + (io) + e_{pio}")
  expect_equal(n_lines(eq), 4)
})

# method comparison ----
mvc_dat <- sim_methods(n_items = 15, n_reps = 2, methods = c("A", "B", "C"),
                       replicates = "linked", seed = 1)

test_that("method_vc: linked replicates, alpha, sigma, transforms", {
  fv <- method_vc(mvc_dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked")
  eq <- model_equation(fv)
  expect_balanced_latex(eq)
  has(eq, "y_{mir} &= \\alpha_{m} + \\mu_{i} + a_{ir} + c_{mi} + e_{mir}")
  has(eq, "a_{ir} &\\sim N(0,\\, \\omega^{2}), \\quad c_{mi} \\sim N(0,\\, \\tau^{2})")
  has(eq, "\\textstyle\\sum_{i} \\mu_{i} = 0")

  eq <- model_equation(fv, use_coefs = TRUE)
  has(eq, paste0("\\alpha_{\\mathrm{B}} = ",
                 formatC(fv$alpha$alpha[["B"]], format = "f", digits = 3)))
  sig <- fv$varcomp$sd[fv$varcomp$component == "sigma" &
                         fv$varcomp$method %in% "C"]
  has(eq, paste0("\\sigma_{\\mathrm{C}} = ",
                 formatC(sig, format = "f", digits = 3)))

  d <- mvc_dat
  d$y <- abs(d$y) + 1
  fl <- suppressWarnings(method_vc(d, y = "y", method = "method", id = "id",
                                   replicate = "repl", transform = "log"))
  has(model_equation(fl), "\\log y_{mir} &=")
  expect_equal(SimplyAgree:::.eq_tf_lhs(list(name = "logit"), "y"),
               "\\operatorname{logit} y")
  expect_equal(SimplyAgree:::.eq_tf_lhs(list(name = "pctlogit"), "y"),
               "\\operatorname{logit}(y/100)")
  expect_equal(SimplyAgree:::.eq_tf_lhs(list(name = "sqrt"), "y"),
               "\\sqrt{y}")
  expect_equal(SimplyAgree:::.eq_tf_lhs(list(name = "asinsqrt"), "y"),
               "\\arcsin\\sqrt{y}")
  expect_equal(SimplyAgree:::.eq_tf_lhs(list(name = "boxcox"), "y"), "g(y)")
  expect_length(SimplyAgree:::.eq_tf_note(list(name = "boxcox")), 1)
})

test_that("method_vc: method-specific tau and designs without replicates", {
  ft <- suppressWarnings(method_vc(mvc_dat, y = "y", method = "method",
                                   id = "id", replicate = "repl",
                                   tau = "method"))
  eq <- model_equation(ft, use_coefs = TRUE)
  has(eq, "c_{mi} &\\sim N(0,\\, \\tau^{2}_{m}), \\quad \\tau_{\\mathrm{A}} = ")

  d2 <- sim_methods(n_items = 20, n_reps = 1, methods = c("A", "B"), seed = 1)
  eq <- model_equation(method_vc(d2, y = "y", method = "method", id = "id"))
  has(eq, "y_{mi} &= \\alpha_{m} + \\mu_{i} + e_{mi}")
  has(eq, "e_{mi} &\\sim N(0,\\, \\sigma^{2})")

  d3 <- sim_methods(n_items = 20, n_reps = 1, methods = c("A", "B", "C"),
                    seed = 1)
  sm <- suppressMessages(method_vc(d3, y = "y", method = "method", id = "id"))
  eq <- model_equation(sm, use_coefs = TRUE)
  has(eq, "\\mu_{i} &\\sim N(0,\\, ")
  expect_false(grepl("sigma^{2}_{\\mu}", eq, fixed = TRUE))
  expect_false(grepl("sum_{i}", eq, fixed = TRUE))
  sm0 <- suppressMessages(method_vc(d3, y = "y", method = "method", id = "id",
                                    keep_model = FALSE))
  has(model_equation(sm0, use_coefs = TRUE), "\\sigma^{2}_{\\mu}")
})

test_that("method_convert: linear bias, pairs, and conversion from method_vc", {
  cd <- sim_methods(n_items = 30, n_reps = 2, methods = c("A", "B"),
                    alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10,
                    mu_sd = 3, tau = 0.2, sigma = 0.3, seed = 1)
  cv <- method_convert(cd, y = "y", method = "method", id = "id",
                       replicate = "repl", pair_seed = 1)
  eq <- model_equation(cv)
  expect_balanced_latex(eq)
  has(eq, "D_{ir} &= a + b\\,A_{ir} + e_{ir}")
  has(eq, "e_{ir} &\\sim N(0,\\, s^{2})")
  has(eq, "D_{ir} = y_{\\mathrm{A},ir} - y_{\\mathrm{B},ir}")
  eq <- model_equation(cv, use_coefs = TRUE)
  has(eq, formatC(cv$da$a, format = "f", digits = 3))

  cl <- method_convert(cd, y = "y", method = "method", id = "id",
                       replicate = "repl", pair_seed = 1, variance = "linear")
  eq <- model_equation(cl, use_coefs = TRUE)
  expect_balanced_latex(eq)
  has(eq, "\\,A_{ir})^{2}\\right)")

  c3 <- method_convert(mvc_dat, y = "y", method = "method", id = "id",
                       replicate = "repl", pair_seed = 1)
  eq <- model_equation(c3)
  has(eq, "D^{\\mathrm{B},\\mathrm{C}}_{ir} &= a_{\\mathrm{B},\\mathrm{C}}")
  has(eq, "D^{m,k}_{ir} = y_{m,ir} - y_{k,ir}")
  eq <- model_equation(c3, pair = c("C", "A"))
  has(eq, "D_{ir} = y_{\\mathrm{A},ir} - y_{\\mathrm{C},ir}")
  expect_error(model_equation(c3, pair = c("A", "Z")), "pair")
  expect_error(model_equation(c3, pair = "A"), "pair")

  fv <- method_vc(mvc_dat, y = "y", method = "method", id = "id",
                  replicate = "repl", replicates = "linked")
  cc <- method_convert(object = fv)
  eq <- model_equation(cc, use_coefs = TRUE)
  has(eq, "y_{mir} &= \\alpha_{m} + \\mu_{i} + a_{ir} + c_{mi} + e_{mir} \\\\")
  expect_error(model_equation(cc, pair = c("A", "B")), "bias = \"linear\"")
  expect_equal(SimplyAgree:::.eq_mvc_design(fv$varcomp),
               list(model_type = "reps", linked = TRUE, tau = "common"))
})

test_that("method_convert: scaled model", {
  skip_on_cran()
  cd <- sim_methods(n_items = 30, n_reps = 2, methods = c("A", "B"),
                    alpha = c(0, 1), beta = c(1, 1.2), mu_mean = 10,
                    mu_sd = 3, tau = 0.2, sigma = 0.3, seed = 1)
  cs <- method_convert(cd, y = "y", method = "method", id = "id",
                       replicate = "repl", bias = "scaled", tol = 1e-4)
  eq <- model_equation(cs)
  expect_balanced_latex(eq)
  has(eq, "y_{mir} &= \\alpha_{m} + \\beta_{m}(\\mu_{i} + c_{mi}) + e_{mir}, \\quad \\alpha_{\\mathrm{A}} = 0,\\ \\beta_{\\mathrm{A}} = 1")
  eq <- model_equation(cs, use_coefs = TRUE)
  has(eq, paste0("\\beta_{\\mathrm{B}} = ",
                 formatC(cs$scaled$methods$beta[2], format = "f", digits = 3)))
  # tau on the scale of the reference method
  tau <- cs$varcomp$sd[cs$varcomp$component == "tau"]
  expect_equal(tau[2], cs$scaled$methods$beta[2] * tau[1], tolerance = 1e-6)
  has(eq, paste0("c_{mi} &\\sim N(0,\\, ",
                 formatC(tau[1], format = "f", digits = 3), "^{2})"))
})

# printing ----
test_that("print and knit_print", {
  eq <- model_equation(lm(mpg ~ wt, data = mtcars))
  expect_output(print(eq), "\\begin{aligned}", fixed = TRUE)
  expect_equal(SimplyAgree:::.eq_render(SimplyAgree:::.eq_spec(list(
    SimplyAgree:::.eq_line("y", "=", "x")))), "y = x")
  skip_if_not_installed("knitr")
  out <- knitr::knit_print(eq)
  expect_s3_class(out, "knit_asis")
  expect_true(startsWith(out, "$$\n\\begin{aligned}"))
  out <- knitr::knit_print(eq, inline = TRUE)
  expect_true(startsWith(out, "$\\begin{aligned}"))
})
