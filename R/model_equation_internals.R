# Internals for model_equation() ----
#
# A model is described by an "eq_spec": a list of lines, each with a
# left-hand side, a relation ("=", "\\sim", or "" for a note) and a
# right-hand side made of pieces. A piece is a string, a parameter
# (.eq_par(): a symbol and, optionally, its estimate), a sum of terms
# (.eq_sum(): signs are handled when estimates are substituted), or a list of
# pieces. A line can also carry families of indexed parameters
# (.eq_family(), e.g. alpha_m for each method), whose estimates are listed at
# the end of the line. .eq_render() turns the spec into LaTeX, with symbols
# or with estimates (use_coefs = TRUE).
#
# Literal extractors (.eq_from_lm/lmer/gls/lme) read a fitted model;
# domain builders (.eq_reli_*, .eq_mvc_*, .eq_mconv_*) build the spec from
# the stored results with the notation of the vignettes, because the fitted
# objects of those functions are computational forms of the model (e.g. the
# ANOVA ICC stores lm(values ~ id + items)).

# LaTeX helpers ----

# a name (variable, level, method) in upright math: escape the characters
# that are special in math mode, drop the ones that cannot be shown
.eq_name = function(x) {
  x = gsub("[\\\\^~]", "", as.character(x))
  x = gsub("([{}_%&#$])", "\\\\\\1", x)
  x = gsub(" ", "\\\\ ", x)
  paste0("\\mathrm{", x, "}")
}

# a number with a fixed number of decimals; values that round to zero are
# shown in scientific notation, and -0.000 as 0.000
.eq_num = function(x,
                   digits = 3) {
  if (length(x) != 1 || !is.finite(x)) {
    return("\\mathrm{NA}")
  }
  out = formatC(x, format = "f", digits = digits)
  if (as.numeric(out) == 0) {
    if (x == 0) {
      return(formatC(0, format = "f", digits = digits))
    }
    sci = strsplit(formatC(x, format = "e", digits = max(digits - 1, 1)),
                   "e", fixed = TRUE)[[1]]
    return(paste0(sci[1], " \\times 10^{", as.integer(sci[2]), "}"))
  }
  out
}

.eq_sub = function(sym,
                   sub) {
  paste0(sym, "_{", sub, "}")
}

.eq_check_args = function(use_coefs,
                          coef_digits) {
  if (!is.logical(use_coefs) || length(use_coefs) != 1 || is.na(use_coefs)) {
    stop("`use_coefs` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(coef_digits) || length(coef_digits) != 1 ||
      !is.finite(coef_digits) || coef_digits < 0 ||
      coef_digits != round(coef_digits)) {
    stop("`coef_digits` must be a single non-negative whole number.",
         call. = FALSE)
  }
  invisible(NULL)
}

# name of a column given in a call (a string, or a symbol)
.eq_call_name = function(e) {
  if (is.character(e)) e[1] else deparse(e)
}

# representation ----

.eq_par = function(sym,
                   val = NULL,
                   num = "%s") {
  structure(list(sym = sym, val = val, num = num), class = "eq_par")
}

# a number used as the base of a power: in parentheses when negative or in
# scientific notation
.eq_base = function(val,
                    digits) {
  s = .eq_num(val, digits)
  if (startsWith(s, "-") || grepl("times", s, fixed = TRUE)) {
    paste0("(", s, ")")
  } else {
    s
  }
}

# a variance shown as SD^2 when estimates are substituted
.eq_var_par = function(sym,
                       sd = NULL) {
  .eq_par(sym, sd, num = function(v, digits) {
    paste0(.eq_base(v, digits), "^{2}")
  })
}

.eq_term = function(par = NULL,
                    x = NULL) {
  list(par = par, x = x)
}

.eq_sum = function(terms) {
  structure(list(terms = terms), class = "eq_sum")
}

# label: function giving the symbol for one level; fixed: named character
# values shown also without estimates (e.g. a reference level equal to 1)
.eq_family = function(vals,
                      label,
                      fixed = NULL) {
  list(vals = vals, label = label, fixed = fixed)
}

.eq_line = function(lhs,
                    rel = "=",
                    rhs = NULL,
                    fams = list()) {
  list(lhs = lhs, rel = rel, rhs = rhs, fams = fams)
}

.eq_note = function(tex) {
  .eq_line("", "", tex)
}

# a distributional statement "lhs ~ rhs"
.eq_stmt = function(lhs,
                    rhs,
                    fams = list()) {
  list(lhs = lhs, rhs = rhs, fams = fams)
}

.eq_normal = function(var) {
  list("N(0,\\, ", var, ")")
}

.eq_spec = function(lines) {
  structure(list(lines = lines), class = "eq_spec")
}

# statements -> lines, several statements per line
.eq_dist_lines = function(stmts,
                          per_line = 2) {
  if (length(stmts) == 0) {
    return(list())
  }
  grp = unname(split(stmts, ceiling(seq_along(stmts) / per_line)))
  lapply(grp, function(g) {
    rhs = list(g[[1]]$rhs)
    for (s in g[-1]) {
      rhs = c(rhs, list(", \\quad ", s$lhs, " \\sim ", s$rhs))
    }
    .eq_line(g[[1]]$lhs, "\\sim", rhs,
             fams = do.call(c, lapply(g, function(s) s$fams)))
  })
}

# mean line, random effects, residual, correlation and notes
.eq_assemble = function(lhs,
                        mean_terms,
                        re_stmts = list(),
                        resid_stmt,
                        cor_line = NULL,
                        notes = list(),
                        per_line = 2,
                        mean_fams = list()) {
  .eq_spec(c(list(.eq_line(lhs, "=", .eq_sum(mean_terms), fams = mean_fams)),
             .eq_dist_lines(re_stmts, per_line),
             .eq_dist_lines(list(resid_stmt), 1),
             if (!is.null(cor_line)) list(cor_line),
             notes))
}

# renderer ----

.eq_par_tex = function(p,
                       opt) {
  if (opt$use_coefs && !is.null(p$val) && all(is.finite(p$val))) {
    if (is.function(p$num)) p$num(p$val, opt$digits) else
      sprintf(p$num, .eq_num(p$val, opt$digits))
  } else {
    p$sym
  }
}

.eq_sum_tex = function(s,
                       opt) {
  out = character(0)
  for (t in s$terms) {
    coef = if (is.null(t$par)) "" else .eq_par_tex(t$par, opt)
    neg = startsWith(coef, "-")
    if (neg) {
      coef = substring(coef, 2)
    }
    body = if (is.null(t$x)) coef else if (coef == "") t$x else
      paste0(coef, "\\,", t$x)
    out = c(out, if (length(out) == 0) {
      paste0(if (neg) "-" else "", body)
    } else {
      paste(if (neg) "-" else "+", body)
    })
  }
  paste(out, collapse = " ")
}

.eq_piece = function(p,
                     opt) {
  if (is.null(p)) {
    return("")
  }
  if (is.character(p)) {
    return(paste(p, collapse = ""))
  }
  if (inherits(p, "eq_par")) {
    return(.eq_par_tex(p, opt))
  }
  if (inherits(p, "eq_sum")) {
    return(.eq_sum_tex(p, opt))
  }
  paste(vapply(p, .eq_piece, character(1), opt = opt), collapse = "")
}

.eq_fam_tex = function(fams,
                       opt) {
  parts = unlist(lapply(fams, function(f) {
    vals = if (opt$use_coefs) {
      v = f$vals[is.finite(f$vals)]
      vapply(v, .eq_num, character(1), digits = opt$digits)
    } else {
      f$fixed
    }
    if (length(vals) == 0) {
      return(NULL)
    }
    paste0(vapply(names(vals), f$label, character(1)), " = ", vals)
  }))
  if (length(parts) == 0) "" else
    paste0(", \\quad ", paste(parts, collapse = ",\\ "))
}

.eq_render = function(spec,
                      use_coefs = FALSE,
                      coef_digits = 3) {
  opt = list(use_coefs = use_coefs, digits = coef_digits)
  rows = lapply(spec$lines, function(l) {
    list(lhs = l$lhs, rel = l$rel,
         rhs = paste0(.eq_piece(l$rhs, opt), .eq_fam_tex(l$fams, opt)))
  })
  if (length(rows) == 1) {
    r = rows[[1]]
    return(trimws(paste(r$lhs, r$rel, r$rhs)))
  }
  body = vapply(rows, function(r) {
    trimws(paste0(r$lhs, " &", r$rel, " ", r$rhs))
  }, character(1))
  paste0("\\begin{aligned}\n", paste(body, collapse = " \\\\\n"),
         "\n\\end{aligned}")
}

.eq_finish = function(spec,
                      use_coefs,
                      coef_digits) {
  .eq_check_args(use_coefs, coef_digits)
  structure(.eq_render(spec, use_coefs, coef_digits),
            class = c("model_equation", "character"))
}

# literal extractors ----

# notation for the literal extractors
# lhs: symbol of the response (NULL: from the formula); obs: subscript of an
# observation; resid: residual symbol; vars: symbols for variables, by name;
# groups: by grouping factor, list(sym, mean, dist, var) for the random
# effect symbol, its subscript in the mean and distribution lines, and the
# variance symbol; expand_max: factors with more coefficients than this are
# shown as one indexed effect.
.eq_notation = function(lhs = NULL,
                        obs = "i",
                        resid = "\\varepsilon",
                        vars = list(),
                        groups = list(),
                        expand_max = 4) {
  list(lhs = lhs, obs = obs, resid = resid, vars = vars, groups = groups,
       expand_max = expand_max)
}

# symbol of a variable (without subscript)
.eq_var = function(v,
                   notation) {
  if (!is.null(notation$vars[[v]])) {
    return(notation$vars[[v]])
  }
  .eq_name(sub("^(as\\.)?factor\\((.*)\\)$", "\\2", v))
}

# variables in the right-hand side of a formula, as written in the terms
.eq_model_vars = function(f) {
  fac = attr(stats::terms(f), "factors")
  vars = rownames(fac)
  if (is.null(vars)) {
    return(character(0))
  }
  if (attr(stats::terms(f), "response") == 1) vars[-1] else vars
}

# covariate for one coefficient name: the variable for an exact match, an
# indicator for a factor level (variable name followed by the level), and a
# product for interactions
.eq_covariate = function(cn,
                         vars,
                         notation) {
  obs = notation$obs
  pieces = strsplit(cn, ":", fixed = TRUE)[[1]]
  matched = character(0)
  is_factor = FALSE
  tex = vapply(pieces, function(p) {
    if (p %in% vars) {
      matched <<- c(matched, p)
      return(.eq_sub(.eq_var(p, notation), obs))
    }
    cand = vars[startsWith(p, vars)]
    if (length(cand) > 0) {
      v = cand[which.max(nchar(cand))]
      matched <<- c(matched, v)
      is_factor <<- TRUE
      lev = substring(p, nchar(v) + 1)
      return(paste0("\\mathrm{I}(", .eq_sub(.eq_var(v, notation), obs),
                    " = ", .eq_name(lev), ")"))
    }
    .eq_sub(.eq_name(p), obs)
  }, character(1))
  list(x = paste(tex, collapse = " \\times "),
       vars = matched,
       factor = is_factor && length(pieces) == 1)
}

# fixed-effect terms: beta_0 (intercept), beta_1, ...; the coefficients of a
# factor with more than expand_max levels are one indexed effect gamma
.eq_fixed_terms = function(est,
                           vars,
                           notation) {
  est = est[!is.na(est)]
  nm = names(est)
  info = lapply(nm, function(cn) {
    if (cn == "(Intercept)") NULL else .eq_covariate(cn, vars, notation)
  })
  main_fac = vapply(info, function(i) {
    if (!is.null(i) && i$factor) i$vars[1] else NA_character_
  }, character(1))
  n_fac = table(main_fac)
  big = names(n_fac)[n_fac > notation$expand_max]
  k = if ("(Intercept)" %in% nm) 0 else 1
  done = character(0)
  terms = list()
  for (j in seq_along(nm)) {
    v = main_fac[j]
    if (!is.na(v) && v %in% big) {
      if (!(v %in% done)) {
        done = c(done, v)
        sym = paste0("\\gamma_{", .eq_var(v, notation), "[", notation$obs,
                     "]}")
        terms = c(terms, list(.eq_term(.eq_par(sym))))
      }
      next
    }
    par = .eq_par(paste0("\\beta_{", k, "}"), unname(est[j]))
    k = k + 1
    terms = c(terms, list(.eq_term(par, info[[j]]$x)))
  }
  terms
}

# left-hand side from the response of a formula
.eq_response = function(f,
                        notation) {
  obs = notation$obs
  if (!is.null(notation$lhs)) {
    return(.eq_sub(notation$lhs, obs))
  }
  resp = f[[2]]
  if (is.call(resp) && length(resp) == 2) {
    fn = as.character(resp[[1]])
    inner = .eq_sub(.eq_var(deparse(resp[[2]]), notation), obs)
    out = switch(fn,
                 log = paste0("\\log ", inner),
                 log10 = paste0("\\log_{10} ", inner),
                 log2 = paste0("\\log_{2} ", inner),
                 sqrt = paste0("\\sqrt{", inner, "}"),
                 exp = paste0("\\exp(", inner, ")"),
                 NULL)
    if (!is.null(out)) {
      return(out)
    }
  }
  .eq_sub(.eq_name(deparse(resp)), obs)
}

# notation for the k-th grouping factor (outer first)
.eq_group = function(g,
                     k,
                     notation) {
  if (!is.null(notation$groups[[g]])) {
    return(notation$groups[[g]])
  }
  sym = c("u", "v", "w", "b", "c", "g")[min(k, 6)]
  idx = c("j", "k", "l", "q", "s", "h")[min(k, 6)]
  list(sym = sym,
       mean = paste0(idx, "[", notation$obs, "]"),
       dist = idx,
       var = paste0("\\sigma^{2}_{", sym, "}"))
}

# random effects of one grouping factor: V is the covariance matrix of the
# effects, type one of "single", "diag", "ident", "unstructured"
.eq_random = function(g,
                      k,
                      cols,
                      V,
                      type,
                      vars,
                      notation) {
  gr = .eq_group(g, k, notation)
  if (length(cols) == 1) {
    type = "single"
  }
  idx = if (length(cols) == 1) "" else as.character(seq_along(cols) - 1)
  sym_mean = paste0(gr$sym, "_{", idx, gr$mean, "}")
  sym_dist = paste0(gr$sym, "_{", idx, gr$dist, "}")
  mean_terms = lapply(seq_along(cols), function(c) {
    if (cols[c] == "(Intercept)") {
      return(.eq_term(NULL, sym_mean[c]))
    }
    cov = .eq_covariate(cols[c], vars, notation)$x
    .eq_term(NULL, paste0(sym_mean[c], "\\,", cov))
  })
  sds = sqrt(diag(V))
  stmts = switch(
    type,
    single = list(.eq_stmt(sym_dist, .eq_normal(.eq_var_par(gr$var, sds[1])))),
    diag = lapply(seq_along(cols), function(c) {
      .eq_stmt(sym_dist[c],
               .eq_normal(.eq_var_par(paste0("\\sigma^{2}_{", gr$sym,
                                             idx[c], "}"), sds[c])))
    }),
    ident = list(.eq_stmt(paste(sym_dist, collapse = ", "),
                          .eq_normal(.eq_var_par(gr$var, sds[1])))),
    unstructured = {
      vec = paste0("\\begin{pmatrix} ", paste(sym_dist, collapse = " \\\\ "),
                   " \\end{pmatrix}")
      mat = .eq_par(paste0("\\boldsymbol{\\Sigma}_{", gr$sym, "}"), V,
                    num = function(V, digits) {
                      rows = apply(V, 1, function(r) {
                        paste(vapply(r, .eq_num, character(1),
                                     digits = digits), collapse = " & ")
                      })
                      paste0("\\begin{pmatrix} ",
                             paste(rows, collapse = " \\\\ "),
                             " \\end{pmatrix}")
                    })
      list(.eq_stmt(vec, list("N\\left(\\mathbf{0},\\, ", mat, "\\right)")))
    })
  list(mean_terms = mean_terms, stmts = stmts)
}

# residual statement, with the nlme variance function (or lm weights)
.eq_resid_stmt = function(sigma,
                          notation,
                          vs = NULL,
                          weighted = FALSE) {
  obs = notation$obs
  r = .eq_sub(notation$resid, obs)
  s2 = .eq_var_par("\\sigma^{2}", sigma)
  if (weighted) {
    return(.eq_stmt(r, list("N(0,\\, ", s2, "/w_{", obs, "})")))
  }
  if (is.null(vs)) {
    return(.eq_stmt(r, .eq_normal(s2)))
  }
  cov_tex = function() {
    cv = nlme::getCovariateFormula(vs)
    v = deparse(cv[[length(cv)]])
    if (grepl("fitted", v, fixed = TRUE)) {
      paste0("\\hat{\\mu}_{", obs, "}")
    } else {
      .eq_sub(.eq_var(v, notation), obs)
    }
  }
  cf = stats::coef(vs, unconstrained = FALSE, allCoef = TRUE)
  strata = !is.null(nlme::getGroupsFormula(vs))
  generic = .eq_stmt(r, list("N\\left(0,\\, ", s2, "g^{2}_{", obs,
                             "}\\right)"))
  switch(
    class(vs)[1],
    varIdent = {
      gv = all.vars(nlme::getGroupsFormula(vs))
      gtex = if (length(gv) == 1) .eq_sub(.eq_var(gv, notation), obs) else
        .eq_sub(.eq_name(paste(gv, collapse = "*")), obs)
      ref = names(cf)[which(cf == 1)[1]]
      .eq_stmt(r, list("N\\left(0,\\, ", s2, "\\delta^{2}_{", gtex,
                       "}\\right)"),
               fams = list(.eq_family(
                 cf, function(l) paste0("\\delta_{", .eq_name(l), "}"),
                 fixed = stats::setNames("1", ref))))
    },
    varPower = if (strata || length(cf) != 1) generic else {
      .eq_stmt(r, list("N\\left(0,\\, ", s2, "\\left|", cov_tex(),
                       "\\right|^{2", .eq_par("\\delta", unname(cf), "(%s)"),
                       "}\\right)"))
    },
    varExp = if (strata || length(cf) != 1) generic else {
      .eq_stmt(r, list("N\\left(0,\\, ", s2, "\\exp(2",
                       .eq_par("\\delta", unname(cf), "(%s)"), cov_tex(),
                       ")\\right)"))
    },
    varFixed = .eq_stmt(r, list("N(0,\\, ", s2, cov_tex(), ")")),
    generic)
}

# correlation of the residuals within a group (nlme corStruct)
.eq_cor_line = function(cs,
                        notation) {
  if (is.null(cs)) {
    return(NULL)
  }
  obs = notation$obs
  last = substring(obs, nchar(obs))
  r1 = .eq_sub(notation$resid, obs)
  r2 = .eq_sub(notation$resid, paste0(obs, "'"))
  lhs = paste0("\\operatorname{Cor}(", r1, ", ", r2, ")")
  grp = all.vars(nlme::getGroupsFormula(cs))
  same = if (nchar(obs) == 1 && length(grp) > 0) {
    paste0(",\\ \\text{same } ",
           paste(vapply(grp, .eq_var, character(1), notation = notation),
                 collapse = ", "))
  } else ""
  cond = paste0(", \\quad ", last, " \\neq ", last, "'", same)
  cf = stats::coef(cs, unconstrained = FALSE)
  switch(
    class(cs)[1],
    corCompSymm = .eq_line(lhs, "=", list(.eq_par("\\rho", unname(cf)),
                                          cond)),
    corAR1 = ,
    corCAR1 = {
      cv = nlme::getCovariateFormula(cs)
      v = deparse(cv[[length(cv)]])
      lag = if (v == "1") {
        paste0("|", last, " - ", last, "'|")
      } else {
        vt = .eq_var(v, notation)
        paste0("|", .eq_sub(vt, obs), " - ", .eq_sub(vt, paste0(obs, "'")),
               "|")
      }
      .eq_line(lhs, "=", list(.eq_par("\\phi", unname(cf), .eq_base), "^{",
                              lag, "}",
                              cond))
    },
    .eq_line(lhs, "=", list("R_{", obs, ",", obs, "'}", cond,
                            ",\\ \\text{", class(cs)[1], "}")))
}

.eq_from_lm = function(x,
                       notation = .eq_notation()) {
  if (inherits(x, c("glm", "mlm"))) {
    stop("model_equation() supports linear models (lm), not ",
         class(x)[1], " objects.", call. = FALSE)
  }
  f = stats::formula(x)
  vars = .eq_model_vars(f)
  .eq_assemble(lhs = .eq_response(f, notation),
               mean_terms = c(.eq_fixed_terms(stats::coef(x), vars, notation),
                              list(.eq_term(NULL, .eq_sub(notation$resid,
                                                          notation$obs)))),
               resid_stmt = .eq_resid_stmt(stats::sigma(x), notation,
                                           weighted = !is.null(x$weights)))
}

.eq_from_gls = function(x,
                        notation = .eq_notation()) {
  f = stats::formula(x)
  vars = .eq_model_vars(f)
  .eq_assemble(lhs = .eq_response(f, notation),
               mean_terms = c(.eq_fixed_terms(stats::coef(x), vars, notation),
                              list(.eq_term(NULL, .eq_sub(notation$resid,
                                                          notation$obs)))),
               resid_stmt = .eq_resid_stmt(x$sigma, notation,
                                           vs = x$modelStruct$varStruct),
               cor_line = .eq_cor_line(x$modelStruct$corStruct, notation))
}

.eq_from_lme = function(x,
                        notation = .eq_notation()) {
  f = stats::formula(x)
  vars = .eq_model_vars(f)
  re = x$modelStruct$reStruct
  vars_re = unique(c(vars, unlist(lapply(re, function(pd) {
    all.vars(stats::formula(pd))
  }))))
  groups = names(x$groups)
  rand = lapply(seq_along(groups), function(k) {
    pd = re[[groups[k]]]
    type = switch(class(pd)[1],
                  pdDiag = "diag",
                  pdIdent = "ident",
                  pdBlocked = stop("model_equation() does not support ",
                                   "pdBlocked random effects.",
                                   call. = FALSE),
                  "unstructured")
    V = as.matrix(pd) * x$sigma^2
    .eq_random(groups[k], k, colnames(V), V, type, vars_re, notation)
  })
  .eq_assemble(lhs = .eq_response(f, notation),
               mean_terms = c(.eq_fixed_terms(nlme::fixef(x), vars, notation),
                              do.call(c, lapply(rand, `[[`, "mean_terms")),
                              list(.eq_term(NULL, .eq_sub(notation$resid,
                                                          notation$obs)))),
               re_stmts = do.call(c, lapply(rand, `[[`, "stmts")),
               resid_stmt = .eq_resid_stmt(x$sigma, notation,
                                           vs = x$modelStruct$varStruct),
               cor_line = .eq_cor_line(x$modelStruct$corStruct, notation))
}

.eq_from_lmer = function(x,
                         notation = .eq_notation()) {
  f = stats::formula(x, fixed.only = TRUE)
  vars = .eq_model_vars(f)
  vars_re = unique(c(vars, all.vars(stats::formula(x))))
  cnms = lme4::getME(x, "cnms")
  vc = lme4::VarCorr(x)
  rand = lapply(seq_along(cnms), function(k) {
    V = matrix(as.numeric(vc[[k]]), nrow(vc[[k]]))
    .eq_random(names(cnms)[k], k, cnms[[k]], V, "unstructured", vars_re,
               notation)
  })
  .eq_assemble(lhs = .eq_response(f, notation),
               mean_terms = c(.eq_fixed_terms(lme4::fixef(x), vars, notation),
                              do.call(c, lapply(rand, `[[`, "mean_terms")),
                              list(.eq_term(NULL, .eq_sub(notation$resid,
                                                          notation$obs)))),
               re_stmts = do.call(c, lapply(rand, `[[`, "stmts")),
               resid_stmt = .eq_resid_stmt(stats::sigma(x), notation))
}

# domain builders: reliability ----

# y_ij = mu + s_i [+ t_j] + e_ij (reliability vignette)
.eq_reli_twoway = function(mu,
                           vc,
                           model = "twoway",
                           log_scale = FALSE) {
  sd = sqrt(vc)
  twoway = model == "twoway"
  .eq_assemble(
    lhs = if (log_scale) "\\log y_{ij}" else "y_{ij}",
    mean_terms = c(list(.eq_term(.eq_par("\\mu", mu)),
                        .eq_term(NULL, "s_{i}")),
                   if (twoway) list(.eq_term(NULL, "t_{j}")),
                   list(.eq_term(NULL, "e_{ij}"))),
    re_stmts = c(list(.eq_stmt("s_{i}", .eq_normal(
      .eq_var_par("\\sigma^{2}_{s}", sd[["id"]])))),
      if (twoway) list(.eq_stmt("t_{j}", .eq_normal(
        .eq_var_par("\\sigma^{2}_{t}", sd[["items"]]))))),
    resid_stmt = .eq_stmt("e_{ij}", .eq_normal(
      .eq_var_par("\\sigma^{2}_{e}", sd[["resid"]]))))
}

# facet designs (generalizability theory): an effect for every subset of
# p (subjects), i (items) and o (occasions), and e (repeats) with
# replication; without replication the highest-order effect is the residual
.eq_reli_facet = function(fit,
                          log_scale = FALSE) {
  eff = fit$effects
  resid_eff = if ("e" %in% eff) "e" else eff[length(eff)]
  obs = paste0(paste(fit$factors, collapse = ""),
               if ("e" %in% eff) "r" else "")
  mu = if (inherits(fit$fit, "merMod")) {
    unname(lme4::fixef(fit$fit)[1])
  } else {
    fit$grand_mean
  }
  sd = sqrt(pmax(fit$vc, 0))
  re = setdiff(eff, resid_eff)
  tex = ifelse(nchar(re) == 1, re, paste0("(", re, ")"))
  .eq_assemble(
    lhs = paste0(if (log_scale) "\\log " else "", "y_{", obs, "}"),
    mean_terms = c(list(.eq_term(.eq_par("\\mu", mu))),
                   lapply(tex, function(t) .eq_term(NULL, t)),
                   list(.eq_term(NULL, paste0("e_{", obs, "}")))),
    re_stmts = lapply(seq_along(re), function(j) {
      .eq_stmt(tex[j], .eq_normal(.eq_var_par(
        paste0("\\sigma^{2}_{", re[j], "}"), sd[[re[j]]])))
    }),
    resid_stmt = .eq_stmt(paste0("e_{", obs, "}"), .eq_normal(
      .eq_var_par("\\sigma^{2}_{e}", sd[[resid_eff]]))),
    per_line = 3)
}

# domain builders: method comparison ----

# left-hand side on the analysis scale of a method comparison transform
.eq_tf_lhs = function(tf,
                      y) {
  nm = if (is.null(tf)) "none" else tf$name
  switch(nm,
         none = y,
         log = paste0("\\log ", y),
         logit = paste0("\\operatorname{logit} ", y),
         pctlogit = paste0("\\operatorname{logit}(", y, "/100)"),
         sqrt = paste0("\\sqrt{", y, "}"),
         asinsqrt = paste0("\\arcsin\\sqrt{", y, "}"),
         paste0("g(", y, ")"))
}

.eq_tf_note = function(tf) {
  nm = if (is.null(tf)) "none" else tf$name
  builtin = c("none", "log", "logit", "pctlogit", "sqrt", "asinsqrt")
  if (nm %in% builtin) {
    return(list())
  }
  list(.eq_note(paste0("\\text{where } g = ", .eq_name(nm))))
}

.eq_method_fam = function(vals,
                          sym,
                          fixed = NULL) {
  .eq_family(vals, function(l) paste0(sym, "_{", .eq_name(l), "}"),
             fixed = fixed)
}

# design of a variance component model from its table of components
.eq_mvc_design = function(varcomp) {
  comp = varcomp$component
  tau_m = varcomp$method[comp == "tau"]
  sig_m = varcomp$method[comp == "sigma"]
  model_type = if (any(comp == "tau")) "reps" else
    if (length(sig_m) == 1 && is.na(sig_m)) "single_2" else "single_m"
  list(model_type = model_type,
       linked = any(comp == "omega"),
       tau = if (length(tau_m) > 0 && !all(is.na(tau_m))) "method" else
         "common")
}

# Carstensen (2010) variance component model (method_vc vignette)
.eq_mvc_spec = function(design,
                        varcomp,
                        tf,
                        alpha = NULL,
                        sigma_mu = NULL) {
  vc_sd = function(comp, m = NA) {
    sel = varcomp$component == comp &
      (if (is.na(m)) is.na(varcomp$method) else varcomp$method %in% m)
    if (any(sel)) varcomp$sd[sel][1] else NULL
  }
  vc_fam = function(comp) {
    rows = varcomp[varcomp$component == comp & !is.na(varcomp$method), ]
    stats::setNames(rows$sd, rows$method)
  }
  alpha_fam = if (!is.null(alpha)) list(.eq_method_fam(alpha, "\\alpha")) else
    list()
  sig_fam = list(.eq_method_fam(vc_fam("sigma"), "\\sigma"))
  sum_note = list(.eq_note("\\textstyle\\sum_{i} \\mu_{i} = 0"))
  type = design$model_type

  if (type == "single_2") {
    return(.eq_assemble(
      lhs = .eq_tf_lhs(tf, "y_{mi}"),
      mean_terms = list(.eq_term(NULL, "\\alpha_{m}"),
                        .eq_term(NULL, "\\mu_{i}"),
                        .eq_term(NULL, "e_{mi}")),
      mean_fams = alpha_fam,
      resid_stmt = .eq_stmt("e_{mi}", .eq_normal(
        .eq_var_par("\\sigma^{2}", vc_sd("sigma")))),
      notes = c(sum_note, .eq_tf_note(tf))))
  }

  if (type == "single_m") {
    # items random: with fixed items the likelihood is unbounded
    return(.eq_assemble(
      lhs = .eq_tf_lhs(tf, "y_{mi}"),
      mean_terms = list(.eq_term(NULL, "\\alpha_{m}"),
                        .eq_term(NULL, "\\mu_{i}"),
                        .eq_term(NULL, "e_{mi}")),
      mean_fams = alpha_fam,
      re_stmts = list(.eq_stmt("\\mu_{i}", .eq_normal(
        .eq_var_par("\\sigma^{2}_{\\mu}", sigma_mu)))),
      resid_stmt = .eq_stmt("e_{mi}", .eq_normal("\\sigma^{2}_{m}"),
                            fams = sig_fam),
      notes = .eq_tf_note(tf)))
  }

  # replicates: fixed items, method-by-item effects c, item-by-replicate
  # effects a (linked replicates)
  tau_stmt = if (design$tau == "method") {
    .eq_stmt("c_{mi}", .eq_normal("\\tau^{2}_{m}"),
             fams = list(.eq_method_fam(vc_fam("tau"), "\\tau")))
  } else {
    .eq_stmt("c_{mi}", .eq_normal(.eq_var_par("\\tau^{2}", vc_sd("tau"))))
  }
  .eq_assemble(
    lhs = .eq_tf_lhs(tf, "y_{mir}"),
    mean_terms = c(list(.eq_term(NULL, "\\alpha_{m}"),
                        .eq_term(NULL, "\\mu_{i}")),
                   if (design$linked) list(.eq_term(NULL, "a_{ir}")),
                   list(.eq_term(NULL, "c_{mi}"),
                        .eq_term(NULL, "e_{mir}"))),
    mean_fams = alpha_fam,
    re_stmts = c(if (design$linked) {
      list(.eq_stmt("a_{ir}", .eq_normal(
        .eq_var_par("\\omega^{2}", vc_sd("omega")))))
    }, list(tau_stmt)),
    resid_stmt = .eq_stmt("e_{mir}", .eq_normal("\\sigma^{2}_{m}"),
                          fams = sig_fam),
    notes = c(sum_note, .eq_tf_note(tf)))
}

# method conversion: differences on averages, one block per pair
# (Carstensen 2010, section 4.7)
.eq_mconv_da = function(da,
                        variance,
                        has_reps,
                        tf) {
  obs = if (has_reps) "ir" else "i"
  multi = nrow(da) > 1
  lines = list()
  for (j in seq_len(nrow(da))) {
    m1 = da$method1[j]
    m2 = da$method2[j]
    pr = if (multi) paste0(.eq_name(m1), ",", .eq_name(m2)) else ""
    sup = if (multi) paste0("^{", pr, "}") else ""
    par = function(s, v) {
      .eq_par(if (multi) paste0(s, "_{", pr, "}") else s, v)
    }
    D = paste0("D", sup, "_{", obs, "}")
    A = paste0("A", sup, "_{", obs, "}")
    e = paste0("e", sup, "_{", obs, "}")
    lines = c(lines, list(.eq_line(D, "=", .eq_sum(list(
      .eq_term(par("a", da$a[j])),
      .eq_term(par("b", da$b[j]), A),
      .eq_term(NULL, e))))))
    lines = c(lines, list(if (variance == "linear") {
      .eq_line(e, "\\sim", list("N\\left(0,\\, (", .eq_sum(list(
        .eq_term(par("c", da$c_sd[j])),
        .eq_term(par("d", da$d_sd[j]), A))), ")^{2}\\right)"))
    } else {
      .eq_line(e, "\\sim", .eq_normal(.eq_var_par(
        if (multi) paste0("s^{2}_{", pr, "}") else "s^{2}", da$s[j])))
    }))
  }
  y1 = .eq_tf_lhs(tf, paste0("y_{", if (multi) "m" else .eq_name(da$method1[1]),
                             ",", obs, "}"))
  y2 = .eq_tf_lhs(tf, paste0("y_{", if (multi) "k" else .eq_name(da$method2[1]),
                             ",", obs, "}"))
  sup = if (multi) "^{m,k}" else ""
  .eq_spec(c(lines,
             list(.eq_note(paste0(
               "\\text{where } D", sup, "_{", obs, "} = ", y1, " - ", y2,
               ",\\ A", sup, "_{", obs, "} = (", y1, " + ", y2, ")/2"))),
             .eq_tf_note(tf)))
}

# method conversion: scaled model (Carstensen 2010, ch. 7). alpha and beta
# are relative to the first method; tau and omega are on its scale.
.eq_mconv_scaled = function(methods,
                            varcomp,
                            tf,
                            ml = FALSE) {
  lv = methods$method
  ref = lv[1]
  vc_sd = function(comp) {
    varcomp$sd[varcomp$component == comp & varcomp$method %in% ref][1]
  }
  linked = any(varcomp$component == "omega")
  sig = varcomp[varcomp$component == "sigma", ]
  inner = paste0("\\mu_{i}", if (linked) " + a_{ir}" else "", " + c_{mi}")
  .eq_assemble(
    lhs = .eq_tf_lhs(tf, "y_{mir}"),
    mean_terms = list(.eq_term(NULL, "\\alpha_{m}"),
                      .eq_term(NULL, paste0("\\beta_{m}(", inner, ")")),
                      .eq_term(NULL, "e_{mir}")),
    mean_fams = list(
      .eq_method_fam(stats::setNames(methods$alpha, lv), "\\alpha",
                     fixed = stats::setNames("0", ref)),
      .eq_method_fam(stats::setNames(methods$beta, lv), "\\beta",
                     fixed = stats::setNames("1", ref))),
    # the ML fit is structural: the true values are normally distributed
    re_stmts = c(if (ml) {
      list(.eq_stmt("\\mu_{i}", list("N(\\mu,\\, \\sigma^{2}_{\\mu})")))
    }, if (linked) {
      list(.eq_stmt("a_{ir}", .eq_normal(
        .eq_var_par("\\omega^{2}", vc_sd("omega")))))
    }, list(.eq_stmt("c_{mi}", .eq_normal(
      .eq_var_par("\\tau^{2}", vc_sd("tau")))))),
    resid_stmt = .eq_stmt("e_{mir}", .eq_normal("\\sigma^{2}_{m}"),
                          fams = list(.eq_method_fam(
                            stats::setNames(sig$sd, sig$method), "\\sigma"))),
    notes = .eq_tf_note(tf))
}

# class builders ----

# tolerance_limit(): the stored nlme model with the notation of the
# agreement vignette (subject i, measurement j; nested: setting j, k)
.eq_tolerance = function(x) {
  if (is.null(x$model)) {
    stop("model_equation() needs the fitted model; refit with ",
         "keep_model = TRUE.", call. = FALSE)
  }
  call2 = x$call
  id = call2$id
  nested = length(id) == 2
  obs = if (is.null(id)) "i" else if (nested) "ijk" else "ij"
  vars = list(avg = "A", time = "t")
  if (!is.null(call2$condition)) {
    vars$condition = .eq_name(.eq_call_name(call2$condition))
  }
  groups = list(id = list(sym = "u", mean = "i", dist = "i",
                          var = if (nested) "\\sigma^{2}_{1}" else
                            "\\sigma^{2}_{u}"),
                id_2 = list(sym = "v", mean = "ij", dist = "ij",
                            var = "\\sigma^{2}_{2}"))
  notation = .eq_notation(lhs = "d", obs = obs, resid = "e", vars = vars,
                          groups = groups, expand_max = Inf)
  spec = if (inherits(x$model, "lme")) {
    .eq_from_lme(x$model, notation)
  } else {
    .eq_from_gls(x$model, notation)
  }
  xv = .eq_sub(.eq_name(.eq_call_name(call2$x)), obs)
  yv = .eq_sub(.eq_name(.eq_call_name(call2$y)), obs)
  d_def = if (isTRUE(call2$log_tf)) {
    paste0("\\log ", xv, " - \\log ", yv)
  } else {
    paste0(xv, " - ", yv)
  }
  note = paste0("\\text{where } d_{", obs, "} = ", d_def,
                if (isTRUE(call2$prop_bias)) {
                  paste0(",\\ A_{", obs, "} = (", xv, " + ", yv, ")/2")
                })
  spec$lines = c(spec$lines, list(.eq_note(note)))
  spec
}

.eq_reli_htest = function(x) {
  s = x$settings
  fit = x$fit
  log_scale = identical(s$scale, "log")
  if (identical(s$statistic, "facet") || inherits(fit, "facet_fit")) {
    return(.eq_reli_facet(fit, log_scale = log_scale))
  }
  mu = if (!is.null(fit$effects$mu)) fit$effects$mu else fit$grand_mean
  .eq_reli_twoway(mu, fit$vc, model = fit$model, log_scale = log_scale)
}

.eq_simple_reli = function(x) {
  vc = x$var_comp
  # reli_aov() stores lm(values ~ id + items), whose mean is the grand mean
  mu = if (inherits(x$lmer, "merMod")) {
    unname(lme4::fixef(x$lmer)[1])
  } else if (inherits(x$lmer, "lm")) {
    mean(stats::model.response(stats::model.frame(x$lmer)))
  }
  .eq_reli_twoway(mu, c(id = vc["ID", "variance"],
                        items = vc["Items", "variance"],
                        resid = vc["Residual", "variance"]))
}

.eq_simple_mvc = function(x) {
  sigma_mu = NULL
  if (x$design$model_type == "single_m" && inherits(x$model, "lme")) {
    re = as.matrix(x$model$modelStruct$reStruct)
    sigma_mu = sqrt(re[[1]][1, 1]) * x$model$sigma
  }
  .eq_mvc_spec(x$design, x$varcomp, x$transform,
               alpha = x$alpha$alpha, sigma_mu = sigma_mu)
}

.eq_simple_mconv = function(x,
                            pair = NULL) {
  bias = x$fit_info$bias
  if (!is.null(pair) && bias != "linear") {
    stop("`pair` applies only to method_convert() results with ",
         "bias = \"linear\".", call. = FALSE)
  }
  if (bias == "scaled") {
    return(.eq_mconv_scaled(x$scaled$methods, x$varcomp, x$transform,
                            ml = mc_is_ml(x$fit_info)))
  }
  if (bias == "constant") {
    return(.eq_mvc_spec(.eq_mvc_design(x$varcomp), x$varcomp, x$transform))
  }
  da = x$da
  if (!is.null(pair)) {
    pair = as.character(pair)
    sel = (da$method1 == pair[1] & da$method2 == pair[2]) |
      (da$method1 == pair[2] & da$method2 == pair[1])
    if (length(pair) != 2 || pair[1] == pair[2] || !any(sel)) {
      stop("`pair` must name two different methods from: ",
           paste(unique(c(da$method1, da$method2)), collapse = ", "), ".",
           call. = FALSE)
    }
    da = da[sel, ]
  }
  .eq_mconv_da(da, x$fit_info$variance, isTRUE(x$fit_info$has_reps),
               x$transform)
}
