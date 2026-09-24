args = commandArgs(TRUE)
design = args[1]; nsim = as.integer(args[2]); off = as.integer(args[3])
suppressMessages(devtools::load_all("C:/GitHub/SimplyAgree", quiet = TRUE))
beta = 0.95; zp = qnorm(0.975)
mu = 0.8; s1 = 0.7; s2 = 0.5; se = 1
sig = sqrt(s1^2 + s2^2 + se^2)

# subjects with settings nested within subject, and measurements within
# setting; nested random intercepts plus residual error
gen = switch(design,
  balanced = function() {
    n = 15; s = 3; k = 5
    d = expand.grid(shot = 1:k, setting = 1:s, subject = 1:n)
    d
  },
  unbalanced = function() {
    n = 15
    do.call(rbind, lapply(1:n, function(i) {
      s = sample(2:4, 1)
      do.call(rbind, lapply(1:s, function(j) {
        data.frame(shot = seq_len(sample(3:7, 1)), setting = j, subject = i)
      }))
    }))
  })

fits = list(lme_nested = list(id = c("subject", "setting"), model = "lme"),
            lme_subject = list(id = "subject", model = "lme"),
            lme_combined = list(id = "subset", model = "lme"),
            gls_subject = list(id = "subject"))

set.seed(which(c("balanced", "unbalanced") == design) * 1000 + 7919 * off)
res = replicate(nsim, {
  d = gen()
  d$subset = paste(d$subject, d$setting, sep = ":")
  u1 = rnorm(max(d$subject), 0, s1)
  u2 = rnorm(length(unique(d$subset)), 0, s2)
  names(u2) = unique(d$subset)
  d$y = 0
  d$x = mu + u1[d$subject] + u2[d$subset] + rnorm(nrow(d), 0, se)
  out = c()
  for (fn in names(fits)) {
    for (bt in c("joint", "iu")) {
      r = tryCatch(suppressWarnings(suppressMessages(do.call(tolerance_limit,
            c(list(data = d, x = "x", y = "y", bound_type = bt), fits[[fn]])))),
            error = function(e) NULL)
      if (is.null(r)) {
        if (bt == "joint") out[paste(fn, "joint", sep = "_")] = NA
        else out[paste(fn, c("iu_lo", "iu_hi"), sep = "_")] = NA
        next
      }
      lim = r$limits
      if (bt == "joint") {
        cont = pnorm((lim$upper.TL - mu)/sig) - pnorm((lim$lower.TL - mu)/sig)
        out[paste(fn, "joint", sep = "_")] = mean(cont >= beta)
      } else {
        out[paste(fn, "iu_lo", sep = "_")] = mean(lim$lower.TL <= mu - zp * sig)
        out[paste(fn, "iu_hi", sep = "_")] = mean(lim$upper.TL >= mu + zp * sig)
      }
    }
  }
  out
})
saveRDS(res, sprintf("nest_%s_%d.rds", design, off))
cat(sprintf("%s (nsim=%d) fails: %d\n", design, nsim, sum(is.na(res))))
print(round(rowMeans(res, na.rm = TRUE), 3))
