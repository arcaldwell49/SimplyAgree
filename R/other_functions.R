ccc.xy <- function(x, y, conf.level) {
  N. <- 1 - ((1 - conf.level) / 2)
  zv <- qnorm(N., mean = 0, sd = 1)
  dat <- data.frame(x, y)
  id <- complete.cases(dat)
  nmissing <- sum(!complete.cases(dat))
  dat <- dat[id,]
  k <- length(dat$y)
  yb <- mean(dat$y)
  sy2 <- var(dat$y) * (k - 1) / k
  sd1 <- sd(dat$y)
  xb <- mean(dat$x)
  sx2 <- var(dat$x) * (k - 1) / k
  sd2 <- sd(dat$x)
  r <- cor(dat$x, dat$y)
  sl <- r * sd1 / sd2
  sxy <- r * sqrt(sx2 * sy2)
  p <- 2 * sxy / (sx2 + sy2 + (yb - xb) ^ 2)

  delta <- (dat$x - dat$y)
  rmean <- apply(dat, MARGIN = 1, FUN = mean)
  blalt <- data.frame(x = dat$x,
                      y = dat$y,
                      mean = rmean,
                      delta)
  v <- sd1 / sd2
  u <- (yb - xb) / ((sx2 * sy2) ^ 0.25)
  C.b <- p / r
  sep <- sqrt(((1 - ((r) ^ 2)) * (p) ^ 2 * (1 - ((p) ^ 2)) / (r) ^ 2 +
                 (2 * (p) ^ 3 * (1 - p) * (u) ^ 2 / r) - 0.5 * (p) ^ 4 * (u) ^
                 4 / (r) ^ 2
  ) / (k -
         2))
  ll <- p - (zv * sep)
  ul <- p + (zv * sep)
  t <- log((1 + p) / (1 - p)) / 2
  set = sep / (1 - ((p) ^ 2))
  llt = t - (zv * set)
  ult = t + (zv * set)
  llt = (exp(2 * llt) - 1) / (exp(2 * llt) + 1)
  ult = (exp(2 * ult) - 1) / (exp(2 * ult) + 1)
  delta.sd <- sqrt(var(delta, na.rm = TRUE))

  ba.p <- mean(delta)

  sblalt <- data.frame(est = ba.p, delta.sd = delta.sd)

  rho.c <- data.frame(p, llt, ult)
  names(rho.c) <- c("est.ccc", "lower.ci", "upper.ci")

  res_list <- list(
    rho.c = rho.c,
    s.shift = v,
    l.shift = u,
    bias = C.b,
    df_diff = blalt,
    delta = sblalt,
    nmissing = nmissing
  )
  #s.shift = scale (variance) shift
  #l.shift = change in mean
  #bias = bias correction (1 = perfect)
  #mean.dlt = mean and delta pairs data frame
  #sblalt =

}


