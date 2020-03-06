ccc.xy <- function(x,y) {
  
  
  
  dat <- data.frame(x, y)
  id <- complete.cases(dat)
  nmissing <- sum(!complete.cases(dat))
  dat <- dat[id, ]
  k <- length(dat$y)
  yb <- mean(dat$y)
  sy2 <- var(dat$y) * (k - 1)/k
  sd1 <- sd(dat$y)
  xb <- mean(dat$x)
  sx2 <- var(dat$x) * (k - 1)/k
  sd2 <- sd(dat$x)
  r <- cor(dat$x, dat$y)
  sl <- r * sd1/sd2
  sxy <- r * sqrt(sx2 * sy2)
  p <- 2 * sxy/(sx2 + sy2 + (yb - xb)^2)
  
  return(p)
  
}

