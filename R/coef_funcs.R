# functions to aid agree_coef

quadratic_weights = function (categ)
  {
    q <- length(categ)
    weights <- diag(q)
    if (is.numeric(categ)) {
      categ.vec <- sort(categ)
    }
    else {
      categ.vec <- 1:length(categ)
    }
    xmin <- min(categ.vec)
    xmax <- max(categ.vec)
    for (k in 1:q) {
      for (l in 1:q) {
        weights[k, l] <- 1 - (categ.vec[k] - categ.vec[l])^2/(xmax -
                                                                xmin)^2
      }
    }
    return(weights)
  }


pa_coef = function(ratings.mat,
                   conf.level,
                   weighted = FALSE){

  n <- nrow(ratings.mat) # number of subjects
  r <- ncol(ratings.mat) # number of raters

  # creating a vector containing all categories used by the raters

  categ.init <- unique(na.omit(as.vector(ratings.mat)))
  categ <- sort(categ.init)

  q <- length(categ)

  # creating the weights matrix

  if (weighted == TRUE) {
    weights.mat <- quadratic_weights(categ)
  } else {
    weights.mat <- diag(length(categ))
  }
  # creating the nxq agreement matrix representing the distribution of raters by subjects and category

  agree.mat <- matrix(0,nrow=n,ncol=q)
  for(k in 1:q){
    categ.is.k <- (ratings.mat==categ[k])
    agree.mat[,k] <- (replace(categ.is.k,is.na(categ.is.k),FALSE)) %*% rep(1,r)
  }
  agree.mat.w <- t(weights.mat%*%t(agree.mat))

  # Percent Agreement  ---------

  ri.vec <- agree.mat%*%rep(1,q)
  sum.q <- (agree.mat*(agree.mat.w-1))%*%rep(1,q)
  n2more <- sum(ri.vec>=2)
  pa <- sum(sum.q[ri.vec>=2]/((ri.vec*(ri.vec-1))[ri.vec>=2]))/n2more
  pe <- 0
  coef_val_pa <- pa

  den.ivec <- ri.vec*(ri.vec-1)
  den.ivec <- den.ivec - (den.ivec==0) # this operation replaces each 0 value with -1 to make the next ratio calculation always possible.
  pa.ivec <- (n/n2more)*(sum.q/den.ivec)

  if (q>=2){
    # calculating variance, stderr & p-value of percent agreement

    if (n>=2){
      var.pa <- ((1)/(n*(n-1))) * sum((pa.ivec - pa)^2)
      stderr_pa <- sqrt(var.pa)# pa's standard error
      p.value_pa <- 1-pt( coef_val_pa/stderr_pa,n-1)
      lcb_pa <- pa - stderr_pa*qt(1-(1-conf.level)/2,n-1) # lower confidence bound
      ucb_pa <- min(1,pa + stderr_pa*qt(1-(1-conf.level)/2,n-1)) # upper confidence bound
    }

  }


  # Gwet's AC1 ------
  ri.vec <- agree.mat%*%rep(1,q)
  sum.q <- (agree.mat*(agree.mat.w-1))%*%rep(1,q)
  n2more <- sum(ri.vec>=2)
  pa <- sum(sum.q[ri.vec>=2]/((ri.vec*(ri.vec-1))[ri.vec>=2]))/n2more
  pi.vec <- t(t(rep(1/n,n))%*%(agree.mat/(ri.vec%*%t(rep(1,q)))))
  if (q >= 2) {
    pe <- sum(weights.mat) * sum(pi.vec * (1 - pi.vec)) / (q * (q - 1))
  } else {
    pe = 1e-15
  }
  gwet.ac1.est <- gwet.ac1 <- (pa-pe)/(1-pe)

  pa.ivec <- sum.q/den.ivec
  # calculating variance, stderr & p-value of gwet's ac1 coeficient

  pe.r2 <- pe*(ri.vec>=2)
  ac1.ivec <- (n/n2more)*(pa.ivec-pe.r2)/(1-pe)
  pe.ivec <- (sum(weights.mat)/(q*(q-1))) * (agree.mat%*%(1-pi.vec))/ri.vec
  ac1.ivec.x <- ac1.ivec - 2*(1-gwet.ac1) * (pe.ivec-pe)/(1-pe)

  if (n>=2){
    var.ac1 <- ((1)/(n*(n-1))) * sum((ac1.ivec.x - gwet.ac1)^2)
    stderr_ac1 <- sqrt(var.ac1)# ac1's standard error
    p.value_ac1 <- 1-pt(gwet.ac1/stderr_ac1,n-1)
    lcb_ac1 <- gwet.ac1 - stderr_ac1*qt(1-(1-conf.level)/2,n-1) # lower confidence bound
    ucb_ac1 <- min(1,gwet.ac1 + stderr_ac1*qt(1-(1-conf.level)/2,n-1)) # upper confidence bound
  }

  if (q == 1) {
    coef_name_ac <- "Gwet's AC1"
  } else{
    if (sum(weights.mat) == q) {
      coef_name_ac <- "Gwet's AC1"
    }
    else {
      coef_name_ac <- "Gwet's AC2"
    }
  }
  coef_val_ac <- gwet.ac1.est

  # Fleiss Kappa ---------
  if (q>=2) {
    pe <- sum(weights.mat * (pi.vec %*% t(pi.vec)))
  } else{
    pe=1e-15
  }

  fleiss.kappa <- (pa-pe)/(1-pe)
  coef_name = "Fleiss' kappa"
  pe.r2 <- pe*(ri.vec>=2)
  kappa.ivec <- (n/n2more)*(pa.ivec-pe.r2)/(1-pe)
  pi.vec.wk. <- weights.mat%*%pi.vec
  pi.vec.w.k <- t(weights.mat)%*%pi.vec
  pi.vec.w <- (pi.vec.wk. + pi.vec.w.k)/2
  pe.ivec <- (agree.mat%*%pi.vec.w)/ri.vec
  kappa.ivec.x <- kappa.ivec - 2*(1-fleiss.kappa) * (pe.ivec-pe)/(1-pe)

  if (n>=2){
    var.fleiss <- ((1)/(n*(n-1))) * sum((kappa.ivec.x - fleiss.kappa)^2)
    stderr_kap <- sqrt(var.fleiss)# kappa's standard error
    stderr.est_kap <- round(stderr_kap,5)
    p.value_kap <- 1-pt(fleiss.kappa/stderr_kap,n-1)
    lcb_kap <- fleiss.kappa - stderr_kap*qt(1-(1-conf.level)/2,n-1) # lower confidence bound
    ucb_kap <- min(1,fleiss.kappa + stderr_kap*qt(1-(1-conf.level)/2,n-1)) # upper confidence bound
  }

  # Kripendorff's Alpha -------

  ri.vec <- agree.mat%*%rep(1,q)
  agree.mat <- agree.mat[(ri.vec>=2),]
  agree.mat.w <- agree.mat.w[(ri.vec>=2),]
  ri.vec <- ri.vec[(ri.vec>=2)]
  ri.mean <- mean(ri.vec)
  n <- nrow(as.matrix(agree.mat))
  epsi <- 1/sum(ri.vec)
  sum.q <- (agree.mat*(agree.mat.w-1))%*%as.matrix(rep(1,q))
  paprime <-sum(sum.q/(ri.mean*(ri.vec-1)))/n
  pa <- (1-epsi)*paprime + epsi
  pi.vec <- t(t(rep(1/n,n))%*%(agree.mat/ri.mean))
  if (q>=2){pe <- sum(weights.mat * (pi.vec%*%t(pi.vec)))}else  pe=1e-15
  krippen.alpha <- (pa-pe)/(1-pe)
  krippen.alpha.prime <- (paprime-pe)/(1-pe)

  if (q>=2){
    # calculating variance, stderr & p-value of krippendorff's alpha coefficient
    pa.ivec <- sum.q/(ri.mean*(ri.vec-1)) - paprime*(ri.vec-ri.mean)/ri.mean
    krippen.ivec <- (pa.ivec-pe)/(1-pe)
    pi.vec.wk. <- weights.mat%*%pi.vec
    pi.vec.w.k <- t(weights.mat)%*%pi.vec
    pi.vec.w <- (pi.vec.wk. + pi.vec.w.k)/2
    pe.ivec <- (agree.mat%*%pi.vec.w)/ri.mean - pe * (ri.vec-ri.mean)/ri.mean
    krippen.ivec.x <- krippen.ivec - 2*(1-krippen.alpha.prime) * (pe.ivec-pe)/(1-pe)

    if (n>=2){
      var.krippen <- ((1)/(n*(n-1))) * sum((krippen.ivec.x - krippen.alpha.prime)^2)
      stderr_krippen <- sqrt(var.krippen)# alpha's standard error
      p.value_krippen <- 1-pt(krippen.alpha/stderr_krippen,n-1)
      lcb_krippen <- krippen.alpha - stderr_krippen*qt(1-(1-conf.level)/2,n-1) # lower confidence bound
      ucb_krippen <- min(1,krippen.alpha + stderr_krippen*qt(1-(1-conf.level)/2,n-1)) # upper confidence bound
    }
  }

  df_res = data.frame(
    row.names = c("Percent Agreement",
                  coef_name_ac,
                  "Fleiss' Kappa",
                  "Kririppendorff's Alpha"),
    est = c(coef_val_pa, gwet.ac1.est, fleiss.kappa, krippen.alpha),
    se = c(stderr_pa, stderr_ac1, stderr_kap, stderr_krippen),
    lower.ci =  c(lcb_pa, lcb_ac1, lcb_kap, lcb_krippen),
    upper.ci = c(ucb_pa, ucb_ac1, ucb_kap, ucb_krippen)
  )


}
