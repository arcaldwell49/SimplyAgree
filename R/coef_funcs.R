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
                   weighted =FALSE){

  if (is.character(ratings.mat)){
    ratings.mat <- trim(toupper(ratings.mat))
    ratings.mat[ratings.mat==''] <- NA_character_
  }
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

  # calculating percent agreement coeficient

  ri.vec <- agree.mat%*%rep(1,q)
  sum.q <- (agree.mat*(agree.mat.w-1))%*%rep(1,q)
  n2more <- sum(ri.vec>=2)
  pa <- sum(sum.q[ri.vec>=2]/((ri.vec*(ri.vec-1))[ri.vec>=2]))/n2more
  pe <- 0
  coef_val <- pa

  den.ivec <- ri.vec*(ri.vec-1)
  den.ivec <- den.ivec - (den.ivec==0) # this operation replaces each 0 value with -1 to make the next ratio calculation always possible.
  pa.ivec <- (n/n2more)*(sum.q/den.ivec)

  if (q>=2){
    # calculating variance, stderr & p-value of percent agreement



    if (n>=2){
      var.pa <- ((1)/(n*(n-1))) * sum((pa.ivec - pa)^2)
      stderr <- sqrt(var.pa)# pa's standard error
      stderr.est <- round(stderr,5)
      p.value <- 1-pt(pa/stderr,n-1)
      lcb <- pa - stderr*qt(1-(1-conf.level)/2,n-1) # lower confidence bound
      ucb <- min(1,pa + stderr*qt(1-(1-conf.level)/2,n-1)) # upper confidence bound
    }

    coef_se <- stderr.est
  }

  coef_name <- "Percent Agreement"
  df_pa <- data.frame(coef_name = "Percent Agreement",
                       pa,pe,
                       coef_val, coef_se,
                       coef_lower = lcb, coef_upper = ucb,p.value)
  # calculating gwet's ac1 coeficient

  pa <- sum(sum.q[ri.vec>=2]/((ri.vec*(ri.vec-1))[ri.vec>=2]))/n2more
  pi.vec <- t(t(rep(1/n,n))%*%(agree.mat/(ri.vec%*%t(rep(1,q)))))
  if (q >= 2) {
    pe <- sum(weights.mat) * sum(pi.vec * (1 - pi.vec)) / (q * (q - 1))
  } else {
    pe = 1e-15
  }
  gwet.ac1 <- (pa-pe)/(1-pe)
  #gwet.ac1.est <- round(gwet.ac1,5)

  # calculating variance, stderr & p-value of gwet's ac1 coeficient

  pe.r2 <- pe*(ri.vec>=2)
  ac1.ivec <- (n/n2more)*(pa.ivec-pe.r2)/(1-pe)
  pe.ivec <- (sum(weights.mat)/(q*(q-1))) * (agree.mat%*%(1-pi.vec))/ri.vec
  ac1.ivec.x <- ac1.ivec - 2*(1-gwet.ac1) * (pe.ivec-pe)/(1-pe)

  if (n>=2){
    var.ac1 <- ((1-f)/(n*(n-1))) * sum((ac1.ivec.x - gwet.ac1)^2)
    stderr <- sqrt(var.ac1)# ac1's standard error
    stderr.est <- round(stderr,5)
    p.value <- 1-pt(gwet.ac1/stderr,n-1)
    lcb <- gwet.ac1 - stderr*qt(1-(1-conf.level)/2,n-1) # lower confidence bound
    ucb <- min(1,gwet.ac1 + stderr*qt(1-(1-conf.level)/2,n-1)) # upper confidence bound
  }

  if (q==1) coef_name <- "AC1"
  else{
    if (sum(weights.mat)==q) coef_name <- "AC1"
    else coef_name <- "AC2"
  }
  coef_val <- gwet.ac1.est
  coef_se <- stderr.est

  df_ac <- data.frame(coef_name = coef_name,
                      pa,pe,
                      coef_val, coef_se,
                      coef_lower = lcb, coef_upper = ucb,p.value)


}
