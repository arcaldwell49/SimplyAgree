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


pa_coef = function(ratings.mat, conf.level){

  if (is.character(ratings.mat)){
    ratings.mat <- trim(toupper(ratings.mat))
    ratings.mat[ratings.mat==''] <- NA_character_
  }
  n <- nrow(ratings.mat) # number of subjects
  r <- ncol(ratings.mat) # number of raters
  f <- 0 # finite population correction

  # creating a vector containing all categories used by the raters

  categ.init <- unique(na.omit(as.vector(ratings.mat)))
  categ <- sort(categ.init)

  q <- length(categ)

  # creating the weights matrix

    if (weights==TRUE) {
      weights.mat<-quadratic_weights(categ)
    } else {
      weights.mat<-diag(length(categ))
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
  coef.val <- pa

  if (q>=2){
    # calculating variance, stderr & p-value of percent agreement

    den.ivec <- ri.vec*(ri.vec-1)
    den.ivec <- den.ivec - (den.ivec==0) # this operation replaces each 0 value with -1 to make the next ratio calculation always possible.
    pa.ivec <- (n/n2more)*(sum.q/den.ivec)
    var.pa<-NA;stderr<-NA;stderr.est<-NA;p.value<-NA;lcb<-NA;ucb<-NA
    if (n>=2){
      var.pa <- ((1-f)/(n*(n-1))) * sum((pa.ivec - pa)^2)
      stderr <- sqrt(var.pa)# pa's standard error
      stderr.est <- round(stderr,5)
      p.value <- 1-pt(pa/stderr,n-1)
      lcb <- pa - stderr*qt(1-(1-conf.level)/2,n-1) # lower confidence bound
      ucb <- min(1,pa + stderr*qt(1-(1-conf.level)/2,n-1)) # upper confidence bound
    }
    #conf.int <- paste0("(",round(lcb,3),",",round(ucb,3),")")
    coef.se <- stderr.est
  }
  cnames <- colnames(ratings)
  colnames(ratings) <- sapply(1:r, function(x) paste0("dumcol", x))
  obs.count <- dplyr::summarise(as.data.frame(1-is.na(ratings)),across(1:r,sum))
  tot.obs <- sum((1-is.na(ratings)))
  colnames(obs.count) <- cnames
  coef.name <- "Percent Agreement"
  df.out <- data.frame(coef.name,pa,pe,coef.val,coef.se,
                       coef.lower = lcb, coef.upper = ucb,p.value)
  return(df.out)


}
