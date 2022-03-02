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


pa_coef = function(ratings.mat){

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

  # calculating percent agreement coefficient

  ri.vec <- agree.mat%*%rep(1,q)
  sum.q <- (agree.mat*(agree.mat.w-1))%*%rep(1,q)
  n2more <- sum(ri.vec>=2)
  pa <- sum(sum.q[ri.vec>=2]/((ri.vec*(ri.vec-1))[ri.vec>=2]))/n2more
  pe <- 0
  coeff.val <- pa



}
