#' Design function used to specify the parameters to be used in simulations
#' @param design String specifying the ANOVA design.
#' @param n Sample size in each condition
#' @param mu Vector specifying mean for each condition
#' @param sd standard deviation for all conditions (or a vector specifying the sd for each condition)
#' @param r Correlation between dependent variables (single value or matrix)
#' @param labelnames Optional vector to specifying factor and condition names (recommended, if not used factors and levels are indicated by letters and numbers)
#' @param plot Should means plot be printed (defaults to TRUE)
#' @return Returns single list with simulated data, design, design list, factor names, formulas for ANOVA, means, sd, correlation, sample size per condition, correlation matrix, covariance matrix, design string, labelnames, labelnameslist, factor names, meansplot
#'
#' \describe{
#'   \item{\code{"dataframe"}}{A sample dataframe of what data could look like given the proposed parameters.}
#'   \item{\code{"design"}}{\code{aov} The design string, e.g. "2b*2w".}
#'   \item{\code{"design_list"}}{The list of variables in the design.}
#'   \item{\code{"frml1"}}{The first formula created for this design.}
#'   \item{\code{"frml2"}}{The second formula created for this design.}
#'   \item{\code{"mu"}}{Vector of means.}
#'   \item{\code{"sd"}}{Vector of standard deviations.}
#'   \item{\code{"r"}}{Common correlation coefficient.}
#'   \item{\code{"n"}}{Sample size per cell. Can be entered as a single value or list of sample sizes for each condition. If unequal n is entered then the design can only be passed onto ANOVA_power.}
#'   \item{\code{"cor_mat"}}{The correlation matrix.}
#'   \item{\code{"sigmatrix"}}{The variance-covariance matrix.}
#'   \item{\code{"design_factors"}}{Total number of within-subjects factors.}
#'   \item{\code{"labelnames"}}{List of the label names.}
#'   \item{\code{"labelnameslist"}}{Secondary list of labelnames}
#'   \item{\code{"factornames"}}{List of the factor titles.}
#'   \item{\code{"meansplot"}}{Plot of the experimental design.}
#'
#' }

#' @examples
#'
#' @section Warnings:
#' Varying the sd or r (e.g., entering multiple values) violates assumptions of homoscedascity and sphericity respectively
#' @importFrom stats pnorm pt qnorm qt as.formula median
#' @import ggplot2
#' @export
#'


agree_test <- function (data, x,y,
  alpha=0.05,prop0=0.8,delta=.1,n=15,xbar=0.011,s=0.044) {
  #USER SPECIFICATIONS PORTION
  #alpha<-0.05 #DESIGNATED ALPHA
  #prop0<-0.8 #NULL CENTRAL PROPORTION
  #delta<-0.1 #THRESHOLD
  #n<-15 #SAMPLE SIZE
  #xbar<-0.011 #SAMPLE MEAN
  #s<-0.044 #SAMPLE STANDARD DEVIATION
  #END OF SPECIFICATION
  pct<-1-(1-prop0)/2
  zp<-qnorm(pct)
  df<-n-1
  stdh<-s/sqrt(n)
  numint<-1000
  coevec<-c(1,rep(c(4,2),numint/2-1),4,1)
  cl<-1e-6
  cu<-qchisq(1-cl,df)
  int<-cu-cl
  intl<-int/numint
  cvec<-cl+intl*(0:numint)
  wcpdf<-(intl/3)*coevec*dchisq(cvec,df)
  fungam<-function () {
    gaml<-0
    gamu<-100
    loop<-0
    dalpha<-1
    while(abs(dalpha)>1e-8 | dalpha<0){
      gam<-(gaml+gamu)/2
      h<-zp*sqrt(n)-gam*sqrt(cvec/df)
      ht<-h*(cvec<n*df*(zp/gam)^2)
      alphat<-sum(wcpdf*(2*pnorm(ht)-1))
      if (alphat>alpha) gaml<-gam else gamu<-gam
      loop<-loop+1
      dalpha<-alphat-alpha
    }

    return(gam)
  }
  gam<-fungam()
  el<-xbar-gam*stdh
  eu<-xbar+gam*stdh
  rej<-(-delta<el)*(eu<delta)
  if (rej==1) test=c('reject ho') else test=("don't reject h0")
  print(c('gam, el, eu'))
  print(c(gam,el,eu),digits=3)
  print(c('test:',test))
}
