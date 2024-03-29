% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blandPowerCurve.R
\name{blandPowerCurve}
\alias{blandPowerCurve}
\title{Power Curve for Bland-Altman Limits of Agreement}
\usage{
blandPowerCurve(
  samplesizes = seq(10, 100, 1),
  mu = 0,
  SD,
  delta,
  conf.level = 0.95,
  agree.level = 0.95
)
}
\arguments{
\item{samplesizes}{vector of samples sizes at which to estimate power.}

\item{mu}{mean of differences}

\item{SD}{standard deviation of differences}

\item{delta}{The threshold below which methods agree/can be considered equivalent, can be in any units. Equivalence Bound for Agreement. More than one delta can be provided.}

\item{conf.level}{the confidence level(s) required. Default is 95\%. More than one confidence level can be provided.}

\item{agree.level}{the agreement level(s) required. Default is 95\%. The proportion of data that should lie between the thresholds, for 95\% limits of agreement this should be 0.95. More than one confidence level can be provided.}
}
\value{
A dataframe is returned containing the power analysis results. The results can then be plotted with the plot.powerCurve function.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#maturing}{\figure{lifecycle-maturing.svg}{options: alt='[Maturing]'}}}{\strong{[Maturing]}}
This function calculates the power for the Bland-Altman method under varying parameter settings and for a range of sample sizes.
}
\examples{
\donttest{
powerCurve <- blandPowerCurve(samplesizes = seq(10, 200, 1),
mu = 0,
SD = 3.3,
delta = 8,
conf.level = .95,
agree.level = .95)
# Plot the power curve
plot(powerCurve, type = 1)
# Find at what N power of .8 is achieved
find_n(powerCurve, power = .8)
}
# If the desired power is not found then
## Sample size range must be expanded
}
\references{
Lu, M. J., et al. (2016). Sample Size for Assessing Agreement between Two Methods of Measurement by Bland-Altman Method. The international journal of biostatistics, 12(2),
\doi{10.1515/ijb-2015-0039}
}
