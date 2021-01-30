#' @title temps
#' @description A dataset from a study on the reliability of human body temperature at diferents times of day before and after exercise.
#' @format A data frame with 60 rows and 10 variables:
#' \describe{
#'   \item{id}{Subject identifier}
#'   \item{trial_num}{order in which the experimental trial was completed}
#'   \item{trial_condition}{Environmental condition and metabolic heat production}
#'   \item{tod}{Time of Day}
#'   \item{trec_pre}{Rectal temperature before the beginning of the trial}
#'   \item{trec_post}{Rectal temperature at the end of the trial}
#'   \item{trec_delta}{Change in rectal temperature}
#'   \item{teso_pre}{Esophageal temperature before the beginning of the trial}
#'   \item{teso_post}{Esophageal temperature at the end of the trial}
#'   \item{teso_delta}{Change in esophageal temperature}
#'
#' }
#' @source \url{https://doi.org/10.1249/MSS.0000000000002575}
"temps"

#' @rdname temps
"recpre_long"

#' @title reps
#' @description A fake data set of a agreement study where X is measured in duplicate and Y is measured in triplicate
#' @format A data frame with 11 rows and 5 columns:
#' \describe{
#'   \item{X1}{X first measurement}
#'   \item{X2}{X second measurment}
#'   \item{Y1}{Y first measurement}
#'   \item{Y2}{Y second measurement}
#'   \item{Y3}{Y third measurement}
#'
#'
#' }
"reps"
