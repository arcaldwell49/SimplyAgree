#' @title Data
#' @description A dataset from a study on the reliability of human body temperature at different times of day before and after exercise.
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
#' @source Ravanelli N, Jay O. The Change in Core Temperature and Sweating Response during Exercise Are Unaffected by Time of Day within the Wake Period. Med Sci Sports Exerc. 2020 Dec 1. doi: 10.1249/MSS.0000000000002575. Epub ahead of print. PMID: 33273272.
"temps"

#' @rdname temps
"recpre_long"

#' @title reps
#' @description A fake data set of a agreement study where both measures have replicates.
#' @format A data frame with 20 rows with 3 variables
#' \describe{
#'   \item{id}{Subject identifier}
#'   \item{x}{X measurement}
#'   \item{y}{Y measurement}

#'
#'
#' }
#' @rdname Data
"reps"


#' @title ba1986
#' @description The data set published in the original Bland & Altman paper on agreement.
#' @format A data frame with 17 rows with 5 variables
#' \describe{
#'   \item{id}{Subject identifier}
#'   \item{wright1}{PERF measurement #1 using Wright device}
#'   \item{wright2}{PERF measurement #2 using Wright device}
#'   \item{mini1}{PERF measurement #1 using Mini device}
#'   \item{mini2}{PERF measurement #2 using Mini device}
#'  }
#' @section References:
#'
#' Bland, J. M., & Altman, D. (1986). Statistical methods for assessing agreement between two methods of clinical measurement. The Lancet, 327(8476), 307-310.
#'
#'
#'
#'
#' @rdname Data
"ba1986"

