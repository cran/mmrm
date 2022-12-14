#' `mmrm` Package
#'
#' `mmrm` implements mixed models with repeated measures (MMRM) in R.
#'
#' @aliases mmrm-package
"_PACKAGE"

#' @useDynLib mmrm, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @import checkmate
#' @importFrom lifecycle deprecated
#' @importFrom methods is
#' @importFrom numDeriv genD
#' @importFrom stats acf
#' @importFrom stringr boundary
#' @importFrom parallel clusterApply
#' @importFrom Rdpack reprompt
#' @importFrom utils modifyList
NULL
