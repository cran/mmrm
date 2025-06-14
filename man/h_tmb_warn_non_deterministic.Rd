% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{h_tmb_warn_non_deterministic}
\alias{h_tmb_warn_non_deterministic}
\title{Warn if TMB is Configured to Use Non-Deterministic Hash for Tape Optimizer}
\usage{
h_tmb_warn_non_deterministic()
}
\value{
No return value, called for side effects.
}
\description{
This function checks the TMB configuration for the \code{tmbad_deterministic_hash} setting
If it is set to \code{FALSE}, a warning is issued indicating that this may lead to
unreproducible results.
}
\keyword{internal}
