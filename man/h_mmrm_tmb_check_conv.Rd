% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmb.R
\name{h_mmrm_tmb_check_conv}
\alias{h_mmrm_tmb_check_conv}
\title{Checking the \code{TMB} Optimization Result}
\usage{
h_mmrm_tmb_check_conv(tmb_opt, mmrm_tmb)
}
\arguments{
\item{tmb_opt}{(\code{list})\cr optimization result.}

\item{mmrm_tmb}{(\code{mmrm_tmb})\cr result from \code{\link[=h_mmrm_tmb_fit]{h_mmrm_tmb_fit()}}.}
}
\value{
Nothing, only used to generate warnings in case that the model
did not converge.
}
\description{
Checking the \code{TMB} Optimization Result
}
\keyword{internal}
