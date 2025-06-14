% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-formula.R
\name{h_add_terms}
\alias{h_add_terms}
\title{Add Formula Terms with Character}
\usage{
h_add_terms(f, adds, drop_response = FALSE)
}
\arguments{
\item{f}{(\code{formula})\cr a formula to be updated.}

\item{adds}{(\code{character})\cr representation of elements to be added.}

\item{drop_response}{(\code{flag})\cr whether response should be dropped.}
}
\value{
A new formula with elements in \code{drops} removed.
}
\description{
Add formula terms from the original formula with character representation.
}
\details{
Elements in \code{adds} will be added from the formula, while the environment
of the formula is unchanged. If \code{adds} is \code{NULL} or \code{character(0)}, the formula is
unchanged.
}
\keyword{internal}
