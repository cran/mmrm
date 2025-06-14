% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testing.R
\name{h_test_md}
\alias{h_test_md}
\title{Creating F-Statistic Test Results For Multi-Dimensional Contrast}
\usage{
h_test_md(object, contrast, df, f_stat_factor = 1)
}
\arguments{
\item{object}{(\code{mmrm})\cr the MMRM fit.}

\item{contrast}{(\code{matrix})\cr numeric contrast matrix.}

\item{df}{(\code{number})\cr denominator degrees of freedom for the multi-dimensional contrast.}

\item{f_stat_factor}{(\code{number})\cr optional scaling factor on top of the standard F-statistic.}
}
\value{
List with \code{num_df}, \code{denom_df}, \code{f_stat} and \code{p_val} (2-sided p-value).
}
\description{
Creates a list of results for multi-dimensional contrasts using
an F-test statistic and the given degrees of freedom.
}
\keyword{internal}
