% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{drop_elements}
\alias{drop_elements}
\title{Drop Items from an Indexible}
\usage{
drop_elements(x, n)
}
\arguments{
\item{x}{Any object that can be consumed by \code{\link[=seq_along]{seq_along()}} and indexed by a
logical vector of the same length.}

\item{n}{(\code{integer})\cr the number of terms to drop.}
}
\value{
A subset of \code{x}.
}
\description{
Drop elements from an indexible object (\code{vector}, \code{list}, etc.).
}
\keyword{internal}
