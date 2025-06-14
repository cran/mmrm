% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/satterthwaite.R
\name{h_gradient}
\alias{h_gradient}
\title{Computation of a Gradient Given Jacobian and Contrast Vector}
\usage{
h_gradient(jac_list, contrast)
}
\arguments{
\item{jac_list}{(\code{list})\cr Jacobian list produced e.g. by \code{\link[=h_jac_list]{h_jac_list()}}.}

\item{contrast}{(\code{numeric})\cr contrast vector, which needs to have the
same number of elements as there are rows and columns in each element of
\code{jac_list}.}
}
\value{
Numeric vector which contains the quadratic forms of each element of
\code{jac_list} with the \code{contrast} vector.
}
\description{
Computes the gradient of a linear combination of \code{beta} given the Jacobian matrix and
variance parameters.
}
\keyword{internal}
