% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testing.R
\name{df_1d}
\alias{df_1d}
\title{Calculation of Degrees of Freedom for One-Dimensional Contrast}
\usage{
df_1d(object, contrast)
}
\arguments{
\item{object}{(\code{mmrm})\cr the MMRM fit.}

\item{contrast}{(\code{numeric})\cr contrast vector. Note that this should not include
elements for singular coefficient estimates, i.e. only refer to the
actually estimated coefficients.}
}
\value{
List with \code{est}, \code{se}, \code{df}, \code{t_stat} and \code{p_val}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
Calculates the estimate, adjusted standard error, degrees of freedom,
t statistic and p-value for one-dimensional contrast.
}
\examples{
object <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
contrast <- numeric(length(object$beta_est))
contrast[3] <- 1
df_1d(object, contrast)
}
