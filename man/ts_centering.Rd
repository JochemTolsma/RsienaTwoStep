% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataprep.R
\name{ts_centering}
\alias{ts_centering}
\alias{ts_prep_dep}
\alias{ts_simij}
\alias{ts_prepdata}
\title{Data preparation}
\usage{
ts_centering(cov)

ts_prep_dep(cov)

ts_simij(cov, min = NULL, max = NULL)

ts_prepdata(ccovar)
}
\arguments{
\item{cov}{numeric, behavioral scores of actors}

\item{min}{numeric, minimum value of behavioral scores of actors. If \code{NULL} the empirically observed minimum is used.}

\item{max}{numeric, maximum value of behavioral scores of actors. If \code{NULL} the empirically observed maximum is used.}

\item{ccovar}{data frame with named time-constant covariates.}
}
\description{
\code{ts_prepdata} performs centering and similarity score and set attribute prepared to TRUE
\code{ts_centering} centers the variables before use.
\code{ts_simij} calculates the similarity scores before use.
}
\details{
I really need to update the dataprep part, so to have behavioral dependents
ccovars and time varying covars.
}
\examples{
ts_centering(cov=df_ccovar1[,"cov1"])
ts_simij(cov=df_ccovar1[,"cov2"])
ts_simij(cov=df_ccovar1[,"cov2"], min=-5, max=5)
}
