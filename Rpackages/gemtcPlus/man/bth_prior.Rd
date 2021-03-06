% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bth_prior.R
\name{bth_prior}
\alias{bth_prior}
\title{Creates a fractional polynomial model plan}
\usage{
bth_prior(model = "PWE", bth.model = "RE", type, distr, param)
}
\arguments{
\item{model}{One of "PWE" or "FP"}

\item{bth.model}{If model is PWE then must be "RE". If model is FP then One of "RE" or "REINT"}

\item{type}{If PWE RE or FP REINT then one of "sd" or "var". If FP RE then "vcov"}

\item{distr}{If type is "sd" then one of "unif" or "hn". If type is "var" then "ln". If typeis "vcov" then "invwish"}

\item{param}{A \code{list} with ... if distibution is "unif" then named values min, max (numeric > 0). If distribution is hn or ln then list must contain mean and precision.
Of distribution in invwish then names value "scale" (a matrix).}
}
\description{
Creates a fractional polynomial model plan
}
