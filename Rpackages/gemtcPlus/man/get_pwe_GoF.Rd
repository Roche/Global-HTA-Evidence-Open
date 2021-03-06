% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pwe_GoF.R
\name{get_pwe_GoF}
\alias{get_pwe_GoF}
\title{Calculate the survivor function estimates for each study and arm. Calculate also the observed survival curves
from the binned KM data to compare observed and estimated survivor functions.}
\usage{
get_pwe_GoF(
  fit,
  data.arms,
  data.jg,
  time = 0:60,
  bl.node = "mu",
  contrast.node = "d"
)
}
\arguments{
\item{fit}{JAGS object with NMA fit.}

\item{data.arms}{Data frame with one line per study arm and columns study, treatment, studyn, treatmentn.}

\item{data.jg}{List with input data set that was used in jags fit.}

\item{time}{Vector of time-points at which S(t) functions are calculated.}

\item{bl.node}{Charactor to identify the node in the jags model that identifies the baseline estimates (default is "mu").}

\item{contrast.node}{Charactor to identify the node in the jags model that identifies the baseline estimates (default is "d").}
}
\value{
data.frame
}
\description{
Calculate the survivor function estimates for each study and arm. Calculate also the observed survival curves
from the binned KM data to compare observed and estimated survivor functions.
}
