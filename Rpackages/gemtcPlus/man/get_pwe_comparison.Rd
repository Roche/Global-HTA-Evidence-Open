% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pwe_comparison.R
\name{get_pwe_comparison}
\alias{get_pwe_comparison}
\title{Extract model information and fit statistics from a list of piecewise-exponential NMA fits.}
\usage{
get_pwe_comparison(nmaout.list)
}
\arguments{
\item{nmaout.list}{List of results lists (which must contain the names elements `descr_s, model.pars, fit, DICsamp`).}
}
\value{
matrix comparing the models
}
\description{
Extract model information and fit statistics from a list of piecewise-exponential NMA fits.
}
