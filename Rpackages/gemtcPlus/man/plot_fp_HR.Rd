% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_fp_HR.R
\name{plot_fp_HR}
\alias{plot_fp_HR}
\title{Produce ggplot from HR values in data.frame (medians vs time for several trts, all in one plot)}
\usage{
plot_fp_HR(
  HRs,
  xlab = "Month",
  legend.pos = "right",
  breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8),
  facet = FALSE,
  ncol = NULL
)
}
\arguments{
\item{HRs}{}
}
\value{
fig
}
\description{
Produce ggplot from HR values in data.frame (medians vs time for several trts, all in one plot)
}
