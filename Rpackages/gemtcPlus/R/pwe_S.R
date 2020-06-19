#' Calculate the survivor function S(t) from a piecewise exponential model.
#'
#' @param time        Time-point at which survivor fct is evaluated, S(time) = exp(-H(time)).
#' @param cut.pts     Vector of cut points.
#' @param haz.rates   Vector of hazard rates (must have one element more than the cut.pts vector).
#'
#' @return            \code{numeric} estimate of survivor function at time t, S(t)
#' @export
#'

pwe_S <- function(time, cut.pts, haz.rates){
  out <- exp(-pwe_H(time, cut.pts, haz.rates))
  return(out)
}

