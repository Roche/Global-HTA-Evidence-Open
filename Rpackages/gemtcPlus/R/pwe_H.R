#' Calculate the cumulative hazard over [0, tmax] from piecewise constant model.
#'
#' @param time        Time-point at which cumulative hazard evaluated, H(time) = int_0^time h(u) du.
#' @param cut.pts     Vector of cut points.
#' @param haz.rates   Vector of hazard rates (must have one element more than the cut.pts vector).
#'
#' @return            \code{numeric} estimate of the hazard over [0, tmax]
#' @export
#'

pwe_Hu <- function(time, cut.pts, haz.rates){
  hf <- stepfun(x = cut.pts, y = haz.rates)
  H <- integrate(hf, lower = 0, upper = time)

  if(H$message != "OK") { warning(H$message) }

  return(H$value)
}
pwe_H <- Vectorize(pwe_Hu, vectorize.args = "time")
