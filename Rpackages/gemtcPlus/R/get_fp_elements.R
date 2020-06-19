#'  Extract model information and fit statistics from NMA fit in jags of a fractional polynomial model.
#'
#' @param nmaout A list with the results from NMA fit (jags). The list must contain the names elements `descr_s, model.pars, fit, DICsamp`.
#'
#' @return list with description and fit metrics
#' @export
#' 

get_fp_elements <- function(nmaout){
  
  rc <- list(Model = nmaout$descr_s,
             Order = length(nmaout$model.pars$exponents),
             Exponents = paste(unlist(nmaout$model.pars$exponents), collapse = ", "),
             DIC = round(nmaout$BUGSoutput$DIC, 1),
             pD = round(nmaout$BUGSoutput$pD, 1),
             meanDev = round(nmaout$BUGSoutput$summary["deviance", "mean"], 1)
             )
  
  return(rc)
}
