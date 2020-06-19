#'  Extract model information and fit statistics from NMA fit in jags of a piecewise-exponential model.
#'
#' @param nmaout A list with the results from NMA fit (jags). The list must contain the named elements `descr_s, model.pars, fit, DICsamp`.
#'
#' @return list with description and fit metrics
#' @export
#' 
get_pwe_elements <- function(nmaout){
  
  rc <- list(Model = nmaout$descr_s,
             CutPoints = paste(unlist(nmaout$model.pars), collapse = ", "),
             DIC = round(nmaout$BUGSoutput$DIC, 1),
             pD = round(nmaout$BUGSoutput$pD, 1),
             meanDev = round(nmaout$BUGSoutput$summary["deviance", "mean"], 1)
             )
  
  return(rc)
}
