#' Calculate the study and arm level survivor functions estimates from a fractional polynomial NMA. These estimates provide the basis for a goodness-of-fit graph when plotted along with the input data.
#'
#' @param fit       JAGS object with FP NMA fit.
#' @param treatments Vector with character strings to label the treatments. Argument not currently used
#' @param time      Vector of time-points at which S(t) functions are calculated.
#' @param bl.node   Charactor to identify the node in the jags model that identifies the baseline estimates (default is "mu").
#' @param contrast.node Charactor to identify the node in the jags model that identifies the baseline estimates (default is "d").
#' 
#' @details         The FP parameters to calculate the log-hazard curves for each survivor function are combinations \code{bl.node + contrast.node}. Here, every study-specific baseline estimate is combined with the arm-specific contrast to obtain arm level NMA estimates.
#' @return          a \code{data.frame} containing survivor function for each treatment
#' @export
#'
#' @examples
#' 
get_fp_GoF <- function(fit, treatments,
                          time = 1:24, bl.node = "mu", contrast.node = "d"){

  exponents <- fit$model.pars$exponents
  if (length(exponents) != 1 & length(exponents) != 2){
    out <- NULL
    warning("Fractional polynomial must be of 1st or 2nd order")
  }
  
  if (length(exponents) == 1){
    out <- get_fp_1o_GoF(fit = fit, 
                         time = time,
                         bl.node = bl.node, 
                         contrast.node = contrast.node)
  } 
  
  if (length(exponents) == 2){
    out <- get_fp_2o_GoF(fit = fit, 
                         time = time,
                         bl.node = bl.node, 
                         contrast.node = contrast.node)
  }
  
  return(out)
}
