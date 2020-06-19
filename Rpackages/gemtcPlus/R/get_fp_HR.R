#' Calculate the time-dependent hazard ratios obtained from fitting a fractional polynomial model (first or second order).
#'
#' @param x          A vector with the dependent variable.
#' @param fit        An rjags object with the output from the JAGS fit.
#' @param trt.nos    A vector with the numerical treatment IDs for which the HRs shall be calculated (including the ref).
#' @param ref.no     An integer with the numerical ID of the reference for the HR calculations.
#' @param trt.labs   A character vector of same length as trt.nos with the treatment labels.
#' @param node       A character string that identifies the node in the JAGS model giving the treatment effect estimates.
#' @param CI         Logical, shall CIs for the fractional polynomial be given? (Medians are always provided.)
#'
#' @return A data frame with pointwise median (and CI) HRs for all comparisons of trt.nos vs ref.no.
#' @export
#' 
#' @details Requires the packages: dplyr, coda.
get_fp_HR <- function(x, fit, trt.nos, ref.no, trt.labs = NULL, node = "d", CI = TRUE, revert = FALSE){
  
  if(is.null(trt.labs)){
    trt.labs <- unique(attr(fit$data.jg, "d_arms")$treatment[order(attr(fit$data.jg, "d_arms")$treatmentn)]) 
  }
  
  exponents <- fit$model.pars$exponents
  if (length(exponents) != 1 & length(exponents) != 2){
    out <- NULL
    warning("Fractional polynomial must be of 1st or 2nd order")
  }
  
  if (length(exponents) == 1){
    out <- get_fp_1o_HR(x = x, 
                        fit = fit, 
                        trt.nos = trt.nos, 
                        ref.no = ref.no, 
                        trt.labs = trt.labs, 
                        node = node, 
                        CI = CI, 
                        revert = revert)
  } 
  
  if (length(exponents) == 2){
    out <- get_fp_2o_HR(x = x, 
                        fit = fit, 
                        trt.nos = trt.nos, 
                        ref.no = ref.no, 
                        trt.labs = trt.labs, 
                        node = node, 
                        CI = CI, 
                        revert = revert)
  }
  
  return(out)
}

