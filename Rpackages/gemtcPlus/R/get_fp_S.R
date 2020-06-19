#' Calculate the survivor functions estimated in a fractional polynomial NMA model. The absolute S(t) estimates combining the estimated baseline survival
#' from a reference trial (in the NMA) with the fractional polynomial (log)hazard ratio estimates to construct the S(t) functions for each treatment.
#'
#' @param fit       JAGS object with NMA fit.
#' @param ref.std   Numeric identifier of reference study to use for baseline survival estimate.
#' @param ref.arm   Numeric identifier of arm in reference study to use for baseline survival estimate.
#' @param treatments Vector with character strings to label the treatments. If NULL treatments calculated from fit
#' @param time      Vector of time-points at which S(t) functions are calculated.
#' @param bl.node   Charactor to identify the node in the jags model that identifies the baseline estimates (default is "mu").
#' @param contrast.node Charactor to identify the node in the jags model that identifies the baseline estimates (default is "d").
#' 
#' @details         The FP parameters to calculate the log-hazard curves for each survivor function are combinations \code{bl.node + contrast.node}. This means the combination \code{ref.std, ref.arm} must identify a study arm in the NMA that used the (NMA) reference treatment. The basic parameters (contrasts vs reference) are then added to optain the parameters for each treatment in the study. 
#'
#' @return          A \code{data.frame} containing survivor function for each treatment
#' @export
#'

get_fp_S <- function(fit, ref.std, ref.arm, treatments = NULL, time, bl.node = "mu", contrast.node = "d"){

  if(is.null(treatments)){
    treatments <- unique(attr(fit$data.jg, "d_arms")$treatment[order(attr(fit$data.jg, "d_arms")$treatmentn)]) 
  }
  
  exponents <- fit$model.pars$exponents
  if (length(exponents) != 1 & length(exponents) != 2){
    out <- NULL
    warning("Fractional polynomial must be of 1st or 2nd order")
  }
  
  if (length(exponents) == 1){
    out <- get_fp_1o_S(fit = fit, 
                       ref.std = ref.std, 
                       ref.arm = ref.arm, 
                       treatments = treatments, 
                       time = time,
                       bl.node = bl.node, 
                       contrast.node = contrast.node)
  } 
  
  if (length(exponents) == 2){
    out <- get_fp_2o_S(fit = fit, 
                       ref.std = ref.std, 
                       ref.arm = ref.arm, 
                       treatments = treatments,
                       time = time,
                       bl.node = bl.node, 
                       contrast.node = contrast.node)
  }
  
  return(out)
}
