#' Calculate the survivor functions estimated in a 2nd order fractional polynomial NMA model. The absolute S(t) estimates combining the estimated baseline survival
#' from a reference trial (in the NMA) with the fractional polynomial (log)hazard ratio estimates to construct the S(t) functions for each treatment.
#'
#' @param fit       JAGS object with FP NMA fit.
#' @param ref.std   Numeric identifier of reference study to use for baseline survival estimate.
#' @param ref.arm   Numeric identifier of arm in reference study to use for baseline survival estimate.
#' @param treatments Vector with character strings to label the treatments.
#' @param time      Vector of time-points at which S(t) functions are calculated.
#' @param bl.node   Charactor to identify the node in the jags model that identifies the baseline estimates (default is "mu").
#' @param contrast.node Charactor to identify the node in the jags model that identifies the baseline estimates (default is "d").
#' 
#' @details         The FP parameters to calculate the log-hazard curves for each survivor function are combinations \code{bl.node + contrast.node}. This means the combination \code{ref.std, ref.arm} must identify a study arm in the NMA that used the (NMA) reference treatment. The basic parameters (contrasts vs reference) are then added to optain the parameters for each treatment in the study. 
#'
#' @return          a \code{data.frame} containing survivor function for each treatment
#' @export
#'
#' @examples
#' 
get_fp_2o_S <- function(fit, ref.std, ref.arm, treatments,
                        time = 1:24, bl.node = "mu", contrast.node = "d"){
  
  ## check order of FP
  exponents <- fit$model.pars$exponents
  if (length(exponents) != 2){
    out <- NULL
    warning("Fractional polynomial must be of 2nd order")
  }
  
  
  ## define the summary function used later on
  my_sum <- function(u){
    out <- c(S = median(u),
             lCrI = quantile(u, probs = 0.025, names = FALSE),
             uCrI = quantile(u, probs = 0.975, names = FALSE))
    return(out)
  }
  
  
  ## extract MCMC chains for baseline and contrast paramters to construct coefficients in FP
  mu <- fit$BUGSoutput$sims.list[[bl.node]][, ref.std, ]
  d  <- fit$BUGSoutput$sims.list[[contrast.node]]
  n_trt  <- length(treatments)
  
  
  ## calculate treatment specific NMA estimates (FP parameters = baseline + contrast)
  ## and plug them into the fractional polynomial to get the log-hazard functions
  dS <- data.frame()
  time0 <- c(0, time)
  dt <- diff(time0)
  for (i in 1:n_trt){
    beta_i <- mu + d[,i,]
    loghaz_i <- get_fp_2o(x = time,
                          params = beta_i,
                          exponents = exponents)
    
    haz_i <- exp(loghaz_i)
    
    dH_i <- dt * haz_i # approximate the cumulative hazard (for every MCMC iteration); first calculate the incerments over every interval, then sum up
    H_i  <- apply(dH_i, MAR = 2, cumsum)
    S_i  <- exp(-H_i)
    S_is <- t(apply(S_i, MAR = 1, FUN = my_sum))
    S_id <- data.frame(time = time,
                       S_is,
                       treatment = treatments[i])
    dS <- rbind(dS, S_id)
  }
  
  return(dS)
}
