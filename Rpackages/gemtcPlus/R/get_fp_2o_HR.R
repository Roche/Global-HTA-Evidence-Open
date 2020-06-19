#' Calculate the time-dependent hazard ratios obtained from fitting a second order fractional polynomial model.
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
#' 
get_fp_2o_HR <- function(x, fit, trt.nos, ref.no, trt.labs = NULL, node = "d", CI = TRUE, revert = FALSE){

  `%>%` <- magrittr::`%>%`
  
  exponents <- fit$model.pars$exponents
  
  ## prepare the comparisons, derive labels
  if (is.null(trt.labs)){
    trt.labs <- as.character(trt.nos)
  }
  dict <- matrix(trt.labs, ncol = 1, dimnames = list(as.character(trt.nos), NULL))
  
  comp <- setdiff(trt.nos, ref.no)
  dcomp <- data.frame(comp = comp,
                      ref = ref.no,
                      comp.lab = dict[as.character(comp),],
                      ref.lab = dict[as.character(ref.no),]
  )
  if (revert){
    dcomp <- dcomp %>%
      dplyr::mutate(lab = paste(ref.lab, "vs", comp.lab),
                    lab_s = paste("vs", comp.lab))
  } else {
    dcomp <- dcomp %>%
      dplyr::mutate(lab = paste(comp.lab, "vs", ref.lab),
                    lab_s = paste(comp.lab, "vs"))
  }
  
  
  ## define the summary function
  if(CI){
    my_sum <- function(u){
      out <- c(median = median(u),
               lCI = quantile(u, probs = 0.025, names = FALSE),
               uCI = quantile(u, probs = 0.975, names = FALSE))
      return(out)
    }
  } else{
    my_sum <- function(u){
      out <- c(median = median(u))
      return(out)
    }
  }
  
  ## calculate the HRs and combine them all
  dhr <- data.frame()
  fit_ma <- as.matrix(as.mcmc(fit))
  for (i in 1:nrow(dcomp)){
    c_i <- comp[i]
    d1c_i <- paste(node, "[", c_i, ",", 1, "]", sep = "")
    d1ref <- paste(node, "[", ref.no, ",", 1, "]", sep = "")
    d2c_i <- paste(node, "[", c_i, ",", 2, "]", sep = "")
    d2ref <- paste(node, "[", ref.no, ",", 2, "]", sep = "")
    d3c_i <- paste(node, "[", c_i, ",", 3, "]", sep = "")
    d3ref <- paste(node, "[", ref.no, ",", 3, "]", sep = "")
    
    logHR_i <- get_fp_2o(x = x,
                         params = cbind(fit_ma[, d1c_i] - fit_ma[, d1ref],
                                        fit_ma[, d2c_i] - fit_ma[, d2ref],
                                        fit_ma[, d3c_i] - fit_ma[, d3ref]),
                         exponents = exponents,
                         sums = my_sum)
    
    if (revert) { logHR_i <- -logHR_i }
    
    HR_i <- data.frame(x = x,
                       exp(logHR_i),
                       dcomp[i,], 
                       row.names = NULL)
    dhr <- rbind(dhr, HR_i)
  }
  
  if (revert) {
    idLo <- which(names(dhr) == "lCI")
    idUp <- which(names(dhr) == "uCI")
    names(dhr)[idLo] <- "uCI"
    names(dhr)[idUp] <- "lCI"
  }
  
  return(dhr)
}
