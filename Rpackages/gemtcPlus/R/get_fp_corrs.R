#' Calculate correlations between the contrast estimates for multi-dimensional effect estimates for all treatments in a FP NMA.
#'
#' @param fit   Jags output 
#' @param node  The name of the node with the contrasts (default is "d").
#' 
#' @return          a \code{data.frame} containing the posterior correlations
#' @export
#'
#' @examples
#' 
get_fp_corrs <- function(fit, node = "d") {
  d <- fit$BUGSoutput$sims.list[[node]]
  
  cors_all <- array(NA, dim = c(dim(d)[2], dim(d)[3], dim(d)[3])) 
  for (i in 1:dim(d)[2]){
    cors_i <- cor(d[,i,])
    cors_all[i,,] <- round(cors_i, 3)
  }
  
  treatments <- unique(attr(fit$data.jg, "d_arms")$treatment[order(attr(fit$data.jg, "d_arms")$treatmentn)]) # ensure order is same as in JAGS fit
  n_par <- dim(d)[3]
  par_labs <- c("Int", paste("Slope", 1:(n_par - 1), sep = ""))
  
  
  dimnames(cors_all) <- list(treatment = treatments, coef = par_labs, coef = par_labs)
  
  return(cors_all)
}
