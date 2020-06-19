#' Extract the treatment contrasts vs the reference in the network
#'
#' @param fit.jg       The jags output object.
#' @param treatments   A vector with the labels for the treatments in the network (in the correct order). If NULL treatments calculate from fit
#' @param node         The name of the node identifying the contrast.
#' @export
#' 
get_fp_contrasts <- function(fit.jg, treatments = NULL, node = "d", revert = FALSE){
  
  if(is.null(treatments)){
    treatments <- unique(attr(fit.jg$data.jg, "d_arms")$treatment[order(attr(fit.jg$data.jg, "d_arms")$treatmentn)]) 
  }
  Ntx <- length(treatments)
  
  sel_d <- grep("d[", rownames(fit.jg$BUGSoutput$summary), fixed = TRUE)
  d <- round(fit.jg$BUGSoutput$summary[sel_d, c("50%", "2.5%", "97.5%")], 3)
  
  if (revert) {
    d <- -d
    d <- d[, c(1, 3, 2)] # lower and upper CIs get reversed
  }
  
  colnames(d) <- c("med", "lCI", "uCI")
  
  
  ddl <- data.frame(Treatment = treatments,
                    d)
  
  order <- nrow(ddl) / Ntx - 1
  
  if (order == 0) {
    ddw <- ddl
  } else {
    dd.int <- ddl[1:Ntx,]
    colnames(dd.int)[-1] <- paste("Int", colnames(d), sep = ".") 
    dd.s <- list()
    for(i in 1:order){
      dd.s[[i]] <- ddl[i*Ntx + 1:Ntx,]
      colnames(dd.s[[i]])[-1] <- paste(paste("Slope", i, sep = ""), colnames(dd.s[[i]])[-1], sep = ".")
    }
    ddw <- dd.int
    for(i in 1:order){
      ddw <- left_join(ddw, dd.s[[i]], by = "Treatment")
    }
  }
  
  return(ddw[-1,])
}
