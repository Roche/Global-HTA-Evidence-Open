#' NMA data pre-processing
#'
#' @param data input \code{data.frame}
#' @param plan A \code{list} containing the model plan 
#'
#' @return A network object or if gsd a list containing a network object and jags init parameters
#' @export
#' @importFrom network network
#' @importFrom gemtc mtc.network
#' 
#' @seealso 
#' \code{\link{groupedTTE_fp_pre_proc}}, \code{\link{groupedTTE_pwe_pre_proc}}
#'

nma_pre_proc <- function(data, plan){
 stopifnot(plan$analysis %in% c("BINARY", "HR", "GSD"))
  
  if(plan$analysis %in% c("BINARY", "HR")){
    
   data <- switch(plan$analysis, 
                  "BINARY" = process_binary(data),
                  "HR"     = process_hr(data))
   
   network_input <- list(data)
   names(network_input) <- plan$binary_data_type
   
   nw <- do.call(gemtc::mtc.network, network_input)
   
   return(list(fitting_data = nw, plan = plan))
   
 } else if(plan$analysis == "GSD"){
    stopifnot(plan$model %in% c("FP", "PWE"))
   
    data <- process_gsd(data)
    plan$params$jags_init_params$dat <- data # Add data to plan 
    
    jags_out <- switch(plan$model,
                       "FP" = do.call(groupedTTE_fp_pre_proc, plan$params$jags_init_params),
                       "PWE" = do.call(groupedTTE_pwe_pre_proc, plan$params$jags_init_params))
    #Create network
    ft <- get_nw_fromto(data) 
    nw <- network::network(ft, matrix.type = "edgelist", directed = FALSE)
    
    return(list(fitting_data= jags_out, network = nw, plan = plan))
  }
}
