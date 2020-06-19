#' Transforms binary data 
#'
#' @param data input \code{data.frame}
#'
#' @return data.frame
#' @import dplyr 
#' @export
process_binary <- function(data){
  
  dmtc <- binary_data %>%
    dplyr::transmute(study, treatment, responders = x, sampleSize = n) %>%
    as.data.frame() # needed for nodesplit(.) function (fails for `tibble' input)
  
  return(dmtc)
}
