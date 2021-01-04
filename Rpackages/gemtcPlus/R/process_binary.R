#' Transforms binary data 
#'
#' @param data input \code{data.frame}
#'
#' @return data.frame
#' @import dplyr 
#' @export
process_binary <- function(data){
  
  if(!("x" %in% names(data))) stop("Binary arm-level data must contain named column 'x'.")
  if(!("n" %in% names(data))) stop("Binary arm-level data must contain named column 'n'.")
  if(!("study" %in% names(data))) stop("Binary arm-level data must contain named column 'study'.")
  if(!("treatment" %in% names(data))) stop("Binary arm-level data must contain named column 'treatment'.")
  
  dmtc <- data %>%
    dplyr::transmute(study, treatment, responders = x, sampleSize = n) %>%
    as.data.frame() # needed for nodesplit(.) function (fails for `tibble' input)
  
  return(dmtc)
}
