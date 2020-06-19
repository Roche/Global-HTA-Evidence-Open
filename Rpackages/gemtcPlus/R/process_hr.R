#' Transforms hazard ratio data 
#'
#' @param data input \code{data.frame}
#'
#' @return data.frame
#' @import dplyr 
#' @export
process_hr <- function(data){
  #TODO: add a check for required column names
  
  darm2 <- data %>%
    dplyr::transmute(
      study,
      treatment = new,
      diff = lhr,
      std.err = lhrse
    )
  darm1 <- data %>%
    dplyr::transmute(
      study,
      treatment = ref,
      diff = NA,
      std.err = NA
    )
  
  dmtc <- rbind(darm1, darm2)
  return(dmtc)
}
