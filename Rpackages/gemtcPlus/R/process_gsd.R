#' Transforms grouped survival data 
#'
#' @param data input \code{data.frame}
#'
#' @return data.frame
#' @export
#' 
#'
process_gsd <- function(data){
  #TODO: add a check for required column names
  
  discr_surv <- data %>%
    mutate(h = n.event / n.risk) %>%
    group_by(study, treatment) %>%
    do(data.frame(
      t.start = .$t.start,         # needed for merging later on 
      S.start = cumprod(c(1, 1 - .$h))[seq_len(nrow(.))],
      S.end   = cumprod(1 - .$h))
    )
  
  return(left_join(data, discr_surv, by = c("study", "treatment", "t.start")))
}
