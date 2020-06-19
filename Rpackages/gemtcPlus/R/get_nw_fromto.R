#' Extract edges information ("from-to matrix") from network data frame. 
#'
#' @param dat \code{data.frame} with netowrk data in long format. Must contain columns study and treatment. Can be IPD, grouped data, or AD.
#'
#' @return A \code{matrix} with columns "from", "to".
#' @export
#'

get_nw_fromto <- function(dat){
  
`%>%` <- magrittr::`%>%`

## limit to relevant information: study x treatment
dat <- dat %>%
  dplyr::select(study, treatment) %>%
  dplyr::group_by(study, treatment) %>%
  dplyr::slice(1)
  
## edges for 2-arm studies
anker <- dat %>%
  dplyr::group_by(study) %>%
  dplyr::slice(1)

distant <- dat %>%
  dplyr::group_by() %>%
  dplyr::filter(duplicated(study))

fromto <- dplyr::right_join(anker, distant, by = "study")

## additional edges for multi-arm studies
while(any(duplicated(distant$study))){
  anker <- distant %>%
    dplyr::group_by(study) %>%
    dplyr::slice(1)
  
  distant <- distant %>%
    dplyr::group_by() %>%
    dplyr::filter(duplicated(study))
  
  fromto_now <- dplyr::right_join(anker, distant, by = "study")
  fromto <- rbind(fromto, fromto_now)
}

out <- fromto %>%
  dplyr::group_by() %>%
  dplyr::select(-study) %>%
  as.matrix()
colnames(out) <- c("from", "to")

return(out)
}


