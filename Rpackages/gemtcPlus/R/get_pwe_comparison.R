#'  Extract model information and fit statistics from a list of piecewise-exponential NMA fits.
#'
#' @param nmaout.list List of results lists (which must contain the names elements `descr_s, model.pars, fit, DICsamp`).
#'
#' @return matrix comparing the models
#' @export
#' 

get_pwe_comparison <- function(nmaout.list){

  all_ls <- lapply(nmaout.list, get_pwe_elements)
  all_ls2<- lapply(all_ls, FUN = function(x) t(as.matrix(x)))

  all_ma <- c()
  for (i in seq_along(all_ls2)) {
    all_ma <- rbind(all_ma, all_ls2[[i]])
  }

  return(all_ma)
}
