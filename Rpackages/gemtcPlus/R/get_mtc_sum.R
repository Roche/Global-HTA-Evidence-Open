#' Utility function to extract summary stats from mtc.result object.
#'
#' @param x            Object of class \code{mtc.result} which is the output from a \code{mtc.run}.
#' @param digits  Integer specifiying the number of digits for rounding (default is 2).
#'
#' @return        A data.frame of mtc model summary statistics
#' @export
#'

get_mtc_sum <- function(x, digits = 2){
  out <- data.frame(DIC = x$deviance$DIC,
                    pD = x$deviance$pD,
                    resDev = x$deviance$Dbar,
                    dataPoints = x$deviance[["data points"]])
  out <- round(out, digits)
  return(out)
}
