#' Utility function to extract effect estimates "new vs other treatments" from gemtc fit.
#'
#' @param x            Object of class \code{mtc.result} which is the output from a \code{mtc.run}.
#' @param new.lab      Character string with name of new intervention.
#' @param transform    Optional name of transformation to apply to output (e.g. "exp").
#' @param digits       Optional integer number of digits to round the output to.
#'
#' @return             Returns a data.frame of effect estimates
#' @export
#' @importFrom stats na.omit
#' @importFrom gemtc relative.effect.table
#'


get_mtc_newVsAll <- function(x, new.lab, transform = NULL, digits = NULL){
  out <- relative.effect.table(x)[, new.lab, c("50%", "2.5%", "97.5%")]
  out <- na.omit(out)

  if (!is.null(transform)){
    out <- do.call(transform, list(out))
  }

  if (!is.null(digits)){
    out <- round(out, digits)
  }

  out <- data.frame(rownames(out),
                    out)
  rownames(out) <- NULL
  colnames(out) <- c("Comparator", "Med", "CIlo", "CIup")

  attr(out, "comparison") <- paste(new.lab, "vs other")
  attr(out, "transform") <- transform
  return(out)
}
