#' Utility function to get segments (as character strings) from vector with cutpoints
#'
#' @param cut.pts Vector with cut points
#'
#' @return \code{factor} of segments
#' @export
#'

get_segments <- function(cut.pts){
  cuts0 <- c(0, cut.pts)
  cutsInf <- c(cut.pts, Inf)
  segments <- vector()
  for (i in seq_along(cuts0)){
    seg_i <- paste("[", cuts0[i], ",", cutsInf[i], ")", sep = "")
    segments[i] <- seg_i
  }

  segments_f <- factor(segments,
                       ordered = TRUE,
                       levels = segments) # otherwise the ordering of the character strings is taken (so e.g. "[10, Inf)" < "[3, 10)" )

  return(segments_f)
}

#' Utility function to get segments (as character strings) from vector with cutpoints
#'
#' @param x NULL value
#' @param cuts Vector with cut points
#' @param right Logical 
#' @param ordered_results Logical
#'
#' @return \code{factor} of segments
#' @export
#'

get_pw_segments <- function(x = NULL,
                            cuts,
                            right = FALSE,
                            ordered_results = TRUE) {
  if(is.null(x)){

  } else {
  cuts <- c(0, cuts, Inf)
  segments <- cut(x,
                  breaks = cuts,
                  include.lowest = TRUE,
                  labels = 1:(length(cuts)-1),
                  right = right,
                  ordered_result = ordered_result
                  )
  }
    segments
}

