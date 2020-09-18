

#' Convert a survfit to a dataframe of survival and censoring data
#'
#' This function returns a data frame containing following variables derived from a survfit object. Extends the S3 survfit class.
#' \itemize{
#'    \item t - Time
#'    \item s - KM Survival estimate
#'    \item c - Censoring indicator - 0 is censored, 1 is event
#'    \item strata - character description of strata that survival estimates are for
#'    \item Plus all strata variables
#' }
#'
#' @param x survfit object (survival)
#' @keywords survival
#' @export
#' @examples
#' library(survival)
#' sf <- survfit(Surv(time, status)~x, data = survival::aml)
#' as.data.frame(sf)
#' @import survival
#' @import dplyr

as.data.frame.survfit <- function(x){

  if (!("survfit" %in% class(x))){
    stop("Please provide a survfit object.")
  }

  plot.df <- NULL

  if (is.null(x$strata)) {
    x$strata <- length(x$time)
    names(x$strata) <- "all=all"
  }

  # returns string w/o leading or trailing whitespace
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)

  # build a dataframe of decomposed strata
  n.strata  <- length(x$strata)
  n.stratum <- length(unlist(strsplit(names(x$strata)[1], ",")))
  stratum.text <- names(x$strata) %>%
    strsplit(",") %>%
    unlist() %>%
    strsplit("=") %>%
    unlist()
  stratum.names  <- stratum.text[seq(1,2*n.stratum,2)]
  stratum.levels <- trim(stratum.text[seq(1,2*n.stratum*n.strata,2)+1])
  dim(stratum.levels) <- c(n.stratum, n.strata)

  strata.df <- cbind(strata = names(x$strata), t(stratum.levels)) %>%
    as.data.frame(stringsAsFactors = FALSE)

  names(strata.df) <- c("strata", trim(stratum.names))

  # convert the survival info to a data frame including strata info

  for (i in 1:n.strata){

    this.sel = sum(x$strata[0:(i-1)]) + 1:x$strata[i]

    temp.df <- data.frame(t = c(0,x$time[this.sel]),
                          s = c(1,x$surv[this.sel]),
                          c = c(0, as.numeric(x$n.censor[this.sel]>=1)),
                          strata = names(x$strata[i]),
                          stringsAsFactors = FALSE)

    if (is.null(plot.df)){
      plot.df <- temp.df
    } else{
      plot.df <- rbind(plot.df, temp.df)
    }
  }

  # add in the strata decodes
  plot.df <- left_join(plot.df, strata.df, by = "strata")

  return(plot.df)
}
