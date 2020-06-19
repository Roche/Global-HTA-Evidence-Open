#' Utility function providing pairwise probability of being better (col vs row). (Adapted from gemtc::relative.effect.table()).
#'
#' @param x            Object of class \code{mtc.result} which is the output from a \code{mtc.run}.
#' @param smaller.is.better    Logical (default is TRUE).
#' @param threshold    Numerical value (default is 0) giving the threshold against which contrasts are being compared.
#' @param covariate    NA; for compatibility with gemtc::relative.effect.table().
#'
#' @return             An array of class `mtc.prob.better.table`
#' @export
#' @importFrom utils combn
#' @importFrom gemtc relative.effect
#'

mtc.prob.better.table <- function(x, smaller.is.better, threshold = 0, covariate = NA){

  ts <- as.character(x[["model"]][["network"]][["treatments"]][["id"]])
  tbl <- array(NA, dim = c(length(ts), length(ts)), dimnames = list(ts, ts))

  # pairwise comparisons: column better than row
  comps <- combn(ts, 2)
  for (i in 1:ncol(comps)) {
    comp <- comps[, i]
    samples <- as.matrix(relative.effect(x, comp[1], comp[2], preserve.extra = FALSE, covariate = covariate)$samples)

    if (smaller.is.better){
      samplesBetter <- samples <= threshold
    } else {
      samplesBetter <- samples >= threshold
    }

    probBetter <- mean(samplesBetter)

    tbl[comp[1], comp[2]] <- unname(probBetter)
    probWorse <- 1 - probBetter
    tbl[comp[2], comp[1]] <- unname(probWorse)
  }
  attr(tbl, "model") <- x[["model"]]
  attr(tbl, "covariate") <- covariate
  class(tbl) <- "mtc.prob.better.table"
  tbl
}

#' Utility function to extract probabilities of new treatment being better from gemtc fit (e.g. P(HR<1) for HRs new vs other).
#'
#' @param x            Object of class mtc.result containing the NMA fit.
#' @param new.lab      Character string with name of new intervention.
#' @param smaller.is.better    Logical (default is TRUE).
#' @param threshold    Numerical value (default is 0) giving the threshold against which contrasts are being compared.
#' @param sort.by      Character string, must be either "name" or "effect" (default), indicating the sorting of the output table.
#' @param digits       Integer (default is 3); if provided, outputs will be rounded accordingly.
#'
#' @return             A data.frame of probabilities of a treatment being better from a mtc model
#' @export
#'

get_mtc_probBetter <- function(x, new.lab,
                               smaller.is.better = TRUE,
                               threshold = 0,
                               sort.by = c("name", "effect")[1],
                               digits = 3){
  out <- mtc.prob.better.table(x,
                               smaller.is.better = smaller.is.better,
                               threshold = threshold)[, new.lab]
  out <- na.omit(out)

  if (!is.null(digits)){
    out <- round(out, digits)
  }

  out <- data.frame(New = new.lab,
                    Comparator = names(out),
                    probNewBetter = out,
                    stringsAsFactors = FALSE)

  colnames(out) <- c("New", "Comparator", "probNewBetter")
  rownames(out) <- NULL

  if (sort.by == "name"){
    ord <- order(as.character(out$Comparator))
  }
  if (sort.by == "effect"){
    ord <- rev(order(out$probNewBetter))
  }
  out <- out[ord,]

  attr(out, "comparison") <- paste(new.lab, "vs other")
  return(out)
}
