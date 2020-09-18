#' Plots g-estimation from rpsftm object
#'
#' This function returns a ggplot derived from an rpsftm object showing psi vs test statistic. In case of convergence should see a unique solution.
#'
#' @param x rpsftm object (rpsftm)
#' @keywords survival, rpsftm
#' @export
#' @examples
#' # use data included in rpsftm package
#' library(rpsftm)
#' immdef <- rpsftm::immdef
#' immdef$rx <- with(immdef, 1 - xoyrs/progyrs)
#'
#' # fit the model
#' imm.fit <- rpsftm(Surv(progyrs, prog) ~ rand(imm, rx),
#'                   data=immdef,
#'                   censor_time=censyrs)
#'
#' ggpsi(imm.fit)
#' @import survival
#' @import rpsftm
#' @import ggplot2
#' @import dplyr

ggpsi <- function(x){
  if (!("rpsftm" %in% class(x))){
    stop("Please provide an rpsftm object.")
  }

  df <- x$eval_z
  rc <- ggplot(df, aes(x = psi, y = Z)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    geom_hline(yintercept = c(-1.96, 1.96), linetype = 2) +
    geom_vline(xintercept = x$CI, linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = x$psi) +
    xlab( expression(paste(psi))) +
    ylab( expression(paste(Z(psi)))) +
    ggtitle(expression(paste("Tested values for ", psi)), subtitle = "dashed lines indicate 95% CI")

  return(rc)

}
