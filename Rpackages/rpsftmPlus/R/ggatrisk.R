
#' Plots at risk status from an rpsftm object
#'
#' This function returns a ggplot derived from an rpsftm object showing status over time.
#' It assumes that only a single switch in treatment occurs i.e. a patient who is randomized to control but has 25% exposure to experimental is assumed to have switched at 0.75*event time.
#'
#' @param x rpsftm object (rpsftm)
#' @param eval.times - vector of times to evaluate at risk status for. Default is all event and censoring times.
#' @keywords survival, rpsftm
#' @export
#' @examples
#' # use data included in rpsftm package
#' immdef <- rpsftm::immdef
#' immdef$rx <- with(immdef, 1 - xoyrs/progyrs)
#'
#' # fit the model
#' imm.fit <- rpsftm::rpsftm(Surv(progyrs, prog) ~ rand(imm, rx),
#'                   data=immdef,
#'                   censor_time=censyrs)
#'
#' ggatrisk(imm.fit)
#' @import survival
#' @import rpsftm
#' @import ggplot2
#' @import dplyr

ggatrisk <- function(x, eval.times = NULL){

  if (!("rpsftm" %in% class(x))){
    stop("Please provide an rpsftm object.")
  }

  plot.df <- rpsftm.atrisk(x, eval.times = eval.times)

  # make a plot

  rc <- ggplot(data = plot.df, aes(x = Time)) +
    theme_bw() +
    facet_grid(~Arm) +
    geom_step(aes(y = Percent, color = Status, linetype = Status), direction = "hv") +
    xlab("Time") +
    ylab("Proportion of patients") +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = c("Black", "Red", "Dark Grey", "Blue"))

  return(rc)

}

