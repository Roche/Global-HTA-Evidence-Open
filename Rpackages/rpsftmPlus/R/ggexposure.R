
#' Plots exposure status from an rpsftm object
#'
#' This function returns a ggplot derived from an rpsftm object showing exposure to randomized and switch treatment over time.
#' It assumes that only a single switch in treatment occurs i.e. a patient who is randomized to control but has 25 percents exposure 
#' to experimental is assumed to have switched at 0.75 times the event time.
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
#' ggexposure(imm.fit)
#' @import survival
#' @import rpsftm
#' @import ggplot2
#' @import dplyr

ggexposure <- function(x){

  if (!("rpsftm" %in% class(x))){
    stop("Please provide an rpsftm object.")
  }

  df <- rpsftm.data(x)

  # for illustration of patterns
  # assumes that switch is unidirectional with
  # control -> experimental happening once
  # experimental -> control happening once

  exp.df <- df %>%
    group_by(.arm) %>%
    arrange(desc(time)) %>%
    mutate(uid = order(time, decreasing = TRUE)) %>%
    mutate(cnt.start =  ifelse(.arm == 0, 0, t.on),
           cnt.stop =  ifelse(.arm == 0, t.off, time),
           exp.start = ifelse(.arm == 0, t.off, 0),
           exp.stop = ifelse(.arm == 0, time, t.on),
           Arm = ifelse(.arm == 0, "Randomized to Control", "Randomized to Experimental"))

  rc <- rbind(exp.df, mutate(exp.df, uid = uid+0.99)) %>%
    mutate(uid = uid / max(uid)) %>%
    ggplot(aes(x=uid)) +
    theme_bw() +
    geom_ribbon(aes(ymin = cnt.start, ymax = cnt.stop, fill = "Control")) +
    geom_ribbon(aes(ymin = exp.start, ymax = exp.stop, fill = "Experimental")) +
    scale_fill_manual(values = c("Black","Red"), name = "Treatment") +
    coord_flip() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ylab("Exposure Time prior to event or censoring") +
    facet_grid(~Arm)

  return(rc)
}

