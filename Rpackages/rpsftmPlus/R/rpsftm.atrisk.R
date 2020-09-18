
#' Evaluate at risk status from an rpsftm object
#'
#' This function returns a data frame containing following variables derived from an rpsftm object.
#' It assumes that only a single switch in treatment occurs i.e. a patient who is randomized to control but has 25% exposure to experimental is assumed to have switched at 0.75*event time.
#' \itemize{
#'    \item .arm - 0 or 1 - randomized arm
#'    \item Arm - "Randomized to Control" or "Randomized to Experimental"
#'    \item Time - time that status applies for
#'    \item Status - one of "Event", "Censored", "At risk on Control", "At risk on Experimental"
#'    \item Count - count of patients in this status at this time.
#'    \item Percent - based on total number of patients in arm.
#' }
#'
#' @param x rpsftm object (rpsftm)
#' @param eval.times - vector of times to evaluate at risk status for. Default is all event and censoring times.
#' @keywords survival
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
#' rpsftm.atrisk(imm.fit, eval.times = c(0, 1, 2, 3))
#' @import survival
#' @import rpsftm
#' @import dplyr

rpsftm.atrisk <- function(x, eval.times = NULL){

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
           exp.stop = ifelse(.arm == 0, time, t.on)
    )

  # get unique event & censoring times if not specified

  if (is.null(eval.times)){
    eval.times <- c(0, exp.df$time, exp.df$exp.start, exp.df$exp.stop, exp.df$cnt.start, exp.df$cnt.stop) %>%
      unique()
  }

  # get status at each timepoint

  stat.all <- expand.grid(.arm = c(0,1), eval.time = eval.times, uid = unique(exp.df$uid))

  stat.df <- right_join(exp.df, stat.all, by = c(".arm", "uid")) %>%
    mutate(final.event = ifelse(cens==1, "Event", "Censored"),
           status = ifelse(eval.time > time, final.event,
                           ifelse(eval.time > cnt.start & eval.time <= cnt.stop, "At risk on Control",
                                  ifelse(eval.time > exp.start & eval.time <= exp.stop, "At risk on Experimental", 
                                        ifelse(.arm == 0,  "At risk on Control",  "At risk on Experimental")
                                        )
                           )
           )
    ) %>%
    filter(!is.na(status))

  bign.df <- group_by(exp.df, .arm) %>%
    summarise(bign = n()) %>%
    ungroup()

  count.df <- group_by(stat.df, .arm, eval.time, status) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    left_join(bign.df, by = c(".arm")) %>%
    mutate(Percent = Count/bign) %>%
    transmute(.arm, Time = eval.time, Status = status, Count, Percent)

  # ensure all types are included at each timepoint even if 0

  full.df <- expand.grid(Time = unique(count.df$Time),
                         Status = c("Event", "Censored", "At risk on Control", "At risk on Experimental"),
                         .arm = unique(count.df$.arm)) %>%
    mutate(Status = as.character(Status))

  rc <-  left_join(full.df, count.df, by = c("Time", ".arm", "Status")) %>%
    mutate(Arm = ifelse(.arm == 0, "Randomized to Control", "Randomized to Experimental"),
           Count = ifelse(is.na(Count), 0, Count),
           Percent = ifelse(is.na(Percent), 0, Percent)
    ) %>%
    transmute(.arm, Arm, Time, Status, Count, Percent)

  return(rc)

}


