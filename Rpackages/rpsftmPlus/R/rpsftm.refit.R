
#' Refits an rpsftm model for common treatment effect sensitivity analysis
#'
#' This function refits an rpsftm object with a specific weight applied to control arm efficacy. It is a short cut to recalling rpsftm with same parameters and varying the treat_modifier parameter.
#'
#' See also documentation for rpsftm call treat_modifier= value.
#'
#' @param x rpsftm object (rpsftm)
#' @param k weight to be applied in the control arm for treatment effect. Single number.
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
#' rpsftm.refit(imm.fit, k = 0.7)
#' rpsftm.refit(imm.fit, k = 1.3)
#' @import survival
#' @import rpsftm
#' @import dplyr

rpsftm.refit <- function(x, k){

  if (!("rpsftm" %in% class(x))){
    stop("Please provide an rpsftm object.")
  }
  # add weights to control arm
  rnd <- as.data.frame(x$rand)

  df <- fget(as.character(x$call$data)) %>%
    mutate(.arm  = rnd$arm, .rx = rnd$rx) %>%
    mutate(wt = ifelse(.arm == 1, 1, k))

  frm <- as.formula(x$call$formula)

  psi.tried <- x$eval_z$psi
  
  recens<-NULL
  #If the original fit was with recensoring then recensor here too:
  if (is.null(x$call$censor_time)==FALSE) {recens=df[, as.character(x$call$censor_time)]}
  
  rc <- rpsftm(frm, 
               data = df, 
               treat_modifier = wt, 
               low_psi = min(psi.tried), 
               hi_psi = max(psi.tried), 
               n_eval_z = length(psi.tried), 
               censor_time=recens)


  rc$call <- x$call

  return(rc)
}
