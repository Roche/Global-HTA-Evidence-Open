

#' Fits a cox model to an rpsftm object
#'
#' This function fits a cox model to an rpsftm object. It estimates a hazard ratio for the comparison of experimental observed time compared to control latent time. In addition the variance of the treatment coefficient is inflated based on the test used in g-estimation. Any strata included in rpsftm call are used in the cox model.
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
#' rpsftm.coxph(imm.fit)
#' @import survival
#' @import rpsftm
#' @import dplyr

rpsftm.coxph <- function(x){
  if (!("rpsftm" %in% class(x))){
    stop("Please provide an rpsftm object.")
  }

  # get the data
  # replace the control with latent values
  cfact.df <- rpsftm.data(x) %>%
    mutate(o.time = time,
           o.cens = cens,
           l.time = x$Sstar[,1],
           l.cens = x$Sstar[,2],
           time = ifelse(.arm == 0, l.time, o.time),
           cens = ifelse(.arm == 0, l.cens, o.cens)
    )

  # fit a cox model

  cfact.cox <-coxph(rpsftm.formula(x), data = cfact.df)

  # correct the variance
  cfact.cox$var.orig <- cfact.cox$var

  # find what the estimating function was
  if ("survdiff" %in% class(x)){

    sd.itt  <- survdiff(rpsftm.formula(x), data = rpsftm.data(x))
    z0 <- sd.itt$chisq ^ 0.5
    cfact.cox$var <- matrix((abs(coef(cfact.cox)) / z0)^2)

  } else {
    stop("Sorry so far only survdiff as test function supported.")
  }

  return(cfact.cox)

}

