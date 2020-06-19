#' Utility function for pre-processing: prepare jags input data for FP model.
#'
#' @param dat A \code{data.frame} with columns study, treatment, t.start, t.end, n.event, n.risk.
#' @param ref.std A \code{character} of the reference study
#' @param nma.ref.trt A \code{character} of the reference treatment
#' @param model.pars \code{list}, containing exponents (numeric vector of exponents for FP model) and 
#'                    t.eval - one of "midpoint" (the default), "start", or "end", to identify the time-point within each interval used to calculate the hazard rate (over each interval, the hazard function is approximated with this value h(t.eval)); alternatively, a  \code{numeric} value in the interval [0,1] used to define t.eval (where for example "midpoint" corresponds to the value 0.5).
#' @param feprior_mean A \code{numeric} value representing feprior_mean (default 0)
#' @param feprior_prec A \code{numeric} value representing feprior_prec default 0.0001
#' 
#' @return A \code{list} with input data for jags fit of PWE model.
#' @import dplyr
#' @importFrom tidyr spread
#' @export
#'
#' @seealso \code{\link{nma_pre_proc}}, \code{\link{groupedTTE_pwe_pre_proc}}

groupedTTE_fp_pre_proc <- function(dat,
                                   ref.std = NULL,
                                   nma.ref.trt = NULL,
                                   model.pars = NULL,
                                   feprior_mean = NULL,
                                   feprior_prec = NULL,
                                   bth.prior = NULL) {
  `%>%` <- magrittr::`%>%`

  # identify t.eval
  if (is.character(model.pars$t.eval)){
    if (model.pars$t.eval == "midpoint"){ t.eval <- 0.5 }
    if (model.pars$t.eval == "start"){    t.eval <- 0   }
    if (model.pars$t.eval == "end"){      t.eval <- 1   }
  }
  if (!(is.numeric(t.eval) & t.eval >= 0 & t.eval <=1)){
    stop("t.eval must be a number in [0,1] or one of 'midpoint', 'start', 'end'.")
  }
  
  dat <- dat %>%
    dplyr::mutate(studyf = relevel(study, ref = ref.std),
                  treatmentf = relevel(treatment, ref = nma.ref.trt),
                  studyn = as.numeric(studyf),
                  treatmentn = as.numeric(treatmentf),
                  dt = t.end - t.start,
                  time = t.start + t.eval * (t.end - t.start)
                  )
  
  d_arms <- dat %>%
    # dplyr::mutate(study = as.character(study),
    #               treatment = as.character(treatment)) %>%
    dplyr::group_by(study, treatment) %>%
    dplyr::slice(1) %>%
    dplyr::group_by(study) %>%
    dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
    dplyr::select(study, treatment, studyn, treatmentn, arm, n_arms)
  
  dat <- dat %>%
    dplyr::left_join(d_arms,
                     by = c("study", "treatment", "studyn", "treatmentn"))
  
  d_std <- d_arms %>%
    dplyr::group_by(studyn) %>%
    dplyr::select(studyn, n_arms) %>%
    dplyr::slice(1)
  
  d_trts <- dat %>%
    dplyr::mutate(studyn.arm = interaction(studyn, arm)) %>%
    dplyr::filter(!duplicated(studyn.arm)) %>%
    dplyr::select(studyn, arm, treatmentn) %>%
    dplyr::arrange(studyn, arm) %>%
    tidyr::spread(key = arm, treatmentn, drop = FALSE) # identify trt in each am for each study


  # collect input list for jags fit
  dat_jg <- list(
    Nobs = nrow(dat),
    Ns = nrow(d_std),
    Na = d_std$n_arms,
    r = dat$n.event,
    n = dat$n.risk,
    time = dat$time, # this should be a user option (i.e. select t.start, t.end, or midpoint)
    dt = dat$dt,
    s = dat$studyn,
    a = dat$arm,
    t = as.matrix(select(ungroup(d_trts), -studyn)),
    Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
  )
  

  # exponents in FP model (BUGS) code are called P1, P2 etc (must be given as data to jags fit)
  el <- as.list(model.pars$exponents)
  names(el) <- paste("P", 1:length(model.pars$exponents), sep = "")

  dat_jg <- c(dat_jg,
              el)
  
  ## Calculate and add priors
  dat_jg$feprior_mean <- rep(feprior_mean, length(model.pars$exponents) + 1)            
  dat_jg$feprior_prec <- diag(feprior_prec, length(model.pars$exponents) + 1)
  
  # Add random effect priors
  for(i in seq_along(bth.prior$args)){
    name <- paste("reprior", bth.prior$type, bth.prior$distr, names(bth.prior$args[i]), sep = "_")
    dat_jg[[name]] <- bth.prior$args[[i]]
  }
  
  # arm information needed later for proper labelling in post-processing
  attr(dat_jg, "d_arms") <- d_arms
  
  return(dat_jg)
}  
