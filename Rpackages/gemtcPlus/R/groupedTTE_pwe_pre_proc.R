#' Utility function for pre-processing: prepare jags input data for PWE model.
#'
#' @param dat A \code{data.frame} with columns study, treatment, t.start, t.end, n.event, n.risk.
#' @param ref.std A \code{character} of the reference study
#' @param nma.ref.trt A \code{character} of the reference treatment
#' @param model.pars \code{list}, containing exponents (numeric vector of exponents for FP model) and 
#'                    t.eval - one of "midpoint" (the default), "start", or "end", to identify the time-point within each interval used to calculate the hazard rate (over each interval, the hazard function is approximated with this value h(t.eval)); alternatively, a  \code{numeric} value in the interval [0,1] used to define t.eval (where for example "midpoint" corresponds to the value 0.5).
#' @param feprior_mean A \code{numeric} value representing feprior_mean (default 0)
#' @param feprior_prec A \code{numeric} value representing feprior_prec default 0.0001
#' @param bth.prior A \code{numeric} list containing between-trial heterogeneity priors
#'
#' @return A \code{list} with input data for jags fit of PWE model.
#' @import dplyr
#' @importFrom tidyr spread
#' @export
#'
#' @seealso \code{\link{nma_pre_proc}}, \code{\link{groupedTTE_fp_pre_proc}}
#'

groupedTTE_pwe_pre_proc <- function(dat,
                                    ref.std = NULL,
                                    nma.ref.trt = NULL,
                                    model.pars = NULL,
                                    feprior_mean = NULL,
                                    feprior_prec = NULL,
                                    bth.prior = NULL) {
  
  `%>%` <- magrittr::`%>%`

  dat <- dat %>%
    dplyr::mutate(studyf = relevel(study, ref = ref.std),
                  treatmentf = relevel(treatment, ref = nma.ref.trt),
                  studyn = as.numeric(studyf),
                  treatmentn = as.numeric(treatmentf),
                  dt = t.end - t.start)
  
  cuts <- c(0, model.pars$cut.pts, Inf)
  segment <- cut(dat$t.start,
                 breaks = cuts,
                 include.lowest = TRUE,
                 labels = 1:(length(cuts)-1),
                 right = FALSE,
                 ordered_result = TRUE
  )
  dat$segment <- segment
  
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
    segment = dat$segment,
    Ncuts = length(model.pars$cut.pts),
    r = dat$n.event,
    n = dat$n.risk,
    dt = dat$dt,
    s = dat$studyn,
    a = dat$arm,
    t = as.matrix(select(ungroup(d_trts), -studyn)),
    Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
  )



  ## add priors
  dat_jg$feprior_mean <- feprior_mean           
  dat_jg$feprior_prec <- feprior_prec

  # Add random effect priors
  for(i in seq_along(bth.prior$args)){
    name <- paste("reprior", bth.prior$type, bth.prior$distr, names(bth.prior$args[i]), sep = "_")
    dat_jg[[name]] <- bth.prior$args[[i]]
  }
  
  attr(dat_jg, "d_arms") <- d_arms
  
  return(dat_jg)
}  

