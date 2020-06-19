#' Calculate the survivor functions estimated in piecewise-constant NMA model. The absolute S(t) estimates combining the estimated baseline survival
#' from a reference trial (in the NMA) with the piecewise-constant hazard ratio estimates to construct the S(t) functions for each treatment.
#'
#' @param fit       JAGS object with NMA fit.
#' @param ref.std   Numeric identifier of reference study to use for baseline survival estimate.
#' @param ref.arm   Numeric identifier of arm in reference study to use for baseline survival estimate.
#' @param treatments Vector with character strings to label the treatments. If NULL treatments extracted from fit
#' @param time      Vector of time-points at which S(t) functions are calculated.
#' @param bl.node   Charactor to identify the node in the jags model that identifies the baseline estimates (default is "mu").
#' @param contrast.node Charactor to identify the node in the jags model that identifies the baseline estimates (default is "d").
#'
#' @importFrom magrittr %>%
#' @return          a \code{data.frame} containing survivor function for each treatment
#' @export
#'

get_pwe_S <- function(fit, ref.std, ref.arm, treatments = NULL,
                      time = 0:24, bl.node = "mu", contrast.node = "d"){

  if(is.null(treatments)){
    treatments <- unique(attr(fit$data.jg, "d_arms")$treatment[order(attr(fit$data.jg, "d_arms")$treatmentn)]) 
  }
  cut.pts <- fit$model.pars$cut.pts
  n_trt <- length(treatments)
  seg <- get_segments(cut.pts)
  n_seg <- length(seg)

  ## get baseline and contrast MCMC iterations
  mu_ref <- fit$BUGSoutput$sims.list[[bl.node]][, ref.std, ]
  d <- fit$BUGSoutput$sims.list[[contrast.node]]

  ##  calculate absolute hazard rate estimates
  est_sims <- array(NA, dim = dim(d))
  for (i in 1:n_trt){
    est_sims[,i,] <- mu_ref + d[,i,]
  }

  ## calcuate summaries over MCMC iterations by trt
  n_sims <- dim(d)[1]
  est_df <- data.frame(loghaz = as.vector(est_sims),
                       segment = rep(seg, each = n_sims * n_trt),
                       treatment = rep(rep(treatments, each = n_sims), n_seg))

  est_sum <- est_df %>%
    dplyr::mutate(haz = exp(loghaz)) %>%
    dplyr::group_by(treatment, segment) %>%
    dplyr::summarize(median = median(haz), lCrI = quantile(haz, probs = 0.025), uCrI = quantile(haz, probs = 0.975))

  ## calculate survivor functions
  ## (o.k. to work with parameter summaries as par -> haz -> cumhaz -> S are monotonic transformations and summaries are quantiles
  ##  if interest is in mean instead of median, would need to calculate S(t) for every MCMC iteration and summarize only then)
  dat <- data.frame()
  for (i in 1:n_trt){
    trt_i <- treatments[i]
    mhaz_i <- est_sum %>% dplyr::filter(treatment == trt_i) %>% dplyr::select(treatment, median)
    lhaz_i <- est_sum %>% dplyr::filter(treatment == trt_i) %>% dplyr::select(treatment, lCrI)
    uhaz_i <- est_sum %>% dplyr::filter(treatment == trt_i) %>% dplyr::select(treatment, uCrI)

    mS_i <- pwe_S(time, cut.pts, mhaz_i[[2]])
    lS_i <- pwe_S(time, cut.pts, uhaz_i[[2]])
    uS_i <- pwe_S(time, cut.pts, lhaz_i[[2]])

    dat_i <- data.frame(time = time, S = mS_i, lCrI = lS_i, uCrI = uS_i, treatment = trt_i)
    dat <- rbind(dat, dat_i)
  }

  return(dat)
}
