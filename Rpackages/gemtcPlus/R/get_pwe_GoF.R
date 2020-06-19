#' Calculate the survivor function estimates for each study and arm. Calculate also the observed survival curves
#' from the binned KM data to compare observed and estimated survivor functions.
#'
#' @param fit       JAGS object with NMA fit.
#' @param data.arms Data frame with one line per study arm and columns study, treatment, studyn, treatmentn.
#' @param data.jg   List with input data set that was used in jags fit.
#' @param time      Vector of time-points at which S(t) functions are calculated.
#' @param bl.node   Charactor to identify the node in the jags model that identifies the baseline estimates (default is "mu").
#' @param contrast.node Charactor to identify the node in the jags model that identifies the baseline estimates (default is "d").
#'
#' @importFrom magrittr %>%
#' @importFrom grDevices dev.off pdf
#' @importFrom methods new
#' @importFrom stats dt integrate median quantile relevel stepfun
#' @return data.frame
#' @export
#'

get_pwe_GoF <- function(fit, 
                        data.arms,
                        data.jg,
                        time = 0:60,
                        bl.node = "mu",
                        contrast.node = "d"){
  cut.pts <- fit$model.pars$cut.pts
  seg <- get_segments(cut.pts)
  n_seg <- length(seg)

  studies <- unique(data.arms$study)[order(unique(data.arms$studyn))]

  n_std <- length(studies)

  ## get baseline and contrast MCMC iterations and dimension infos from jags fit/data
  mu <- fit$BUGSoutput$sims.list[[bl.node]][, , ]
  d <- fit$BUGSoutput$sims.list[[contrast.node]]
  n_a <- data.jg$Na
  t.mat <- data.jg$t


  ##  calculate absolute hazard rate estimates for each study and arm
  n_sims <- dim(d)[1]
  beta <- array(NA, dim = c(n_sims, n_std, max(n_a), n_seg))
  for (i in 1:n_std){
    for (j in 1:n_a[i]){
      for (k in 1:n_seg){
        beta[, i, j, k] <- mu[, i, k] + d[, t.mat[i, j], k] - d[, t.mat[i, 1], k]
      }
    }
  }

  ## calcuate summaries over MCMC iterations by study and arm for each segment, combine and identify trts in arms
  haz_m <- apply(exp(beta), c(2, 3, 4), median)
  haz_l <- apply(exp(beta), c(2, 3, 4), quantile, probs = 0.025, na.rm = TRUE)
  haz_u <- apply(exp(beta), c(2, 3, 4), quantile, probs = 0.975, na.rm = TRUE)

  haz_df <- data.frame(median = as.vector(haz_m),
                       lCrI = as.vector(haz_l),
                       uCrI = as.vector(haz_u),
                       segment = rep(seg, each = n_std * max(n_a)),
                       study = rep(rep(studies, max(n_a)), n_seg),
                       arm = rep(rep(1:max(n_a), each = n_std), n_seg),
                       stringsAsFactors = FALSE)

  haz_df$treatment <- NA
  for(i in 1:nrow(haz_df)){
    std_i <- haz_df$study[i]
    arm_i <- haz_df$arm[i]
    trt_i <- data.arms %>% dplyr::filter(study == std_i, arm == arm_i)
    if (nrow(trt_i) == 1){
      haz_df$treatment[i] <- as.character(trt_i$treatment)
    }
  }


  ## calculate survivor functions
  ## (o.k. to work with parameter summaries as par -> haz -> cumhaz -> S are monotonic transformations and summaries are quantiles
  ##  if interest is in mean instead of median, would need to calculate S(t) for every MCMC iteration and summarize only then)
  dat <- data.frame()
  for (i in 1:n_std){
    std_i <- studies[i]
    for (j in 1:n_a[i]){
      mhaz_ij <- haz_df %>% dplyr::filter(study == std_i, arm == j) %>% dplyr::select(segment, median, treatment)
      lhaz_ij <- haz_df %>% dplyr::filter(study == std_i, arm == j) %>% dplyr::select(segment, lCrI, treatment)
      uhaz_ij <- haz_df %>% dplyr::filter(study == std_i, arm == j) %>% dplyr::select(segment, uCrI, treatment)

      mS_ij <- pwe_S(time, cut.pts, mhaz_ij[[2]])
      lS_ij <- pwe_S(time, cut.pts, uhaz_ij[[2]])
      uS_ij <- pwe_S(time, cut.pts, lhaz_ij[[2]])

      dat_ij <- data.frame(time = time, S = mS_ij, lCrI = lS_ij, uCrI = uS_ij, study = std_i, arm = j, treatment = mhaz_ij[1, 3], stringsAsFactors = FALSE)
      dat <- rbind(dat, dat_ij)
    }
  }


  ## calculate observed KM curves
  dobs <- as.data.frame(data.jg[c("r", "n", "a", "s", "dt")])

  km1 <- dobs %>%
    dplyr::mutate(p_death = r / n, p_surv = 1 - p_death) %>%
    dplyr::group_by(s, a) %>%
    dplyr::mutate(S = cumprod(p_surv), time = cumsum(dt)) %>%
    dplyr::select(studyn = s, arm = a, S = S, time = time) %>%
    as.data.frame()

  km0 <- km1 %>%
    dplyr::group_by(studyn, arm) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(time = 0, S = 1) %>%
    dplyr::ungroup() %>%
    as.data.frame()

  km <- rbind(km0, km1) %>%
    dplyr::arrange(studyn, arm, time) %>%
    dplyr::mutate(study = NA, treatment = NA, lCrI = NA, uCrI = NA)

  #km$study <- NA
  #km$treatment <- NA
  for(i in 1:nrow(km)){
    std_i <- km$studyn[i]
    arm_i <- km$arm[i]
    trt_i <- data.arms %>% dplyr::filter(studyn == std_i, arm == arm_i)
    km$study[i] <- as.character(trt_i$study)
    km$treatment[i] <- as.character(trt_i$treatment)
  }


  ## combine with estimates
  km <- km[names(dat)]
  km$type <- "obs"

  dat$type = "nma"

  dall <- rbind(as.data.frame(dat), as.data.frame(km))

  return(dall)
}
