#' Calculate the study and arm level survivor functions estimates from a 2nd order fractional polynomial NMA. These estimates provide the basis for a goodness-of-fit graph when plotted along with the input data.
#'
#' @param fit       JAGS object with FP NMA fit.
#' @param time      Vector of time-points at which S(t) functions are calculated.
#' @param bl.node   Charactor to identify the node in the jags model that identifies the baseline estimates (default is "mu").
#' @param contrast.node Charactor to identify the node in the jags model that identifies the baseline estimates (default is "d").
#' 
#' @details         The FP parameters to calculate the log-hazard curves for each survivor function are combinations \code{bl.node + contrast.node}. Here, every study-specific baseline estimate is combined with the arm-specific contrast to obtain arm level NMA estimates.
#' @return          a \code{data.frame} containing survivor function for each treatment
#' @export
#'
#' @examples
#' 
get_fp_2o_GoF <- function(fit, time = 1:24, bl.node = "mu", contrast.node = "d"){
  
  ## check order of FP
  exponents <- fit$model.pars$exponents
  if (length(exponents) != 2){
    out <- NULL
    warning("Fractional polynomial must be of 2nd order")
  }
  
  
  `%>%` <- magrittr::`%>%`
  
  ## define the summary function used later on
  my_sum <- function(u){
    out <- c(S = median(u),
             lCrI = quantile(u, probs = 0.025, names = FALSE),
             uCrI = quantile(u, probs = 0.975, names = FALSE))
    return(out)
  }
  
  
  ## extract MCMC chains for baseline and contrast paramters for each study and arm to construct coefficients in FP
  mu <- fit$BUGSoutput$sims.list[[bl.node]]
  d  <- fit$BUGSoutput$sims.list[[contrast.node]]
  n_std <- fit$data.jg$Ns
  n_a <- fit$data.jg$Na
  t_mat <- fit$data.jg$t
  n_sims <- dim(mu)[1]
  n_par <- dim(mu)[3]
  d_arms <- attr(fit$data.jg, "d_arms")
  
  
  ## calculate NMA estimates for every study and arm (FP parameters = baseline + contrast)
  beta <- array(NA, dim = c(n_sims, n_std, max(n_a), n_par))
  for (i in 1:n_std){
    for (j in 1:n_a[i]){
      beta[, i, j, ] <- mu[, i, ] + d[, t_mat[i, j], ] - d[, t_mat[i, 1], ]
    }
  }
  
  dS <- data.frame()
  time0 <- c(0, time)
  dt <- diff(time0)
  for (i in 1:n_std){
    for(j in 1:n_a[i]){
      beta_ij <- beta[, i, j, ]   
      loghaz_ij <- get_fp_2o(x = time,
                             params = beta_ij,
                             exponents = exponents)
      
      haz_ij <- exp(loghaz_ij)
      
      dH_ij <- dt * haz_ij # approximate the cumulative hazard (for every MCMC iteration); first calculate the incerments over every interval, then sum up
      H_ij  <- apply(dH_ij, MAR = 2, cumsum)
      S_ij  <- exp(-H_ij)
      S_ijs <- t(apply(S_ij, MAR = 1, FUN = my_sum))
      S_ijd <- data.frame(time = time,
                          S_ijs,
                          studyn = i,
                          arm = j)
      dS <- rbind(dS, S_ijd)
    }
  }
  
  dnma <- dS %>%
    dplyr::left_join(d_arms, by = c("studyn", "arm")) %>%
    dplyr::select(-n_arms)
  
  
  ## calculate observed KM curves
  dobs <- as.data.frame(fit$data.jg[c("r", "n", "a", "s", "dt")])
  
  km1 <- dobs %>%
    dplyr::mutate(p_death = r / n, p_surv = 1 - p_death) %>%
    dplyr::group_by(s, a) %>%
    dplyr::mutate(S = cumprod(p_surv), time = cumsum(dt)) %>%
    dplyr::select(studyn = s, arm = a, S = S, time = time)
  
  km0 <- km1 %>%
    dplyr::group_by(studyn, arm) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(time = 0, S = 1)
  
  km <- rbind(km0, km1) %>%
    dplyr::arrange(studyn, arm, time) %>%
    dplyr::mutate(study = NA, treatment = NA, treatmentn = NA, lCrI = NA, uCrI = NA) %>%
    dplyr::group_by()
  
  #km$study <- NA
  #km$treatment <- NA
  for(i in 1:nrow(km)){
    std_i <- km$studyn[i]
    arm_i <- km$arm[i]
    trt_i <- d_arms %>% dplyr::filter(studyn == std_i, arm == arm_i)
    km$study[i] <- as.character(trt_i$study)
    km$treatment[i] <- as.character(trt_i$treatment)
    km$treatmentn[i] <- trt_i$treatmentn
  }
  
  
  ## combine with estimates
  km <- km[names(dnma)]
  km$type <- "obs"
  
  dnma$type = "nma"
  
  dall <- rbind(dnma, km)
  
  return(dall)
}
