#' Utility function to extract HR estimates from piece-wise exponential model fit in (format needed for ggplot)
#'
#' @param fit             \code{rjags} object which is the return of \code{R2jags::jags()}
#' @param treatments      Vector of treatment names (character string) ordered according to array indexes in jags fit. If NULL treatments extracted from fit
#' @param ref             Character string identifying reference treatment to use in calculating contrasts.
#' @param reverse         Logical, if TRUE (default) "ref vs others" is calculated (instead of "others vs ref").
#' @param exponentiate    Logical, if TRUE (default) contrast estimates are exponentiated in output.
#' @param xmax            Numeric
#' @param digits          Numeric
#' @param alpha           Numeric
#'
#' @importFrom magrittr %>%
#' @return                A \code{data.frame} contatining hazzard ratio estimates from an \code{rjags} object which is the return of \code{R2jags::jags()}
#' @export
#'

get_pwe_contrasts <- function(fit, treatments = NULL, ref,
                              reverse = FALSE, exponentiate = TRUE, xmax = 24, digits = 3,
                              alpha = 0.05){

  if(is.null(treatments)){
    treatments <- unique(attr(fit$data.jg, "d_arms")$treatment[order(attr(fit$data.jg, "d_arms")$treatmentn)]) 
  }
  cut.pts <- fit$model.pars$cut.pts
  segments <- get_segments(cut.pts)
  n_seg <- length(segments)
  n_trt <- length(treatments)

  d <- fit$BUGSoutput$sims.list[["d"]]
  n_sims <- dim(d)[1]

  d_sims <- array(NA, dim = c(n_sims, n_trt, n_trt, n_seg))

  rev.fact <- ifelse(reverse, -1, 1)

  for(s in 1:n_seg){
    for (i in 1:n_trt){
      for (j in 1:n_trt){
        # pairwise comp row-to-column
        d_sims[ , i, j, s] <- (d[, i, s] - d[, j, s]) * rev.fact
      }
    }
  }

  d_med  <- apply(d_sims, c(2,3,4), median)
  d_cilo <- apply(d_sims, c(2,3,4), quantile, probs = (alpha / 2))
  d_ciup <- apply(d_sims, c(2,3,4), quantile, probs = (1 - alpha / 2))

  ref_col <- which(treatments == ref)

  if(exponentiate){
    contrast_med  <- exp(d_med[-ref_col,  ref_col,])
    contrast_cilo <- exp(d_cilo[-ref_col, ref_col,])
    contrast_ciup <- exp(d_ciup[-ref_col, ref_col,])
  } else{
    contrast_med  <- d_med[-ref_col,  ref_col,]
    contrast_cilo <- d_cilo[-ref_col, ref_col,]
    contrast_ciup <- d_ciup[-ref_col, ref_col,]
  }

  if(!is.null(digits)){
    contrast_med <-  round(contrast_med, digits)
    contrast_cilo <- round(contrast_cilo, digits)
    contrast_ciup <- round(contrast_ciup, digits)
  }

  colnames(contrast_med) <- colnames(contrast_cilo) <- colnames(contrast_ciup) <- segments
 # dimnames(contrast_med) <- dimnames(contrast_cilo) <- dimnames(contrast_ciup) <- list(NULL, NULL, segments)
  rownames(contrast_med) <- rownames(contrast_cilo) <- rownames(contrast_ciup) <- treatments[-ref_col]

  contr_med  <- reshape::melt(contrast_med,  varnames = c("comparator", "Segment"))
  contr_cilo <- reshape::melt(contrast_cilo, varnames = c("comparator", "Segment"))
  contr_ciup <- reshape::melt(contrast_ciup, varnames = c("comparator", "Segment"))

  colnames(contr_med)[3]  <- "Median"
  colnames(contr_cilo)[3] <- "lCrI"
  colnames(contr_ciup)[3] <- "uCrI"

  out <- contr_med %>%
    dplyr::left_join(contr_cilo, by = c("comparator", "Segment")) %>%
    dplyr::left_join(contr_ciup, by = c("comparator", "Segment")) %>%
    dplyr::mutate(ref = ref)

  if (reverse){
    out <- out %>%
      dplyr::mutate(Comparison = paste(ref, "vs", comparator))
  } else{
    out <- out %>%
      dplyr::mutate(Comparison = paste(comparator, "vs", ref))
  }

  out <- out %>%
    dplyr::mutate(x = rep(c(0, cut.pts), each = n_trt - 1),
           xend = rep(c(cut.pts, xmax), each = n_trt - 1)) %>%
    dplyr::mutate(Segment = factor(Segment, ordered = TRUE, levels = segments)) %>%
    dplyr::arrange(Comparison, Segment) %>%
    dplyr::select(Comparison, Segment, Median, lCrI, uCrI, x, xend)


  return(out)
}



