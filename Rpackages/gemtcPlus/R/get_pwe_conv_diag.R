#' Utility function: convergence diagnostics for piece-wise constant models
#'
#' @param fit      \code{rjags} object which is the return of \code{R2jags::jags()}
#' @param file     Optional output filename (a pdf with this name will be created if provided).
#' @param patterns Vector of character strings used to identify the patterns selected for conv assessment.
#' @param re.pattern Character
#' @importFrom magrittr %>%
#' @return ggplot object
#' @export

get_pwe_conv_diag <- function(fit,
                              file = NULL,
                              patterns = c("d", "mu"),
                              re.pattern = "sd"){
 # `%>%` <- magrittr::`%>%`
  plots <- list()
  fit_mc <- coda::as.mcmc(fit)
  rnames <- rownames(fit$BUGSoutput$summary)

  ## get dimensions and prepare for outputs
  n_pat <- length(patterns)
  ns <- vector()

  for (i in 1:n_pat){
    pat_i <- patterns[i]
    ns[i] <- dim(fit$BUGSoutput$sims.list[[pat_i]])[2]
  }


  ## open device
  if (!is.null(file)) { pdf(file) }

  ## overall summary tables
  for (i in 1:n_pat){
    pat_i <- patterns[i]
    sel_i <- grep(paste(pat_i, "[", sep = ""), rnames, fixed = TRUE)
    plots <- plots %>%
             append(list(gplots::textplot(round(fit$BUGSoutput$summary[sel_i,
                                                                  c("mean",
                                                                    "50%",
                                                                    "2.5%",
                                                                    "97.5%",
                                                                    "Rhat",
                                                                    "n.eff")], 3))))
  }
  if (re.pattern %in% rnames){
    sum_re <- data.frame(t(round(fit$BUGSoutput$summary[re.pattern, c("mean",
                                                                      "50%",
                                                                      "2.5%",
                                                                      "97.5%",
                                                                      "Rhat",
                                                                      "n.eff")], 3)))
    rownames(sum_re) <- re.pattern # rownames lost if only vector extracted from BUGSoutput$summary
    plots <- plots %>%
             append(list(gplots::textplot(sum_re)))
  }
  sum_de <- data.frame(t(round(fit$BUGSoutput$summary["deviance", c("mean",
                                                                    "50%",
                                                                    "2.5%",
                                                                    "97.5%",
                                                                    "Rhat",
                                                                    "n.eff")], 3)))
  rownames(sum_de) <- "deviance" # rownames lost if only vector extracted from BUGSoutput$summary
  plots <- plots %>%
           append(list(gplots::textplot(sum_de)))


  ## trace- and density-plots
  for (i in 1:n_pat){
    pat_i <- patterns[i]
    for (j in 1:ns[i]){
      lab_ij <- paste(pat_i, "[", j, ",", sep = "")
      sel_ij <- grep(lab_ij, colnames(fit_mc[[1]]), fixed = TRUE)
      plots <- plots %>%
        append(list(ggmcmc::ggs_traceplot(ggmcmc::ggs(fit_mc[, sel_ij]))))
      plots <- plots %>%
        append(list(ggmcmc::ggs_density(ggmcmc::ggs(fit_mc[, sel_ij]))))
    }
  }
  if (re.pattern %in% rnames){
    plots <- plots %>%
      append(list(ggmcmc::ggs_traceplot(ggmcmc::ggs(fit_mc), family = re.pattern)))
    plots <- plots %>%
      append(list(ggmcmc::ggs_density(ggmcmc::ggs(fit_mc), family = re.pattern)))
  }
  plots <- plots %>%
    append(list(ggmcmc::ggs_traceplot(ggmcmc::ggs(fit_mc), family = "deviance")))
  plots <- plots %>%
    append(list(ggmcmc::ggs_density(ggmcmc::ggs(fit_mc), family = "deviance")))

  ## close device
  if (!is.null(file)) { dev.off() }
  plots
}
