# Functions for the estimation of propensity weights


# Internal functions - Not exported ---------------------------------------
# Objective function
objfn <- function(a1, X){
  sum(exp(X %*% a1))
}

# Gradient function
gradfn <- function(a1, X){
  colSums(sweep(X, 1, exp(X %*% a1), "*"))
}

# External functions ------------------------------------------------------

#' Estimate MAIC propensity weights
#'
#' Estimate propensity weights for matching-adjusted indirect comparison (MAIC).
#'
#' @param intervention_data A data frame containing individual patient data from
#'   the intervention study.
#' @param matching_vars A character vector giving the names of the covariates to
#'   use in matching. These names must match the column names in
#'   intervention_data.
#' @param method The method used for optimisation - The default is method =
#'   "BFGS". Refer to \code{\link[stats]{optim}} for options.
#' @param ... Additional arguments to be passed to optimisation functions such
#'   as the method for maximum likelihood optimisation. Refer to \code{\link[stats]{optim}} 
#'   for options.
#'
#' @details The premise of MAIC methods is to adjust for between-trial
#'   differences in patient demographic or disease characteristics at baseline.
#'   When a common treatment comparator or ‘linked network’ are unavailable, a
#'   MAIC assumes that differences between absolute outcomes that would be
#'   observed in each trial are entirely explained by imbalances in prognostic
#'   variables and treatment effect modifiers.
#'
#'   The aim of the MAIC method is to estimate a set of propensity weights based
#'   on prognostic variables and treatment effect modifiers. These weights can
#'   be used in subsequent statistical analysis to adjust for differences in
#'   patient characteristics between the population in the intervention trial
#'   and the population in a comparator study. For additional details on the
#'   statistical methods, refer to the package vignette.
#'
#'   The data required for an unanchored MAIC are:
#'
#'   \itemize{
#'     \item Individual patient data from a single arm study of 'intervention'
#'     \item Aggregate summary data for 'comparator'. This could be from a
#'     single arm study of the comparator or from one arm of a randomized
#'     controlled trial.
#'     \item Psuedo patient data from the comparator study. This is not required
#'     for the matching process but is needed to derive the relative treatment
#'     effects between the intervention and comparator.
#'     }
#'
#'     For the matching process:
#'
#'     \enumerate{
#'       \item All binary variables to be used in the matching should be coded 1
#'       and 0
#'       \item The variable names need to be listed in a character vector called
#'       match_cov
#'       \item Aggregate baseline characteristics (number of patients, mean and
#'       SD for continuous variables and proportion for binary variables) from
#'       the comparator trial are needed as a data frame. Naming of the
#'       covariates in this data frame should be consistent with variable names
#'       in the intervention data.
#'       \item Patient baseline characteristics in the intervention study are
#'       centered on the value of the aggregate data from the comparator study
#'       \item The estimate_weights function can then be used to estimate
#'       propensity weights for each patient in the intervention study
#'     }
#'
#'     For full details refer to the example below and the package vignette
#'
#' @return A list containing 2 objects. First, a data frame named analysis_data
#'   containing intervention_data with additional columns named wt (weights) and
#'   wt_rs (rescaled weights). Second, a vector called matching_vars of the
#'   names of the centered matching variables used.
#' @references NICE DSU TECHNICAL SUPPORT DOCUMENT 18: METHODS FOR
#'   POPULATION-ADJUSTED INDIRECT COMPARISONS IN SUBMSISSIONS TO NICE, REPORT BY
#'   THE DECISION SUPPORT UNIT, December 2016
#' @seealso \code{\link{optim}}
#'
#' @example inst/examples/MAIC_example_weights.R
#'
#' @export
estimate_weights <- function(intervention_data,  matching_vars, method = "BFGS", ...){

  #Basic checks of inputs before proceeding
  #Check intervention data is a data frame
  assertthat::assert_that(
    is.data.frame(intervention_data),
    msg = "intervention_data is expected to be a data frame"
  )

  #Check that matching_vars is a character vector
  assertthat::assert_that(
    is.character(matching_vars),
    msg = "matching_vars is expected to be a character vector"
  )
  #Check that all named matching variables are in the intervention dataset
  assertthat::assert_that(
    all(matching_vars %in% colnames(intervention_data)),
    msg = "matching_vars contains variable names that are not in the intervention dataset"
  )



  # Optimise Q(b) using Newton-Raphson techniques
  opt1 <- optim(par = rep(0,dim(as.data.frame(intervention_data[,matching_vars]))[2]),
                fn = objfn,
                gr = gradfn,
                X = as.matrix(intervention_data[,matching_vars]),
                method = method,
                ...)

  a1 <- opt1$par

  # Calculate weights for intervention data and combine with dataset
  data_with_wts <- dplyr::mutate(intervention_data,
                                 wt = as.vector(exp(as.matrix(intervention_data[,matching_vars]) %*% a1)), # weights
                                 wt_rs = (wt / sum(wt)) * nrow(intervention_data), # rescaled weights
                                 ARM = "Intervention"
  )


  # Outputs are:
  #       - the analysis data (intervention PLD with weights )
  #       - A character  vector with the name of the matching variables
  output <- list(
    matching_vars = matching_vars,
    analysis_data = data_with_wts
  )

  return(output)

}

# Functions for summarizing the weights ----------------------------------------

#' Estimate effective sample size
#'
#' Estimate the effective sample size (ESS).
#'
#' @param data A data frame containing individual patient data from
#'   the intervention study, including a column containing the weights (derived
#'   using \code{\link{estimate_weights}}).
#' @param wt_col The name of the weights column in the data frame containing the
#'   intervention individual patient data and the MAIC propensity weights. The
#'   default is wt.
#'
#' @details For a weighted estimate, the effective sample size (ESS) is the
#'   number of independent non-weighted individuals that would be required to
#'   give an estimate with the same precision as the weighted sample estimate. A
#'   small ESS, relative to the original sample size, is an indication that the
#'   weights are highly variable and that the estimate may be unstable. This
#'   often occurs if there is very limited overlap in the distribution of the
#'   matching variables between the populations being compared. If there is
#'   insufficient overlap between populations it may not be possible to obtain
#'   reliable estimates of the weights
#'
#' @return The effective sample size (ESS) as a numeric value.
#'
#' @references NICE DSU TECHNICAL SUPPORT DOCUMENT 18: METHODS FOR
#'   POPULATION-ADJUSTED INDIRECT COMPARISONS IN SUBMSISSIONS TO NICE, REPORT BY
#'   THE DECISION SUPPORT UNIT, December 2016
#'
#' @seealso \code{\link{estimate_weights}}
#'
#' @example inst/examples/MAIC_example_weight_diagnostics.R
#'
#' @export
estimate_ess <- function(data, wt_col="wt"){
  ess <- sum(data[,wt_col])^2/sum(data[,wt_col]^2)
  return(ess)
}

#' Summarize the weight values
#'
#' Produce a summary of the weights (minimum, maximum, median, mean, standard
#' deviation). Mean and standard deviation are provided for completeness.
#' In practice the distribution of weights may be skewed in which case mean and
#' SD should be interpreted with caution.
#'
#' @param data A data frame containing individual patient data from
#'   the intervention study, including a column containing the weights (derived
#'   using \code{\link{estimate_weights}}).
#' @param wt_col The name of the weights column in the data frame containing the
#'   intervention individual patient data and the MAIC propensity weights. The
#'   default is wt.
#' @param rs_wt_col The name of the rescaled weights column in the data frame
#'   containing the intervention individual patient data and the MAIC propensity
#'   weights. The default is wt_rs.
#'
#' @return A data frame that includes a summary (minimum, maximum, median, mean,
#'   standard deviation) of the weights and rescaled weights.
#'
#' @seealso \code{\link{estimate_weights}}
#'
#' @example inst/examples/MAIC_example_weight_diagnostics.R
#'
#' @export
summarize_wts <- function(data, wt_col="wt", rs_wt_col="wt_rs"){
  summary <- data.frame(
    type = c("Weights", "Rescaled weights"),
    mean = c(mean(data[,wt_col]), mean(data[,rs_wt_col])),
    sd = c(sd(data[,wt_col]), sd(data[,rs_wt_col])),
    median = c(median(data[,wt_col]), median(data[,rs_wt_col])),
    min = c(min(data[,wt_col]), min(data[,rs_wt_col])),
    max = c(max(data[,wt_col]), max(data[,rs_wt_col]))
  )
  return(summary)
}


#' Produce histograms of weights and rescaled weights
#'
#' Produce a plot containing two histograms (one of the weights and one of the
#' rescaled weights).
#'
#' @param data A data frame containing individual patient data from
#'   the intervention study, including a column containing the weights (derived
#'   using \code{\link{estimate_weights}}).
#' @param wt_col The name of the weights column in the data frame containing the
#'   intervention individual patient data and the MAIC propensity weights. The
#'   default is wt.
#' @param rs_wt_col The name of the rescaled weights column in the data frame
#'   containing the intervention individual patient data and the MAIC propensity
#'   weights. The default is wt_rs.
#' @param bin Number of bins to plot histogram. The default is 30.
#'
#' @return A histogram plot of the weights and rescaled weights.
#'
#' @seealso \code{\link{estimate_weights}}
#'
#' @example inst/examples/MAIC_example_weight_diagnostics.R
#'
#' @export
hist_wts <- function(data, wt_col="wt", rs_wt_col="wt_rs", bin = 30) {

  wt_data <- data %>%
    dplyr::select(c(wt_col, rs_wt_col)) %>% # select only the weights and rescaled weights
    dplyr::rename("Weights" = wt_col, "Rescaled weights" = rs_wt_col) %>% # rename so for plots
    tidyr::gather() # weights and rescaled weights in one column for plotting


  hist_plot <- ggplot2::ggplot(wt_data) + ggplot2::geom_histogram(ggplot2::aes(value), bins = bin) +
    ggplot2::facet_wrap(~key,  ncol=1) + # gives the two plots (one on top of the other)
    ggplot2::theme_bw()+
    ggplot2::theme(axis.title = ggplot2::element_text(size = 16),
                   axis.text = ggplot2::element_text(size = 16)) +
    ggplot2::ylab("Frequency") +
    ggplot2::xlab("Weight")

  return(hist_plot)
}


#' Produce a data frame of the weights assigned to patient profiles
#'
#' Select the patient characteristics used in the matching and the MAIC weights
#' and output a data frame of unique propensity weight values with the
#' associated summary baseline characteristics. This data frame helps to
#' understand how different patient profiles are contributing to the analyses by
#' illustrating the patient characteristics associated with different weight
#' values. For example, min, max and median weights. This function is most
#' useful when only matching on binary variables as there are fewer unique
#' values.
#'
#' @param data A data frame containing individual patient data from
#'   the intervention study, including a column containing the weights (derived
#'   using \code{\link{estimate_weights}}).
#' @param wt_col The name of the weights column in the data frame containing the
#'   intervention individual patient data and the MAIC propensity weights. The
#'   default is wt.
#' @param rs_wt_col The name of the rescaled weights column in the data frame
#'   containing the intervention individual patient data and the MAIC propensity
#'   weights. The default is wt_rs.
#' @param vars A character vector giving the variable names of the baseline
#'   characteristics (not centered). These names must match the column names in
#'   the data.
#'
#' @return A data frame that includes a summary of patient characteristics
#'   associated with each weight value.
#'
#' @seealso \code{\link{estimate_weights}}
#'
#' @example inst/examples/MAIC_example_weight_diagnostics.R
#'
#' @export
profile_wts <- function(data, wt_col="wt", wt_rs="wt_rs", vars){
  profile_data <-  data %>%
    dplyr::select(vars, wt_col, wt_rs)

  profile_wts <- profile_data %>%
    dplyr::distinct()

  return(profile_wts)
}

#' Weight diagnostics
#'
#' Produce a set of useful diagnostic metrics to summarize propensity weights
#' \itemize{
#'   \item ESS (\code{\link{estimate_ess}})
#'   \item Summary statistics of the weights: minimum, maximum, median, mean, SD (\code{\link{summarize_wts}})
#'   \item Patient profile associated with weight values (\code{\link{profile_wts}})
#' }
#'
#' @param data A data frame containing individual patient data from
#'   the intervention study, including a column containing the weights (derived
#'   using estimate_weights).
#' @param wt_col The name of the weights column in the data frame containing the
#'   intervention individual patient data and the MAIC propensity weights. The
#'   default is wt.
#' @param rs_wt_col The name of the rescaled weights column in the data frame
#'   containing the intervention individual patient data and the MAIC propensity
#'   weights. The default is wt_rs.
#' @param vars A character vector giving the variable names of the baseline
#'   characteristics (not centered). These names must match the column names in
#'   the data.
#'
#' @return List of the following:
#' \itemize{
#'   \item The effective sample size (ESS) as a numeric value.
#'   \item A data frame that includes a summary (minimum, maximum, median, mean,
#'   standard deviation) of the weights and rescaled weights.
#'   \item A data frame that includes a summary of patient characteristics
#'   associated with each weight value.
#' }
#'
#' @seealso \code{\link{estimate_weights}}, \code{\link{estimate_ess}}, \code{\link{summarize_wts}}, \code{\link{profile_wts}}
#'
#' @example inst/examples/MAIC_example_weight_diagnostics.R
#'
#' @export
wt_diagnostics <- function(data, wt_col="wt", wt_rs="wt_rs", vars){

  # ESS
  ESS <- estimate_ess(data, wt_col)

  # Summary
  summ_wts <- summarize_wts(data, wt_col)

  # Weight profiles
  profile <- profile_wts(data, wt_col, wt_rs, vars)

  output <- list("ESS" = ESS,
                 "Summary_of_weights" = summ_wts,
                 "Weight_profiles" = profile
  )
  return(output)
}






