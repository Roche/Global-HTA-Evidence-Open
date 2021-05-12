#' Run a complete parametric survival analysis 
#'
#' This function is a wrapper for the \code{\link{runPSM}} function intended to be run with the \code{\link{boot}} package. 
#' It enables running a complete parametric survival analysis for use when performing bootstrapping to explore uncertainty.
#' By re-using random seeds for each bootstrap sample it is possible to maintain correlations across multiple endpoints.
#'
#' For more details and examples see the package vignettes:
#' \itemize{
#'   \item \code{vignette("Fitting_models_in_R", package = "flexsurvPlus")}
#'   \item \code{vignette("Fitting_models_in_R_bootstrap", package = "flexsurvPlus")}
#'   \item \code{vignette("Survival_analysis_theory", package = "flexsurvPlus")}
#'   }
#'
#' @param data A data frame containing individual patient data for the relevant
#'   time to event outcomes. This is passed to the \code{data} argument of the
#'   \code{\link{runPSM}} function
#' @param i Index used to select a sample within \code{\link{boot}}.
#' @param ... Additional parameters as used by \code{\link{runPSM}}
#' @details  This function is intended to be used in conjunction with the
#'   \code{\link{boot}} function to return the statistic to be
#'   bootstrapped. In this case by performing parametric survival modelling using
#'   {\link{flexsurv}} and returning the parameters of the survival distributions.
#'    This is used as the 'statistic' argument in the boot function.

#' @return The 'parameters_vector' object from the \code{\link{runPSM}} function.
#'
#'    'parameters_vector' is a vector which contains the coefficients for all of the flexsurv models specified.
#'    The column names are in the format 'modeltype.distribution.parameter.TreatmentName', for example, comshp.weibull.shape.Int refers to the shape parameter
#'     of the common shape weibull distribution for the intervention treatment and 'indshp.gengamma.mu.ref' refers to the mu parameter
#'     of the independent shape generalised gamma distribution for the reference treatment. Columns with 'TE' at the end are the treatment effect coefficients
#'      (applicable to the scale and shape parameters for independent shape models, applicable to the scale parameter only for the common shape
#'      model and not applicable for the separate model).
#'
#' @export
bootPSM <- function(data,
                    i,
                   ...){
  data_boot <- data[i,]
  # call run_PSM (this does validity checks on inputs so no need to duplicate checks here)
  tryCatch({
    output <- runPSM(data = data_boot, ...)
    return(output$parameters_vector)
  })
}

