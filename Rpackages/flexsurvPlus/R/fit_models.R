
#' Fit flexsurv models
#'
#' Fits single or multiple \code{\link{flexsurv}} models using \code{\link{flexsurvreg}}.
#'
#' @param model.formula A survival model formula in the general form shown
#'   below. Note that variable names must match the corresponding columns in the
#'   data. This is passed to the \code{formula} argument of the
#'   \code{\link{fit_models}} function
#'  \itemize{
#'   \item Surv(Time, Event==1) ~ ARM is a model with a single covariate for the
#'   effect of treatment
#'   \item Surv(Time, Event==1) ~ 1 is a model with no covariates typically
#'   fitted to data from a single treatment group
#'  }
#' @param data A data frame containing individual patient data for the relevant
#'   time to event outcomes. This is passed to the \code{data} argument of the
#'   \code{\link{fit_models}} function
#' @param dist A vector string of distributions, see dist argument in
#'   \code{\link{flexsurvreg}}. This is passed to the \code{distr} argument of
#'   the \code{\link{fit_models}} function
#' @details Possible distributions include:
#' \itemize{
#'   \item Exponential ('exp')
#'   \item Weibull ('weibull')
#'   \item Gompertz ('gompertz')
#'   \item Log-normal ('lnorm')
#'   \item Log-logistic ('llogis')
#'   \item Generalized gamma ('gengamma')
#'   \item Gamma ('gamma')
#'   \item Generalised F ('genf')
#'   }
#' For more details and examples see the package vignettes:
#' \itemize{
#'   \item \code{vignette("Fitting_models_in_R", package = "flexsurvPlus")}
#'   \item \code{vignette("Fitting_models_in_R_bootstrap", package = "flexsurvPlus")}
#'   \item \code{vignette("Survival_analysis_theory", package = "flexsurvPlus")}
#'   }
#'   
#' @return A list containing flexsurv objects.
#' @seealso \code{\link{flexsurvreg}}
#'
#' @export
fit_models <- function(model.formula,
                       distr = c('exp',
                                 'weibull',
                                 'gompertz',
                                 'lnorm',
                                 'llogis',
                                 'gengamma',
                                 'gamma',
                                 'genf'),
                       data) {
  
  
  
  runFLEX <- function(dist){
    tryCatch(model <- flexsurv::flexsurvreg(formula=model.formula, data=data, dist=dist),
             error = function(e){
               message("An error occurred in ",dist,":\n", e)
               return("error")
             },
             warning = function(w){
               message("A warning occured in ",dist,":\n",  w)
               return("warning")
             },
             finally = {
               message("Fitting model ", dist)
               
             })
  }
  
  #list of flexsurv objects
  output <- lapply(distr, function(x) runFLEX(x))
  names(output) <- distr
  output
}


