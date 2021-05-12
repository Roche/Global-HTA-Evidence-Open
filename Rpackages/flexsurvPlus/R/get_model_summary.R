
#' Get model summary from \code{\link{fit_models}} objects
#'
#' Manipulates \code{\link{fit_models}} objects to get a model summary of AIC and BIC and status of whether the model converged or produced an error or warning.
#'
#' For more details and examples see the package vignettes:
#' \itemize{
#'   \item \code{vignette("Fitting_models_in_R", package = "flexsurvPlus")}
#'   \item \code{vignette("Fitting_models_in_R_bootstrap", package = "flexsurvPlus")}
#'   \item \code{vignette("Survival_analysis_theory", package = "flexsurvPlus")}
#'   }
#'
#' @param models Object from \code{\link{fit_models}}
#'
#' @return A data frame containing the \code{\link{AIC}}, \code{\link{BIC}}
#'   from flexsurv objects and a status column of whether the model converged or produced an error or warning.
#'
#' @seealso \code{\link{fit_models}} \code{\link{flexsurvreg}}
#'
#' @export
get_model_summary <- function(models) {
  
  # Filter on flexsurv models
  flexsurvreg.test <- sapply(models, function(x) class(x)=="flexsurvreg")
  models.flexsurv  <- models[flexsurvreg.test]
  
  #models that produced an error or warning
  models.nonconverge <- models[flexsurvreg.test==FALSE]
  
  #test inputs before proceeding
  input.class <- sapply(models.flexsurv, class)
  assertthat::assert_that(
    all(input.class == "flexsurvreg"),
    msg = "get_params expects a list of 'flexsurvreg' objects as input. At least one of your inputs is not a flexsurvreg object"
  )
  
  output <-   tibble::enframe(models.flexsurv) %>%
    dplyr::mutate(
      Dist = name,
      AIC = sapply(models.flexsurv, AIC), #get AIC
      BIC = sapply(models.flexsurv, BIC), #get BIC
      Status = "Converged"
    ) %>%
    dplyr::select(-value, -name) %>% 
    as.data.frame()
  
  if(length(models.nonconverge)>0){
    non_converged_output  <- data.frame(
      Dist = names(models.nonconverge),
      AIC = NA,
      BIC = NA,
      Status = unlist(models.nonconverge))
    rownames(non_converged_output) <- c()
    
    output <- rbind(output, non_converged_output)
  }
  
  output
  
}