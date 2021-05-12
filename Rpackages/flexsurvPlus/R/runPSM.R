#' Run complete parametric survival analysis for multiple models with multiple distributions
#'
#' @param data A data frame containing individual patient data for the relevant
#'   time to event outcomes. This is passed to the \code{data} argument of the
#'   \code{\link{fit_models}} function
#' @param time_var Name of time variable in 'data'. Variable must be numerical and >0.
#' @param  event_var Name of event variable in 'data'. Variable must be
#'   numerical and contain 1's to indicate an event and 0 to indicate a censor.
#' @param model.type Character vector indicating the types of model formula
#'   provided. Permitted values are
#' \itemize{
#'   \item 'Common shape' a model with a single covariate for the effect of
#'   treatment on the scale parameter of the model. 
#'   The model fit is in the form Surv(Time, Event==1) ~ ARM.
#'   The shape parameter is the same for each treatment, and derived directly from 
#'   the model (no additional manipulation is required).
#'   The scale parameter is derived directly from the model for the reference category, however, 
#'   for the intervention arm, this is calculated as reference shape + treatment effect (shape).
#'   \item 'Independent shape' a model with a single covariate for treatment
#'   that affects both the scale and shape parameters of the model. 
#'   The model fit is in the form Surv(Time, Event==1) ~ ARM + shape(ARM). 
#'   The scale parameter is derived directly from the model for the reference
#'   category, however, for the intervention arm, this is calculated as reference
#'   scale + treatment effect (scale). 
#'   The shape parameter is derived directly from the model for the reference category, 
#'   however, for the intervention arm, this is calculated as reference shape + treatment effect (shape).
#'   \item 'Separate' a model with no covariates fitted separately to
#'   data from each treatment group in a study. 
#'   The model fit is in the form Surv(Time, Event==1) ~ 1 and is fit twice (one separate 
#'   model for each of the two treatments). The parameters for each treatment, are derived 
#'   directly from the model (no additional manipulation is required).
#'   \item 'One arm' a model with no covariates is fitted to the entire data
#'   set without a strata variable. 
#'   The model fit is in the form Surv(Time, Event==1) ~ 1 and is fit to the entire data (no strata). 
#'   The parameters for each treatment, are derived directly from the model (no additional manipulation
#'   is required).
#'  }
#'  Default is c("Separate", "Common shape", "Independent shape").
#' @param  strata_var Name of stratification variable in "data". This is usually
#'   the treatment variable and must be categorical. Not required when model.type='One arm'.
#' @param int_name Character to indicate the name of the treatment of interest,
#'   must be a level of the "strata_var" column in "data", used for labelling
#'   the parameters.
#' @param ref_name Character to indicate the name of the reference treatment,
#'    must be a level of the "strata_var" column in "data", used for labelling
#'    the parameters. Not required when model.type='One arm'.
#' @param distr A vector string of distributions, see dist argument in
#'   \code{\link{flexsurvreg}}. Default is all available distributions (see below).
#'
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
#'
#' For more details and examples see the package vignettes:
#' \itemize{
#'   \item \code{vignette("Fitting_models_in_R", package = "flexsurvPlus")}
#'   \item \code{vignette("Fitting_models_in_R_bootstrap", package = "flexsurvPlus")}
#'   \item \code{vignette("Survival_analysis_theory", package = "flexsurvPlus")}
#'   }
#' 
#' @return A list containing 'models' (output from \code{\link{fit_models}}), 'model_summary' (output from \code{\link{get_model_summary}}) and
#'   'parameters', a data frame containing the coefficients of each flexsurv model.
#' \itemize{
#'   \item 'models' is a list of flexsurv objects for each distribution specified
#'   \item 'model_summary' is a tibble object containing the fitted model objects, the parameter
#'   estimates (\code{\link{coef}}),  \code{\link{AIC}} and \code{\link{BIC}}
#'   from flexsurv objects.
#'    \item 'parameters_vector' is a vector which contains the coefficients for all of the flexsurv models specified.
#'    The column names are in the format 'modeltype.distribution.parameter.TreatmentName', for example, comshp.weibull.shape.Int refers to the shape parameter
#'     of the common shape weibull distribution for the intervention treatment and 'indshp.gengamma.mu.ref' refers to the mu parameter
#'     of the independent shape generalised gamma distribution for the reference treatment. Columns with 'TE' at the end are the treatment effect coefficients
#'      (applicable to the scale and shape parameters for independent shape models, applicable to the scale parameter only for the common shape
#'      model and not applicable for the separate or one-arm model).
#'      }
#'      
#' @export
runPSM <- function(data,
                   time_var, event_var,
                   model.type = c("Separate", "Common shape", "Independent shape"),
                   distr = c('exp',
                             'weibull',
                             'gompertz',
                             'lnorm',
                             'llogis',
                             'gengamma',
                             'gamma',
                             'genf'
                             ),
                   strata_var,
                   int_name, ref_name){



  #test that a legitimate value for model type has been provided
  assertthat::assert_that(
    all(model.type %in% c('Common shape', 'Independent shape', 'Separate', 'One arm')),
    msg = "Only the following model types are supported are supported: 'Common shape', 'Independent shape', 'Separate', 'One arm' "
  )

  #For models with common shape, calculate the location
  #parameter for the treatment arm for each distribution

  models <- list()
  model_summary <- tibble()
  parameters_vector <- numeric()
  
  if('Common shape' %in% model.type){
    output1 <- run_common_shape(data, time_var, event_var,distr,strata_var, int_name, ref_name)
    models <- output1$models
    model_summary <- output1$model_summary
    parameters_vector <- output1$parameters_vector
  }

  # For separate models split the data by treatment and for 2 separate models
  # for each distribution

  if('Separate' %in% model.type){
    output2 <- run_separate(data, time_var, event_var, distr, strata_var, int_name, ref_name)
    models <- c(models, output2$models)
    model_summary <- dplyr::bind_rows(model_summary, output2$model_summary)
    parameters_vector <- c(parameters_vector, output2$parameters_vector)
  }

  #For models with independent shape calculate the scale and shape parameters for the
  #treatment arm for each distribution
  if('Independent shape' %in% model.type){
    output3 <- run_independent_shape(data, time_var, event_var, distr, strata_var, int_name, ref_name)
    models <- c(models, output3$models)
    model_summary <- dplyr::bind_rows(model_summary, output3$model_summary)
    parameters_vector <- c(parameters_vector, output3$parameters_vector)
  }
  
  if('One arm' %in% model.type){
    output4 <- run_one_arm(data, time_var, event_var, distr, int_name)
    models <- c(models, output4$models)
    model_summary <- dplyr::bind_rows(model_summary, output4$model_summary)
    parameters_vector <- c(parameters_vector, output4$parameters_vector)
  }

  # combine the outputs
  output <- list(models = models,
                 model_summary = model_summary,
                 parameters_vector = parameters_vector
                 )
  
  return(output)
}
