# NOT EXPORTED - called by runPSM
# Run a complete parametric survival analysis for one-arm
#
# Fits a single \code{\link{flexsurv}} models using \code{\link{flexsurvreg}}.
#
# @param data A data frame containing individual patient data for the relevant
#   time to event outcomes. This is passed to the \code{data} argument of the
#   \code{\link{fit_models}} function
# @param time_var Name of time variable in 'data'. Variable must be numerical and >0.
# @param  event_var Name of event variable in 'data'. Variable must be
#   numerical and contain 1's to indicate an event and 0 to indicate a censor.
# @param int_name Character to indicate the name of the treatment of interest,
#   must be a level of the "strata_var" column in "data", used for labeling
#   the parameters.
# @param distr A vector string of distributions, see dist argument in
#   \code{\link{flexsurvreg}}. This is passed to the \code{distr} argument of
#   the \code{\link{fit_models}} function. Default is all available distributions (see below).
# @details Possible distributions include:
# \itemize{
#   \item Exponential ('exp')
#   \item Weibull ('weibull')
#   \item Gompertz ('gompertz')
#   \item Log-normal ('lnorm')
#   \item Log-logistic ('llogis')
#   \item Generalized gamma ('gengamma')
#   \item Gamma ('gamma')
#   \item Generalised F ('genf')
#   }
#
#   The model fit is in the form Surv(Time, Event==1) ~ 1 and is fit to the entire data (no strata). The parameters for each
#   treatment, are derived directly from the model (no additional manipulation
#   is required).
# @return A list containing 'models' (output from \code{\link{fit_models}}), 'model_summary' (output from\code{\link{get_model_summary}}) and
#   'parameters', a data frame containing the coefficients of each flexsurv model.
# \itemize{
#   \item 'models' is a list of flexsurv objects for each distribution specified
#   \item 'model_summary' is a tibble object containing the fitted model objects, the parameter
#   estimates (\code{\link{coef}}),  \code{\link{AIC}} and \code{\link{BIC}}
#   from flexsurv objects.
#   \item 'parameters' is a data frame with with one row which contains the coefficients for all of the flexsurv models specified.
#    The column names are in the format 'distribution.parameter.TreatmentName', for example, weibull.shape.Intervention refers to the shape parameter
#     of the weibull distribution for the intervention treatment.}
#
# @export
run_one_arm <- function(data,
                   time_var, event_var,
                   distr = c('exp',
                             'weibull',
                             'gompertz',
                             'lnorm',
                             'llogis',
                             'gengamma',
                             'gamma', 
                             'genf'),
                      int_name){


  #test that only valid distributions have been provided
  #This is also tested within fit_models. Consider eliminating here to avoid redundancy
  allowed_dist <- c('exp', 'weibull', 'gompertz', 'lnorm', 'llogis', 'gengamma', 'gamma', 'genf')
  assertthat::assert_that(
    all(distr %in% allowed_dist),
    msg = "Only the following distributions are supported: 'exp', 'weibull', 'gompertz', 'lnorm', 'llogis', 'gengamma', 'gamma', 'genf' "
  )

  # standardise variable names
  data_standard=Format_data_onearm(data, time_var, event_var, int_name)
  model.formula.one.arm=Surv(Time, Event==1) ~ 1

  #Fit the models for seven standard distributions
  message("Fitting one arm models")
  models <- fit_models(model.formula=model.formula.one.arm, distr = distr, data=data_standard)

  #get parameter estimates and model fit statistics
  model_summary <- get_model_summary(models=models)

  # Filter on flexsurv models
  flexsurvreg.test <- sapply(models, function(x) class(x)=="flexsurvreg")
  models.flexsurv  <- models[flexsurvreg.test]
  converged_models <- names(models.flexsurv)
  
  
  #Extract parameter estimates
  coef <- lapply(models.flexsurv, coef)
  
  if(length(converged_models)>0){
  param_out <- t(unlist(coef)) %>% as.data.frame()
  # Rename the parameter from the
  # exponential model to be consistent with output from other models
  suppressWarnings(colnames(param_out)[colnames(param_out) == 'exp'] <- "exp.rate")
  suppressWarnings(colnames(param_out) <- paste0(colnames(param_out),".int"))
  } else
  {param_out <- tibble()}
  
  
  # rename for output
  names(models) <- paste0("onearm.int.", names(models))

  models <- c(models)
  
  model_summary$Dist <-  paste0("onearm.int.", model_summary$Dist)

  
  #######################################################
  # prepare parameter outputs
  # this function exponentiates values that coef returns on the log scale
  # e.g. weibull shape and scale
  # this further simplifies other function use
  param_final <- post_process_param_out(param_out)
  
  model_summary.out <- model_summary %>%
    dplyr::mutate(Model="One arm", Intervention_name=int_name) %>%
    dplyr::select(Model, Dist, Intervention_name, Status, AIC, BIC)
  

  params_all <- param_final 
  
  if('exp' %in% distr){
    params_all$exp.rate.int <- ifelse("exp.rate.int" %in% names(params_all), params_all$exp.rate.int, NA) 
  
    }
  
  if('weibull' %in% distr){
    params_all$weibull.shape.int <- ifelse("weibull.shape.int" %in% names(params_all), params_all$weibull.shape.int, NA) 
    params_all$weibull.scale.int <- ifelse("weibull.scale.int" %in% names(params_all), params_all$weibull.scale.int, NA) 
  }
  
  if('gompertz' %in% distr){
    params_all$gompertz.shape.int <- ifelse("gompertz.shape.int" %in% names(params_all), params_all$gompertz.shape.int, NA) 
    params_all$gompertz.rate.int <- ifelse("gompertz.rate.int" %in% names(params_all), params_all$gompertz.rate.int, NA) 
    }
  
  if('llogis' %in% distr){
    params_all$llogis.shape.int <- ifelse("llogis.shape.int" %in% names(params_all), params_all$llogis.shape.int, NA) 
    params_all$llogis.scale.int <- ifelse("llogis.scale.int" %in% names(params_all), params_all$llogis.scale.int, NA) 
    }
  
  if('gamma' %in% distr){
    params_all$gamma.shape.int <- ifelse("gamma.shape.int" %in% names(params_all), params_all$gamma.shape.int, NA) 
    params_all$gamma.rate.int <- ifelse("gamma.rate.int" %in% names(params_all), params_all$gamma.rate.int, NA) 
      }
  
  if('lnorm' %in% distr){
    params_all$lnorm.meanlog.int <- ifelse("lnorm.meanlog.int" %in% names(params_all), params_all$lnorm.meanlog.int, NA) 
    params_all$lnorm.sdlog.int <- ifelse("lnorm.sdlog.int" %in% names(params_all), params_all$lnorm.sdlog.int, NA) 
  }
  
  if('gengamma' %in% distr){
    params_all$gengamma.mu.int <- ifelse("gengamma.mu.int" %in% names(params_all), params_all$gengamma.mu.int, NA) 
    params_all$gengamma.sigma.int <- ifelse("gengamma.sigma.int" %in% names(params_all), params_all$gengamma.sigma.int, NA) 
    params_all$gengamma.Q.int <- ifelse("gengamma.Q.int" %in% names(params_all), params_all$gengamma.Q.int, NA) 
  }
  
  if('genf' %in% distr){
    params_all$genf.mu.int <- ifelse("genf.mu.int" %in% names(params_all), params_all$genf.mu.int, as.numeric(NA)) 
    params_all$genf.sigma.int <- ifelse("genf.sigma.int" %in% names(params_all), params_all$genf.sigma.int, as.numeric(NA)) 
    params_all$genf.Q.int <- ifelse("genf.Q.int" %in% names(params_all), params_all$genf.Q.int, as.numeric(NA)) 
    params_all$genf.P.int <- ifelse("genf.P.int" %in% names(params_all), params_all$genf.P.int, as.numeric(NA)) 
  }
  
  col_names <- c("exp.rate.int",  
                 "weibull.scale.int", "weibull.shape.int",
                 "gompertz.rate.int", "gompertz.shape.int",
                 "llogis.scale.int", "llogis.shape.int",
                 "gamma.rate.int",  "gamma.shape.int",
                 "lnorm.meanlog.int", "lnorm.sdlog.int",
                 "gengamma.mu.int", "gengamma.sigma.int", "gengamma.Q.int",
                 "genf.mu.int",  "genf.sigma.int", "genf.Q.int", "genf.P.int")
  
  col_names_final <- col_names[col_names %in%  names(params_all) ]
  
  params_all <- params_all %>%
    select(col_names_final)
  
  # as a vector version with just numerics - needed for bootstrapping
  paramV <- as.numeric(params_all)
  names(paramV) <- paste0("onearm.", colnames(params_all))


  
  
  #######################################################
  #collect and return output
  output <- list(
    models = models,
    model_summary = model_summary.out,
    parameters_vector = paramV
  )

  return(output)
}
