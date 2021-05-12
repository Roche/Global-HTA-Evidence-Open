# NOT EXPORTED - called by runPSM
# Run a complete parametric survival analysis for a separate model
# 
# Fits 2 separate \code{\link{flexsurv}} models using \code{\link{flexsurvreg}} (one for each treatment).
#
# @param data A data frame containing individual patient data for the relevant
#   time to event outcomes. This is passed to the \code{data} argument of the
#   \code{\link{fit_models}} function
# @param time_var Name of time variable in 'data'. Variable must be numerical and >0.
# @param  event_var Name of event variable in 'data'. Variable must be
#   numerical and contain 1's to indicate an event and 0 to indicate a censor.
# @param  strata_var Name of stratification variable in "data". This is usually
#   the treatment variable and must be categorical.
# @param int_name Character to indicate the name of the treatment of interest,
#   must be a level of the "strata_var" column in "data", used for labeling
#   the parameters.
# @param ref_name Character to indicate the name of the reference treatment,
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
#  \item Log-logistic ('llogis')
#   \item Generalized gamma ('gengamma')
#   \item Gamma ('gamma')
#   \item Generalised F ('genf')
#   }
#
#   The model fit is in the form Surv(Time, Event==1) ~ 1 and is fit twice (one
#   separate model for each of the two treatments). The parameters for each
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
#     of the weibull distribution for the intervention treatment and 'gengamma.mu.Reference' refers to the mu parameter
#     of the generalised gamma distribution for the reference treatment.}
#
# @export
run_separate <- function(data,
                         time_var, event_var,
                         distr = c('exp',
                                   'weibull',
                                   'gompertz',
                                   'lnorm',
                                   'llogis',
                                   'gengamma',
                                   'gamma', 
                                   'genf'),
                         strata_var,
                         int_name, ref_name){
  
  
  #test that only valid distributions have been provided
  #This is also tested within fit_models. Consider eliminating here to avoid redundancy
  allowed_dist <- c('exp', 'weibull', 'gompertz', 'lnorm', 'llogis', 'gengamma', 'gamma', 'genf')
  assertthat::assert_that(
    all(distr %in% allowed_dist),
    msg = "Only the following distributions are supported: 'exp', 'weibull', 'gompertz', 'lnorm', 'llogis', 'gengamma', 'gamma', 'genf' "
  )
  
  # standardise variable names
  data_standard=Format_data_separate(data, time_var, event_var, strata_var, int_name, ref_name)
  model.formula.sep=Surv(Time, Event==1) ~ 1
  
  #Fit the models for seven standard distributions
  message("Fitting separate shape models - intervention arm") 
  models.int <- fit_models(model.formula=model.formula.sep, distr = distr, data=data_standard$dat.int)
  message("Fitting separate shape models - reference arm") 
  models.ref <- fit_models(model.formula=model.formula.sep, distr = distr, data=data_standard$dat.ref)
  
  #get parameter estimates and model fit statistics
  model_summary.int <- get_model_summary(models=models.int)
  model_summary.ref <- get_model_summary(models=models.ref)
  

  # Filter on flexsurv models
  flexsurvreg.test.int <- sapply(models.int, function(x) class(x)=="flexsurvreg")
  models.flexsurv.int  <- models.int[flexsurvreg.test.int]
  converged_models.int <- names(models.flexsurv.int)
  
  flexsurvreg.test.ref <- sapply(models.ref, function(x) class(x)=="flexsurvreg")
  models.flexsurv.ref  <- models.ref[flexsurvreg.test.ref]
  converged_models.ref <- names(models.flexsurv.ref)
  
  
  #Extract parameter estimates
  coef.int <- lapply(models.flexsurv.int, coef)
  coef.ref <- lapply(models.flexsurv.ref, coef)

  if(length(converged_models.int)>0){
  param_out.int <- t(unlist(coef.int)) %>% as.data.frame()
  # If this is a separate model fitted to each treatment group, rename the parameter from the
  # exponential model to be consistent with output from Common shape models  suppressWarnings(colnames(param_out.int)[colnames(param_out.int) == 'exp'] <- "exp.rate")
  suppressWarnings(colnames(param_out.int) <- paste0(colnames(param_out.int),".int"))
  } else
  {param_out.int <- tibble()}
  
  
  if(length(converged_models.ref)>0){
  param_out.ref <- t(unlist(coef.ref)) %>% as.data.frame()
  # If this is a separate model fitted to each treatment group, rename the parameter from the
  # exponential model to be consistent with output from Common shape models  suppressWarnings(colnames(param_out.ref)[colnames(param_out.ref) == 'exp'] <- "exp.rate")
  suppressWarnings(colnames(param_out.ref) <- paste0(colnames(param_out.ref),".ref"))
  } else
  {param_out.ref <- tibble()}


  
  # rename for output
  names(models.int) <- paste0("sep.int.", names(models.int))
  names(models.ref) <- paste0("sep.ref.", names(models.ref))
  
  models <- c(models.int, models.ref)
  
  model_summary.int$Dist <-  paste0("sep.int.", model_summary.int$Dist)
  model_summary.ref$Dist <-  paste0("ref.int.", model_summary.ref$Dist)
  
  model_summary <- dplyr::bind_rows(model_summary.int, model_summary.ref)
  
  if(length(converged_models.int)+length(converged_models.ref)>0){
      param_out <- cbind(param_out.int, param_out.ref)  %>%
    as.data.frame() 
  } else {
    param_out <- aram_out.ref <- tibble()
      }
  
  #######################################################
  # prepare parameter outputs
  # this function exponentiates values that coef returns on the log scale
  # e.g. weibull shape and scale
  # this further simplifies other function use
  param_final <- post_process_param_out(param_out)
  
  model_summary.out <- model_summary %>%
    dplyr::mutate(Model="Seperate", Intervention_name=int_name, Reference_name=ref_name) %>%
    dplyr::select(Model, Dist, Intervention_name, Reference_name, Status, AIC, BIC)
  
  params_all <- param_final 
  
  if('exp' %in% distr){
    params_all$exp.rate.int <- ifelse("exp.rate.int" %in% names(params_all), params_all$exp.rate.int, NA) 
    params_all$exp.rate.ref <- ifelse("exp.rate.ref" %in% names(params_all), params_all$exp.rate.ref, NA) 
      }
  
  if('weibull' %in% distr){
    params_all$weibull.shape.int <- ifelse("weibull.shape.int" %in% names(params_all), params_all$weibull.shape.int, NA) 
    params_all$weibull.scale.int <- ifelse("weibull.scale.int" %in% names(params_all), params_all$weibull.scale.int, NA) 
    params_all$weibull.shape.ref <- ifelse("weibull.shape.ref" %in% names(params_all), params_all$weibull.shape.ref, NA) 
    params_all$weibull.scale.ref <- ifelse("weibull.scale.ref" %in% names(params_all), params_all$weibull.scale.ref, NA) 
  }
  
  if('gompertz' %in% distr){
    params_all$gompertz.shape.int <- ifelse("gompertz.shape.int" %in% names(params_all), params_all$gompertz.shape.int, NA) 
    params_all$gompertz.rate.int <- ifelse("gompertz.rate.int" %in% names(params_all), params_all$gompertz.rate.int, NA) 
    params_all$gompertz.shape.ref <- ifelse("gompertz.shape.ref" %in% names(params_all), params_all$gompertz.shape.ref, NA) 
    params_all$gompertz.rate.ref <- ifelse("gompertz.rate.ref" %in% names(params_all), params_all$gompertz.rate.ref, NA) 
     }
  
  if('llogis' %in% distr){
    params_all$llogis.shape.int <- ifelse("llogis.shape.int" %in% names(params_all), params_all$llogis.shape.int, NA) 
    params_all$llogis.scale.int <- ifelse("llogis.scale.int" %in% names(params_all), params_all$llogis.scale.int, NA) 
    params_all$llogis.shape.ref <- ifelse("llogis.shape.ref" %in% names(params_all), params_all$llogis.shape.ref, NA) 
    params_all$llogis.scale.ref <- ifelse("llogis.scale.ref" %in% names(params_all), params_all$llogis.scale.ref, NA) 
  }
  
  if('gamma' %in% distr){
    params_all$gamma.shape.int <- ifelse("gamma.shape.int" %in% names(params_all), params_all$gamma.shape.int, NA) 
    params_all$gamma.rate.int <- ifelse("gamma.rate.int" %in% names(params_all), params_all$gamma.rate.int, NA) 
    params_all$gamma.shape.ref <- ifelse("gamma.shape.ref" %in% names(params_all), params_all$gamma.shape.ref, NA) 
    params_all$gamma.rate.ref <- ifelse("gamma.rate.ref" %in% names(params_all), params_all$gamma.rate.ref, NA) 
      }
  
  if('lnorm' %in% distr){
    params_all$lnorm.meanlog.int <- ifelse("lnorm.meanlog.int" %in% names(params_all), params_all$lnorm.meanlog.int, NA) 
    params_all$lnorm.sdlog.int <- ifelse("lnorm.sdlog.int" %in% names(params_all), params_all$lnorm.sdlog.int, NA) 
    params_all$lnorm.meanlog.ref <- ifelse("lnorm.meanlog.ref" %in% names(params_all), params_all$lnorm.meanlog.ref, NA) 
    params_all$lnorm.sdlog.ref <- ifelse("lnorm.sdlog.ref" %in% names(params_all), params_all$lnorm.sdlog.ref, NA) 
  }
  
  if('gengamma' %in% distr){
    params_all$gengamma.mu.int <- ifelse("gengamma.mu.int" %in% names(params_all), params_all$gengamma.mu.int, NA) 
    params_all$gengamma.sigma.int <- ifelse("gengamma.sigma.int" %in% names(params_all), params_all$gengamma.sigma.int, NA) 
    params_all$gengamma.Q.int <- ifelse("gengamma.Q.int" %in% names(params_all), params_all$gengamma.Q.int, NA) 
    params_all$gengamma.mu.ref <- ifelse("gengamma.mu.ref" %in% names(params_all), params_all$gengamma.mu.ref, NA) 
    params_all$gengamma.sigma.ref <- ifelse("gengamma.sigma.ref" %in% names(params_all), params_all$gengamma.sigma.ref, NA) 
    params_all$gengamma.Q.ref <- ifelse("gengamma.Q.ref" %in% names(params_all), params_all$gengamma.Q.ref, NA) 
      }
  
  if('genf' %in% distr){
    params_all$genf.mu.int <- ifelse("genf.mu.int" %in% names(params_all), params_all$genf.mu.int, as.numeric(NA)) 
    params_all$genf.sigma.int <- ifelse("genf.sigma.int" %in% names(params_all), params_all$genf.sigma.int, as.numeric(NA)) 
    params_all$genf.Q.int <- ifelse("genf.Q.int" %in% names(params_all), params_all$genf.Q.int, as.numeric(NA)) 
    params_all$genf.P.int <- ifelse("genf.P.int" %in% names(params_all), params_all$genf.P.int, as.numeric(NA)) 
    params_all$genf.mu.ref <- ifelse("genf.mu.ref" %in% names(params_all), params_all$genf.mu.ref, as.numeric(NA)) 
    params_all$genf.sigma.ref <- ifelse("genf.sigma.ref" %in% names(params_all), params_all$genf.sigma.ref, as.numeric(NA)) 
    params_all$genf.Q.ref <- ifelse("genf.Q.ref" %in% names(params_all), params_all$genf.Q.ref, as.numeric(NA)) 
    params_all$genf.P.ref <- ifelse("genf.P.ref" %in% names(params_all), params_all$genf.P.ref, as.numeric(NA)) 
      }
  
  col_names <- c("exp.rate.int", "exp.rate.ref",  
                               "weibull.scale.int", "weibull.scale.ref", "weibull.shape.int", "weibull.shape.ref", 
                               "gompertz.rate.int",  "gompertz.rate.ref", "gompertz.shape.int", "gompertz.shape.ref",
                               "llogis.scale.int", "llogis.scale.ref", "llogis.shape.int", "llogis.shape.ref", 
                               "gamma.rate.int", "gamma.rate.ref", "gamma.shape.int", "gamma.shape.ref",
                               "lnorm.meanlog.int", "lnorm.meanlog.ref", "lnorm.sdlog.int", "lnorm.sdlog.ref",  
                               "gengamma.mu.int", "gengamma.mu.ref", "gengamma.sigma.int", "gengamma.sigma.ref", "gengamma.Q.int", "gengamma.Q.ref",
                               "genf.mu.int", "genf.mu.ref", "genf.sigma.int", "genf.sigma.ref", "genf.Q.int", "genf.Q.ref", "genf.P.int", "genf.P.ref")

  col_names_final <- col_names[col_names %in%  names(params_all) ]

  params_all <- params_all %>%
    select(col_names_final)
  
  
  # as a vector version with just numerics - needed for bootstrapping
  paramV <- as.numeric(params_all)
  names(paramV) <- paste0("sep.", colnames(params_all))
  
  
  
  
  #######################################################
  #collect and return output
  output <- list(
    models = models,
    model_summary = model_summary.out,
    parameters_vector = paramV
  )
  
  return(output)
  }
