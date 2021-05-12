# NOT EXPORTED - called by runPSM
# Run a complete parametric survival analysis for an independent shape model
#
# Fits \code{\link{flexsurv}} models using \code{\link{flexsurvreg}}
# containing a covariate for treatment on shape and scale parameter.
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
#  @param ref_name Character to indicate the name of the reference treatment,
#    must be a level of the "strata_var" column in "data", used for labeling
#    the parameters.
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
#   The model fit is in the form Surv(Time, Event==1) ~ ARM + shape(ARM). The
#   scale parameter is derived directly from the model for the reference
#   category, however for the intervention arm, this is calculated as reference
#   scale + treatment effect (scale). The shape parameter is derived directly
#   from the model for the reference category, however for the intervention
#   arm, this is calculated as reference shape + treatment effect (shape).
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
#     of the generalised gamma distribution for the reference treatment. Columns with 'TE' at the end are the treatment effect coefficients
#      (applicable to the scale and shape parameters for independent shape models).}
#
# @export
run_independent_shape <- function(data,
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
    msg = "Only the following distributions are supported: 'exp', weibull', 'gompertz', 'lnorm', 'llogis', 'gengamma', 'gamma', 'genf' "
  )
  
  # standardise variable names
  data_standard=Format_data(data, time_var, event_var, strata_var, int_name, ref_name)
  
  # Model formulas
  model.formula.int = Surv(Time, Event==1) ~ ARM
  model.formula.shape = Surv(Time, Event==1) ~ ARM + shape(ARM)
  model.formula.sdlog = Surv(Time, Event==1) ~ ARM + sdlog(ARM)
  model.formula.sigma_Q = Surv(Time, Event==1) ~ ARM + sigma(ARM) + Q(ARM)
  model.formula.sigma_Q_P = Surv(Time, Event==1) ~ ARM + sigma(ARM) + Q(ARM) + P(ARM)
  
  models <- list()
  model_summary <- tibble()
  params_out <- tibble(.rows = 1)
  
  message("Fitting independant shape models")
  
  if('exp' %in% distr){
    models.exp <- fit_models(model.formula=model.formula.int, distr = "exp", data=data_standard)
    model_summary.exp <- get_model_summary(models=models.exp)
    model_summary.exp$Dist <- "indshp.exp"
    model_summary <- dplyr::bind_rows(model_summary, model_summary.exp)
    if(class(models.exp$exp)=="flexsurvreg"){
      coef.exp <- lapply(models.exp, coef)
      param_out.exp <- t(unlist(coef.exp)) %>% as.data.frame() %>%
      dplyr::mutate(
        exp.rate.int = exp.rate + exp.ARMInt,
        exp.rate.ref = exp.rate,
        exp.rate.TE = exp.ARMInt) %>%
      dplyr::select(-exp.rate, -exp.ARMInt)
      param_out.exp <- post_process_param_out(param_out.exp) 
    }
    else {
      param_out.exp <-data.frame(
        exp.rate.int = NA,
        exp.rate.ref = NA,
        exp.rate.TE = NA
      )
    }
    # append this model to output 
    models$indshp.exp <- models.exp$exp
    params_out <- dplyr::bind_cols(params_out, param_out.exp)
    
  }
  
  if('weibull' %in% distr){
    models.weib <- fit_models(model.formula=model.formula.shape, distr = "weibull", data=data_standard)
    model_summary.weib <- get_model_summary(models=models.weib)
    model_summary.weib$Dist <- "indshp.weibull"
    model_summary <- dplyr::bind_rows(model_summary, model_summary.weib)
    if(class(models.weib$weibull)=="flexsurvreg"){
      coef.weib <- lapply(models.weib, coef)
      param_out.weib <- t(unlist(coef.weib)) %>% as.data.frame() %>%
        dplyr::mutate(
        weibull.scale.int = weibull.scale + weibull.ARMInt,
        weibull.scale.ref = weibull.scale,
        weibull.shape.int = weibull.shape + `weibull.shape(ARMInt)`,
        weibull.shape.ref = weibull.shape,
        weibull.scale.TE = weibull.ARMInt,
        weibull.shape.TE = `weibull.shape(ARMInt)`) %>%
      select(-weibull.scale, -weibull.shape, -weibull.ARMInt, -`weibull.shape(ARMInt)`)
      param_out.weib <- post_process_param_out(param_out.weib) 
    }
    else {
      param_out.weib <-data.frame(
        weibull.scale.int = NA,
        weibull.scale.ref = NA,
        weibull.shape.int = NA,
        weibull.shape.ref = NA,
        weibull.scale.TE = NA,
        weibull.shape.TE = NA
      )
    }
    # append this model to output 
    models$indshp.weibull <- models.weib$weibull
    params_out <- dplyr::bind_cols(params_out, param_out.weib)
    
  }
  
  if('gompertz' %in% distr){
    models.gomp <- fit_models(model.formula=model.formula.shape, distr = "gompertz", data=data_standard)
    model_summary.gomp <- get_model_summary(models=models.gomp)
    model_summary.gomp$Dist <- "indshp.gompertz"
    model_summary <- dplyr::bind_rows(model_summary, model_summary.gomp)
    if(class(models.gomp$gompertz)=="flexsurvreg"){
      coef.gomp <- lapply(models.gomp, coef)
      param_out.gomp <- t(unlist(coef.gomp)) %>% as.data.frame() %>%
        dplyr::mutate(
        gompertz.rate.int = gompertz.rate + gompertz.ARMInt,
        gompertz.rate.ref = gompertz.rate,
        gompertz.shape.int = gompertz.shape + `gompertz.shape(ARMInt)`,
        gompertz.shape.ref = gompertz.shape,
        gompertz.rate.TE = gompertz.ARMInt,
        gompertz.shape.TE = `gompertz.shape(ARMInt)`) %>%
      select(-gompertz.rate, -gompertz.shape, -gompertz.ARMInt,-`gompertz.shape(ARMInt)`)
      param_out.gomp <- post_process_param_out(param_out.gomp) 
    }
    else {
      param_out.gomp <-data.frame(
        gompertz.rate.int = NA,
        gompertz.rate.ref = NA,
        gompertz.shape.int = NA,
        gompertz.shape.ref = NA,
        gompertz.rate.TE = NA,
        gompertz.shape.TE = NA
      )
    }
    # append this model to output 
    models$indshp.gompertz <- models.gomp$gompertz
    params_out <- dplyr::bind_cols(params_out, param_out.gomp)
  
  }
  
  if('llogis' %in% distr){
    models.llogis <- fit_models(model.formula=model.formula.shape, distr = "llogis", data=data_standard)
    model_summary.llogis <- get_model_summary(models=models.llogis)
    model_summary.llogis$Dist <- "indshp.llogis"
    model_summary <- dplyr::bind_rows(model_summary, model_summary.llogis)
    if(class(models.llogis$llogis)=="flexsurvreg"){
      coef.llogis <- lapply(models.llogis, coef)
      param_out.llogis <- t(unlist(coef.llogis)) %>% as.data.frame() %>%
        dplyr::mutate(
        llogis.scale.int = llogis.scale + llogis.ARMInt,
        llogis.scale.ref = llogis.scale,
        llogis.shape.int = llogis.shape + `llogis.shape(ARMInt)`,
        llogis.shape.ref = llogis.shape,
        llogis.scale.TE = llogis.ARMInt,
        llogis.shape.TE = `llogis.shape(ARMInt)`) %>%
      select(-llogis.scale, -llogis.shape, -llogis.ARMInt, -`llogis.shape(ARMInt)`)
      param_out.llogis <- post_process_param_out(param_out.llogis) 
    }
    else {
      param_out.llogis <-data.frame(
        llogis.scale.int = NA,
        llogis.scale.ref = NA,
        llogis.shape.int = NA,
        llogis.shape.ref = NA,
        llogis.scale.TE = NA,
        llogis.shape.TE = NA
      )
    }
    # append this model to output 
    models$indshp.llogis <- models.llogis$llogis
    params_out <- dplyr::bind_cols(params_out, param_out.llogis)
    
  }
  
  if('gamma' %in% distr){
    models.gamma <- fit_models(model.formula=model.formula.shape, distr = "gamma", data=data_standard)
    model_summary.gamma <- get_model_summary(models=models.gamma)
    model_summary.gamma$Dist <- "indshp.gamma"
    model_summary <- dplyr::bind_rows(model_summary, model_summary.gamma)
    if(class(models.gamma$gamma)=="flexsurvreg"){
      coef.gamma <- lapply(models.gamma, coef)
      param_out.gamma <- t(unlist(coef.gamma)) %>% as.data.frame() %>%
        dplyr::mutate(
        gamma.rate.int = gamma.rate + gamma.ARMInt,
        gamma.rate.ref = gamma.rate,
        gamma.shape.int = gamma.shape + `gamma.shape(ARMInt)`,
        gamma.shape.ref = gamma.shape,
        gamma.rate.TE = gamma.ARMInt,
        gamma.shape.TE = `gamma.shape(ARMInt)`) %>%
      select(-gamma.rate, -gamma.shape, -gamma.ARMInt, -`gamma.shape(ARMInt)`)
      param_out.gamma <- post_process_param_out(param_out.gamma) 
    }
    else {
      param_out.gamma <-data.frame(
        gamma.rate.int = NA,
        gamma.rate.ref = NA,
        gamma.shape.int = NA,
        gamma.shape.ref = NA,
        gamma.rate.TE = NA,
        gamma.shape.TE = NA
        ) 
    }
    # append this model to output 
    models$indshp.gamma <- models.gamma$gamma
    params_out <- dplyr::bind_cols(params_out, param_out.gamma)
    
  }
  
  if('lnorm' %in% distr){
    models.lnorm <- fit_models(model.formula=model.formula.sdlog, distr = "lnorm", data=data_standard)
    model_summary.lnorm <- get_model_summary(models=models.lnorm)
    model_summary.lnorm$Dist <- "indshp.lnorm"
    model_summary <- dplyr::bind_rows(model_summary, model_summary.lnorm)
    if(class(models.lnorm$lnorm)=="flexsurvreg"){
      coef.lnorm <- lapply(models.lnorm, coef)
      param_out.lnorm <- t(unlist(coef.lnorm)) %>% as.data.frame() %>%
        dplyr::mutate(
        lnorm.meanlog.int = lnorm.meanlog + lnorm.ARMInt,
        lnorm.meanlog.ref = lnorm.meanlog,
        lnorm.sdlog.int = lnorm.sdlog + `lnorm.sdlog(ARMInt)`,
        lnorm.sdlog.ref = lnorm.sdlog,
        lnorm.meanlog.TE = lnorm.ARMInt,
        lnorm.sdlog.TE = `lnorm.sdlog(ARMInt)`) %>%
      select(-lnorm.meanlog, -lnorm.sdlog, -lnorm.ARMInt, -`lnorm.sdlog(ARMInt)`)
      param_out.lnorm <- post_process_param_out(param_out.lnorm) 
    }
    else {
      param_out.lnorm <-data.frame(
        lnorm.meanlog.int = NA,
        lnorm.meanlog.ref = NA,
        lnorm.sdlog.int = NA,
        lnorm.sdlog.ref = NA,
        lnorm.meanlog.TE = NA,
        lnorm.sdlog.TE = NA
      )
    } 
    # append this model to output 
    models$indshp.lnorm <- models.lnorm$lnorm
    params_out <- dplyr::bind_cols(params_out, param_out.lnorm)
    
  }
  
  if('gengamma' %in% distr){
    models.gengamma <- fit_models(model.formula=model.formula.sigma_Q, distr = "gengamma", data=data_standard)
    model_summary.gengamma <- get_model_summary(models=models.gengamma)
    model_summary.gengamma$Dist <- "indshp.gengamma"
    model_summary <- dplyr::bind_rows(model_summary, model_summary.gengamma)
if(class(models.gengamma$gengamma)=="flexsurvreg"){
      coef.gengamma <- lapply(models.gengamma, coef)
      param_out.gengamma <- t(unlist(coef.gengamma)) %>% as.data.frame() %>%
        dplyr::mutate(
        gengamma.mu.int = gengamma.mu + gengamma.ARMInt,
        gengamma.mu.ref = gengamma.mu,
        gengamma.sigma.int = gengamma.sigma + `gengamma.sigma(ARMInt)`,
        gengamma.sigma.ref = gengamma.sigma,
        gengamma.Q.int = gengamma.Q + `gengamma.Q(ARMInt)`,
        gengamma.Q.ref = gengamma.Q,
        gengamma.mu.TE = gengamma.ARMInt,
        gengamma.sigma.TE = `gengamma.sigma(ARMInt)`,
        gengamma.Q.TE = `gengamma.Q(ARMInt)`) %>%
      select(-gengamma.mu, -gengamma.sigma, -gengamma.Q, -gengamma.ARMInt, -`gengamma.sigma(ARMInt)`, -`gengamma.Q(ARMInt)`)
      param_out.gengamma <- post_process_param_out(param_out.gengamma) 
}
    else {
      param_out.gengamma <-data.frame(
        gengamma.mu.int = NA,
        gengamma.mu.ref = NA,
        gengamma.sigma.int = NA,
        gengamma.sigma.ref = NA,
        gengamma.Q.int = NA,
        gengamma.Q.ref = NA,
        gengamma.mu.TE = NA,
        gengamma.sigma.TE = NA,
        gengamma.Q.TE = NA
      )
    }
      # append this model to output 
    models$indshp.gengamma <- models.gengamma$gengamma
    params_out <- dplyr::bind_cols(params_out, param_out.gengamma)
 
  }
    
  if('genf' %in% distr){
    models.genf <- fit_models(model.formula=model.formula.sigma_Q_P, distr = "genf", data=data_standard)
    model_summary.genf <- get_model_summary(models=models.genf)
    model_summary.genf$Dist <- "indshp.genf"
    model_summary <- dplyr::bind_rows(model_summary, model_summary.genf)
  if(class(models.genf$genf)=="flexsurvreg"){
      coef.genf <- lapply(models.genf, coef)
      param_out.genf <- t(unlist(coef.genf)) %>% as.data.frame() %>%
        dplyr::mutate(
        genf.mu.int = genf.mu + genf.ARMInt,
        genf.mu.ref = genf.mu,
        genf.sigma.int = genf.sigma + `genf.sigma(ARMInt)`,
        genf.sigma.ref = genf.sigma,
        genf.Q.int = genf.Q + `genf.Q(ARMInt)`,
        genf.Q.ref = genf.Q,
        genf.P.int = genf.P + `genf.P(ARMInt)`,
        genf.P.ref = genf.P,
        genf.mu.TE = genf.ARMInt,
        genf.sigma.TE = `genf.sigma(ARMInt)`,
        genf.Q.TE = `genf.Q(ARMInt)`,
        genf.P.TE = `genf.P(ARMInt)`) %>%
      select(-genf.mu, -genf.sigma, -genf.Q, -genf.P, -genf.ARMInt, -`genf.sigma(ARMInt)`, -`genf.Q(ARMInt)`, -`genf.P(ARMInt)`)
      param_out.genf <- post_process_param_out(param_out.genf) 
  }
    else {
      param_out.genf <-data.frame(
        genf.mu.int = NA,
        genf.mu.ref = NA,
        genf.sigma.int =NA,
        genf.sigma.ref = NA,
        genf.Q.int = NA,
        genf.Q.ref = NA,
        genf.P.int = NA,
        genf.P.ref = NA,
        genf.mu.TE = NA,
        genf.sigma.TE = NA,
        genf.Q.TE = NA,
        genf.P.TE = NA
        )
    }
    # append this model to output 
    models$indshp.genf <- models.genf$genf
    params_out <- dplyr::bind_cols(params_out, param_out.genf)
 
  }
  #######################################################
  # prepare parameter outputs
  # this function exponentiates values that coef returns on the log scale
  # e.g. weibull shape and scale
  # this further simplifies other function use
 
  # param_final <- post_process_param_out(params_out)
  
  model_summary.out <- model_summary %>%
    dplyr::mutate(Model="Independent shape", Intervention_name=int_name, Reference_name=ref_name) %>%
    dplyr::select(Model, Dist, Intervention_name, Reference_name, Status, AIC, BIC)
  
  
  col_names <- c("exp.rate.int", "exp.rate.ref", "exp.rate.TE", 
                 "weibull.scale.int", "weibull.scale.ref", "weibull.shape.int", "weibull.shape.ref", "weibull.scale.TE", "weibull.shape.TE",
                 "gompertz.rate.int",  "gompertz.rate.ref", "gompertz.shape.int", "gompertz.shape.ref", "gompertz.rate.TE", "gompertz.shape.TE",
                 "llogis.scale.int", "llogis.scale.ref", "llogis.shape.int", "llogis.shape.ref", "llogis.scale.TE", "llogis.shape.TE",
                 "gamma.rate.int", "gamma.rate.ref", "gamma.shape.int", "gamma.shape.ref", "gamma.rate.TE", "gamma.shape.TE",
                 "lnorm.meanlog.int", "lnorm.meanlog.ref", "lnorm.sdlog.int", "lnorm.sdlog.ref", "lnorm.meanlog.TE", "lnorm.sdlog.TE", 
                 "gengamma.mu.int", "gengamma.mu.ref", "gengamma.sigma.int", "gengamma.sigma.ref", "gengamma.Q.int", "gengamma.Q.ref", "gengamma.mu.TE","gengamma.sigma.TE", "indshp.gengamma.Q.TE", 
                 "genf.mu.int", "genf.mu.ref", "genf.sigma.int", "genf.sigma.ref", "genf.Q.int", "genf.Q.ref", "genf.P.int", "genf.P.ref", "genf.mu.TE", "genf.sigma.TE", "genf.Q.TE" , "genf.P.TE")
  
  
  col_names_final <- col_names[col_names %in%  names(params_out) ]
  
  param_final <- params_out %>%
    select(col_names_final)
  
  # as a vector version with just numerics - needed for bootstrapping
  paramV <- as.numeric(param_final)
  names(paramV) <- paste0("indshp.", colnames(param_final))
  
  #######################################################
  
  
  # prepare parameter outputs
  
  output <- list(
    models = models,
    model_summary = model_summary.out,
    parameters_vector = paramV
  )
  return(output)
}