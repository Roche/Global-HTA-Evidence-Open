#' Takes input data and a model plan and passes to the model engine specified. 
#' Current supported engines are the `gemtc` package (using mtc.model & mtc.run) or `rjags` (using jags and dic.samples functions)
#'
#' @param model_input  a list containing named elements fitting_data (data which has been pre-processed) and plan (list object containing all input parameters)
#' @return model object of class `rjags` or `mtc.result`
#'
#' @importFrom purrr map2
#' @importFrom R2jags jags
#' @importFrom gemtc mtc.model mtc.run
#' @export
nma_fit <- function(model_input) {
  data <- model_input$fitting_data
  plan <- model_input$plan
  
  switch(plan$engine,
         gemtc = {
           
           set.seed(plan$params$seed)
           
           #Extract the model parameters
           model_params <- plan$params$model_params
           
           # Add network to model input and run model
           model_params$network <- data
           model <- do.call(gemtc::mtc.model,  model_params)
           
           # Add jags seed to each chain
           model$inits <- purrr::map2(model$inits, plan$params$jags_init, ~c(.y, .x))
  
           # Extract the model.run parameters
           run_params <- plan$params$run_params
           # Add the model output to the list and call mtc.run
           run_params$model <- model
           fit <- do.call(gemtc::mtc.run, run_params)
           
           # Collect outputs
           return(fit)
           
         },
         rjags = {

           set.seed(plan$params$seed)

           # Add data to model parameters and run jags
           model_params <- plan$params$model_params
           model_params$data <- data
           
           fit <- do.call(R2jags::jags, model_params)

           # Extract dic.samples
           deviance <- plan$params$deviance_params
           deviance$model <- fit$model
           DICsamp <- do.call(rjags::dic.samples, deviance)
  
           # Manually adjust fit. TODO - create function ?
           fit$BUGSoutput$DIC <- sum(DICsamp$deviance) + sum(DICsamp$penalty)
           fit$BUGSoutput$pD <- sum(DICsamp$penalty)
           fit$BUGSoutput$DICbyR   <- FALSE
           fit$r.seed <- plan$params$seed
           
           # Add data and model
           fit$data.jg <- data
           fit$model.type <- plan$model
           
           #Add model.pars eg exponents
           fit$model.pars <- plan$params$jags_init_params$model.pars
           fit <- c(fit, plan$params$fit_params)
           class(fit) <- plan$engine #re-assign jags class
           
           return(fit)
         }#,
         # stan ={},
         # glm = {},
         # fitsurv = {}
  )

}
