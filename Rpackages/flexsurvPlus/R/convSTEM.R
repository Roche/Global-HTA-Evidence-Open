#' Convert survival parameters to SAS/STEM parametric forms
#'
#' @param x An object created by calling \code{\link{runPSM}}
#' @param samples An object created by calling \code{\link{boot}} with \code{\link{bootPSM}}
#'
#' This function primarily exists for backward compatibility with older excel models where parametric extrapolation was
#' performed with SAS and alternative parametric forms were used for distributions. As such only a subset of models are supported.
#' One or both of \code{x} and \code{samples} must be specified and affect what is returned. For more details please see the 
#' vignette \code{vignette("STEM_compatability", package = "flexsurvPlus")}
#' 
#' Possible distributions include
#' 
#' \itemize{
#'   \item Exponential ('exp')
#'   \item Weibull ('weibull')
#'   \item Gompertz ('gompertz')
#'   \item Log-normal ('lnorm')
#'   \item Log-logistic ('llogis')
#'   \item Generalized gamma ('gengamma')
#'   \item Gamma ('gamma')
#'   }
#'
#' @return a list containing 4 data frames
#' \itemize{
#'   \item stem_param Converted parameter estimates
#'   \item stem_cov Converted covariance matrix (if \code{samples} provided)
#'   \item stem_modsum Converted model summary (if \code{x} provided)
#'   \item stem_boot Converted bootstrap samples (if \code{samples} provided)
#'   }
#' @export
convSTEM <- function(x = NULL, samples = NULL){
  
  #check that at least one object is provided
  assertthat::assert_that(
    !is.null(x) | !is.null(samples),
    msg = "One of x or samples must be provided"
  )
  
  # check x is an object from runPSM
  assertthat::assert_that(
    is.null(x) | any(names(x) == c("parameters_vector")),
    msg = "x must be created using runPSM"
  )
  
  # check samples is a bootstrap object
  assertthat::assert_that(
    is.null(samples) | class(samples) == "boot",
    msg = "samples must be a boot object"
  )
  
  if (!is.null(samples)){
    assertthat::assert_that(
      samples$call$statistic == "bootPSM", 
      msg = "This function only works with bootstrap samples created using boot with statistic = bootPSM"
    )
  }
  
  # check if both x and samples provided and are from same data/models
  if (!is.null(samples) & !is.null(x)) {
    assertthat::assert_that(
      all(x$parameters_vector == samples$t0, na.rm = TRUE),
      msg = "x and samples provided do not match. Please confirm these are from the same models"
    )
  }
  
  ####################################################################################
  # conversion of the parameters to a data frame
  ####################################################################################
  
  # convert estimates to a data frame
  # either from x or from samples t0
  if (!is.null(x)){
    ests <- cbind(Estimate = "Main", bootid = 0, as.data.frame(t(x$parameters_vector)))
  } else {
    ests <- cbind(Estimate = "Main", bootid = 0, as.data.frame(t(samples$t0)))
  }
  
  # if samples are provided then include these into the estimates dataframe
  if (!is.null(samples)) {
    tsamp <- samples$t
    colnames(tsamp) <- names(samples$t0)
    covests <- as_tibble(tsamp)
    covests$Estimate <- paste0("Bootstrap")
    covests$bootid <- 1:nrow(covests)
    ests <- bind_rows(ests, covests)
  }
  
  # do the conversions
  # this is not pretty code...but hopefully transformations are transparent
  
  # set up a return objects for each parameter to convert
  rc_struct <- ests %>%
    transmute(Estimate, bootid, Model = "", Dist = "", Param = "", Value =NaN)
  
  # empty dataframe to append converted parameters too
  rc <- rc_struct %>%
    filter(1 == 2)
  
  #############################
  #### exponential models
  #############################
  
  # common shape
  if (any(grepl("comshp.exp.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Exponential",
             Param = "INTERCEPT", Value = -log(ests$comshp.exp.rate.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Exponential",
             Param = "TX(Intervention)", Value = -(log(ests$comshp.exp.rate.int) - log(ests$comshp.exp.rate.ref))
      ) %>%
      bind_rows(rc)
  }
  
  # separate shape
  if (any(grepl("sep.exp.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Exponential",
             Param = "INTERCEPT", Value = -log(ests$sep.exp.rate.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Exponential",
             Param = "INTERCEPT", Value = -log(ests$sep.exp.rate.int)
      ) %>%
      bind_rows(rc)
  }
  
  # independent shape (not really supported by STEM so handled as if separate models)
  if (any(grepl("indshp.exp.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Exponential",
             Param = "INTERCEPT", Value = -log(ests$indshp.exp.rate.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Exponential",
             Param = "INTERCEPT", Value = -log(ests$indshp.exp.rate.int)
      ) %>%
      bind_rows(rc)
  }
  
  # one arm models
  if (any(grepl("onearm.exp.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Exponential",
             Param = "INTERCEPT", Value = -log(ests$onearm.exp.rate.int)
      ) %>%
      bind_rows(rc)
    
  }
  
  #############################
  #### weibull models
  #############################
  
  # common shape
  if (any(grepl("comshp.weibull.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Weibull",
             Param = "INTERCEPT",
             Value = log(ests$comshp.weibull.scale.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Weibull",
             Param = "TX(Intervention)",
             Value = log(ests$comshp.weibull.scale.int) - log(ests$comshp.weibull.scale.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Weibull",
             Param = "SCALE",
             Value = 1/ests$comshp.weibull.shape.ref
      ) %>%
      bind_rows(rc)
  }
  
  # separate
  if (any(grepl("sep.weibull.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Weibull",
             Param = "INTERCEPT",
             Value = log(ests$sep.weibull.scale.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Weibull",
             Param = "SCALE",
             Value = 1/ests$sep.weibull.shape.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Weibull",
             Param = "INTERCEPT",
             Value = log(ests$sep.weibull.scale.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Weibull",
             Param = "SCALE",
             Value = 1/ests$sep.weibull.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  # independent shape (not really supported by STEM so handled as if separate models)
  if (any(grepl("indshp.weibull.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Weibull",
             Param = "INTERCEPT",
             Value = log(ests$indshp.weibull.scale.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Weibull",
             Param = "SCALE",
             Value = 1/ests$indshp.weibull.shape.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Weibull",
             Param = "INTERCEPT",
             Value = log(ests$indshp.weibull.scale.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Weibull",
             Param = "SCALE",
             Value = 1/ests$indshp.weibull.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  # one arm
  if (any(grepl("onearm.weibull.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Weibull",
             Param = "INTERCEPT",
             Value = log(ests$onearm.weibull.scale.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Weibull",
             Param = "SCALE",
             Value = 1/ests$onearm.weibull.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  #############################
  #### lognormal models
  #############################
  
  # common shape
  if (any(grepl("comshp.lnorm.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Log Normal",
             Param = "INTERCEPT",
             Value = ests$comshp.lnorm.meanlog.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Log Normal",
             Param = "TX(Intervention)",
             Value = ests$comshp.lnorm.meanlog.int - ests$comshp.lnorm.meanlog.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Log Normal",
             Param = "SCALE",
             Value = ests$comshp.lnorm.sdlog.ref
      ) %>%
      bind_rows(rc)
  }
  
  # separate
  if (any(grepl("sep.lnorm.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Log Normal",
             Param = "INTERCEPT",
             Value = ests$sep.lnorm.meanlog.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Log Normal",
             Param = "SCALE",
             Value = ests$sep.lnorm.sdlog.ref
      ) %>%
      bind_rows(rc)
    
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Log Normal",
             Param = "INTERCEPT",
             Value = ests$sep.lnorm.meanlog.int
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Log Normal",
             Param = "SCALE",
             Value = ests$sep.lnorm.sdlog.int
      ) %>%
      bind_rows(rc)
  }
  
  # independent shape (not really supported by STEM so handled as if separate models)
  if (any(grepl("indshp.lnorm.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Log Normal",
             Param = "INTERCEPT",
             Value = ests$indshp.lnorm.meanlog.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Log Normal",
             Param = "SCALE",
             Value = ests$indshp.lnorm.sdlog.ref
      ) %>%
      bind_rows(rc)
    
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Log Normal",
             Param = "INTERCEPT",
             Value = ests$indshp.lnorm.meanlog.int
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Log Normal",
             Param = "SCALE",
             Value = ests$indshp.lnorm.sdlog.int
      ) %>%
      bind_rows(rc)
  }
  
  # One arm
  if (any(grepl("onearm.lnorm.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Log Normal",
             Param = "INTERCEPT",
             Value = ests$onearm.lnorm.meanlog.int
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Log Normal",
             Param = "SCALE",
             Value = ests$onearm.lnorm.sdlog.int
      ) %>%
      bind_rows(rc)
  }
  
  
  #############################
  #### loglogistic models
  #############################
  
  # common shape
  if (any(grepl("comshp.llogis.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Log Logistic",
             Param = "INTERCEPT",
             Value = log(ests$comshp.llogis.scale.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Log Logistic",
             Param = "TX(Intervention)",
             Value = log(ests$comshp.llogis.scale.int) - log(ests$comshp.llogis.scale.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Log Logistic",
             Param = "SCALE",
             Value = 1/ests$comshp.llogis.shape.ref
      ) %>%
      bind_rows(rc)
  }
  
  # separate
  if (any(grepl("sep.llogis.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Log Logistic",
             Param = "INTERCEPT",
             Value = log(ests$sep.llogis.scale.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Log Logistic",
             Param = "SCALE",
             Value = 1/ests$sep.llogis.shape.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Log Logistic",
             Param = "INTERCEPT",
             Value = log(ests$sep.llogis.scale.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Log Logistic",
             Param = "SCALE",
             Value = 1/ests$sep.llogis.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  # independent shape (not really supported by STEM so handled as if separate models)
  if (any(grepl("indshp.llogis.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Log Logistic",
             Param = "INTERCEPT",
             Value = log(ests$indshp.llogis.scale.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Log Logistic",
             Param = "SCALE",
             Value = 1/ests$indshp.llogis.shape.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Log Logistic",
             Param = "INTERCEPT",
             Value = log(ests$indshp.llogis.scale.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Log Logistic",
             Param = "SCALE",
             Value = 1/ests$indshp.llogis.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  # one arm
  if (any(grepl("onearm.llogis.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Log Logistic",
             Param = "INTERCEPT",
             Value = log(ests$onearm.llogis.scale.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Log Logistic",
             Param = "SCALE",
             Value = 1/ests$onearm.llogis.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  #############################
  #### gengamma models
  #############################
  
  # common shape
  if (any(grepl("comshp.gengamma.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Generalized Gamma",
             Param = "INTERCEPT",
             Value = ests$comshp.gengamma.mu.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Generalized Gamma",
             Param = "TX(Intervention)",
             Value = ests$comshp.gengamma.mu.int - ests$comshp.gengamma.mu.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Generalized Gamma",
             Param = "SCALE",
             Value = ests$comshp.gengamma.sigma.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Generalized Gamma",
             Param = "SHAPE",
             Value = ests$comshp.gengamma.Q.ref
      ) %>%
      bind_rows(rc)
  }
  
  # Separate
  if (any(grepl("sep.gengamma.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Generalized Gamma",
             Param = "INTERCEPT",
             Value = ests$sep.gengamma.mu.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Generalized Gamma",
             Param = "SCALE",
             Value = ests$sep.gengamma.sigma.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Generalized Gamma",
             Param = "SHAPE",
             Value = ests$sep.gengamma.Q.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Generalized Gamma",
             Param = "INTERCEPT",
             Value = ests$sep.gengamma.mu.int
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Generalized Gamma",
             Param = "SCALE",
             Value = ests$sep.gengamma.sigma.int
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Generalized Gamma",
             Param = "SHAPE",
             Value = ests$sep.gengamma.Q.int
      ) %>%
      bind_rows(rc)
  }
  
  # independent shape (not really supported by STEM so handled as if separate models)
  if (any(grepl("indshp.gengamma.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Generalized Gamma",
             Param = "INTERCEPT",
             Value = ests$indshp.gengamma.mu.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Generalized Gamma",
             Param = "SCALE",
             Value = ests$indshp.gengamma.sigma.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Generalized Gamma",
             Param = "SHAPE",
             Value = ests$indshp.gengamma.Q.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Generalized Gamma",
             Param = "INTERCEPT",
             Value = ests$indshp.gengamma.mu.int
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Generalized Gamma",
             Param = "SCALE",
             Value = ests$indshp.gengamma.sigma.int
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Generalized Gamma",
             Param = "SHAPE",
             Value = ests$indshp.gengamma.Q.int
      ) %>%
      bind_rows(rc)
  }
  
  # one arm
  if (any(grepl("onearm.gengamma.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Generalized Gamma",
             Param = "INTERCEPT",
             Value = ests$onearm.gengamma.mu.int
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Generalized Gamma",
             Param = "SCALE",
             Value = ests$onearm.gengamma.sigma.int
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Generalized Gamma",
             Param = "SHAPE",
             Value = ests$onearm.gengamma.Q.int
      ) %>%
      bind_rows(rc)
  }
  
  #############################
  #### gompertz models
  #############################
  
  # common shape
  if (any(grepl("comshp.gompertz.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Gompertz",
             Param = "INTERCEPT",
             Value = -log(ests$comshp.gompertz.rate.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Gompertz",
             Param = "TX(Intervention)",
             Value = -(log(ests$comshp.gompertz.rate.int) - log(ests$comshp.gompertz.rate.ref))
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Gompertz",
             Param = "SCALE",
             Value = ests$comshp.gompertz.shape.ref
      ) %>%
      bind_rows(rc)
  }
  
  # separate
  if (any(grepl("sep.gompertz.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Gompertz",
             Param = "INTERCEPT",
             Value = -log(ests$sep.gompertz.rate.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Gompertz",
             Param = "SCALE",
             Value = ests$sep.gompertz.shape.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Gompertz",
             Param = "INTERCEPT",
             Value = -log(ests$sep.gompertz.rate.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Gompertz",
             Param = "SCALE",
             Value = ests$sep.gompertz.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  # independent shape (not really supported by STEM so handled as if separate models)
  if (any(grepl("indshp.gompertz.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Gompertz",
             Param = "INTERCEPT",
             Value = -log(ests$indshp.gompertz.rate.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Gompertz",
             Param = "SCALE",
             Value = ests$indshp.gompertz.shape.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Gompertz",
             Param = "INTERCEPT",
             Value = -log(ests$indshp.gompertz.rate.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Gompertz",
             Param = "SCALE",
             Value = ests$indshp.gompertz.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  # onearm
  if (any(grepl("onearm.gompertz.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Gompertz",
             Param = "INTERCEPT",
             Value = -log(ests$onearm.gompertz.rate.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Gompertz",
             Param = "SCALE",
             Value = ests$onearm.gompertz.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  #############################
  #### gamma models
  #############################
  
  # common shape
  if (any(grepl("comshp.gamma.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Gamma",
             Param = "INTERCEPT",
             Value = -log(ests$comshp.gamma.rate.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Gamma",
             Param = "TX(Intervention)",
             Value = -(log(ests$comshp.gamma.rate.int) - log(ests$comshp.gamma.rate.ref))
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Common Shape", Dist = "Gamma",
             Param = "SCALE",
             Value = ests$comshp.gamma.shape.ref
      ) %>%
      bind_rows(rc)
  }
  
  # separate
  if (any(grepl("sep.gamma.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Gamma",
             Param = "INTERCEPT",
             Value = -log(ests$sep.gamma.rate.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Reference", Dist = "Gamma",
             Param = "SCALE",
             Value = ests$sep.gamma.shape.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Gamma",
             Param = "INTERCEPT",
             Value = -log(ests$sep.gamma.rate.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Separate - Intervention", Dist = "Gamma",
             Param = "SCALE",
             Value = ests$sep.gamma.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  # independent shape (not really supported by STEM so handled as if separate models)
  if (any(grepl("indshp.gamma.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Gamma",
             Param = "INTERCEPT",
             Value = -log(ests$indshp.gamma.rate.ref)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Reference", Dist = "Gamma",
             Param = "SCALE",
             Value = ests$indshp.gamma.shape.ref
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Gamma",
             Param = "INTERCEPT",
             Value = -log(ests$indshp.gamma.rate.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "Independent Shape - Intervention", Dist = "Gamma",
             Param = "SCALE",
             Value = ests$indshp.gamma.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  # onearm
  if (any(grepl("onearm.gamma.", names(ests)))){
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Gamma",
             Param = "INTERCEPT",
             Value = -log(ests$onearm.gamma.rate.int)
      ) %>%
      bind_rows(rc)
    
    rc <- rc_struct %>%
      mutate(Model = "One arm - Intervention", Dist = "Gamma",
             Param = "SCALE",
             Value = ests$onearm.gamma.shape.int
      ) %>%
      bind_rows(rc)
  }
  
  
  
  stemParamDF <- rc
  
  ####################################################################################
  # calculate the covariance matrices from the bootstrap samples if exist
  ####################################################################################
  
  rc.cov.df <- tibble(Model = "", Dist = "", rowParam = "", colParam = "", Value = 1)%>%
    filter(1==2)
  
  if (any(stemParamDF$Estimate == "Bootstrap")){
    
    included_models <- stemParamDF %>%
      filter(Estimate == "Bootstrap") %>%
      transmute(Model, Dist) %>%
      unique()
    
    for (i in 1:nrow(included_models)) {
      # get covariance
      this.model.cov <- stemParamDF %>%
        filter(Model == included_models$Model[i],
               Dist == included_models$Dist[i],
               Estimate == "Bootstrap") %>%
        transmute(bootid, Param, Value) %>%
        reshape(direction = "wide", idvar = "bootid", timevar = "Param", v.names = "Value") %>%
        select(-bootid) %>%
        cov()
      
      # test code for matrix dimensioning
      # this.model.cov <- matrix(data = c(1,2,3,4,5,6), ncol = 2)
      # rownames(this.model.cov) <- c("a","b","c")
      # colnames(this.model.cov) <- c("x","y")
      
      this.cov.df <- tibble(
        Model = included_models$Model[i],
        Dist = included_models$Dist[i],
        rowParam = rep(rownames(this.model.cov), times = dim(this.model.cov)[2]),
        colParam = rep(colnames(this.model.cov), each = dim(this.model.cov)[1]),
        Value = as.numeric(this.model.cov)
      )
      
      rc.cov.df <- rc.cov.df %>%
        bind_rows(this.cov.df)
      
    }
    
    #tidy up the names
    
    rc.cov.df <- rc.cov.df %>%
      mutate(rowParam = gsub("Value.", "", rowParam, fixed=TRUE),
             colParam = gsub("Value.", "", colParam, fixed=TRUE)
      )
    
  }
  
  ####################################################################################
  # calculate a summary of estimates if exist
  ####################################################################################
  
  rc.est.df <- stemParamDF %>%
    filter(Estimate == "Main") %>%
    transmute(Model, Dist, Param, Estimate = Value)
  
  
  ####################################################################################
  # get the model summaries
  # the naming of the models do not match other values
  ####################################################################################
  
  if (!is.null(x)){
    
    rc.modsum <- x$model_summary %>%
      mutate(AIC_SAS = NaN,
             BIC_SAS = NaN)
    
    
    for (i in 1:nrow(rc.modsum)){
      this.mdl <- rc.modsum$Dist[i]
      mdl <- x$models[[this.mdl]]
      
      # convert the AIC & BIC to match SAS formulations
      # difference is due to choice of log(t) vs t as response in likelihood
      # extract the event times from the flexsurv object
      
      sum_log_event_time <- mdl$data$Y[mdl$data$Y[,"status"]==1, "time"] %>%
        log() %>%
        sum()
      
      rc.modsum$AIC_SAS[i] <- AIC(mdl) - 2 * sum_log_event_time
      rc.modsum$BIC_SAS[i] <- BIC(mdl) - 2 * sum_log_event_time
    }
    
    # tidy up the naming to match
    
    oDist <- rc.modsum$Dist
    oModel <- rc.modsum$Model
    
    for (i in 1:nrow(rc.modsum)){
      this.oDist <- oDist[i]
      this.oModel <- oModel[i]
      if(this.oModel %in% c("Common shape", "Independent shape", "One arm")){
        this.Model <- ifelse(this.oModel == "Common shape", "Common Shape",
                             ifelse(this.oModel == "Independent shape", "Independent Shape",
                                    "One arm - Intervention"))
      }
      if(this.oModel == "Seperate"){
        this.Model = ifelse(grepl(".ref", this.oDist, fixed = TRUE), "Separate - Reference", "Separate - Intervention")
      }
      
      this.Dist <- ifelse(grepl(".gamma", this.oDist, fixed = TRUE), "Gamma", NA)
      this.Dist <- ifelse(grepl(".gengamma", this.oDist, fixed = TRUE), "Generalized Gamma", this.Dist)
      this.Dist <- ifelse(grepl(".gompertz", this.oDist, fixed = TRUE), "Gompertz", this.Dist)
      this.Dist <- ifelse(grepl(".llogis", this.oDist, fixed = TRUE), "Log Logistic", this.Dist)
      this.Dist <- ifelse(grepl(".lnorm", this.oDist, fixed = TRUE), "Log Normal", this.Dist)
      this.Dist <- ifelse(grepl(".exp", this.oDist, fixed = TRUE), "Exponential", this.Dist)
      this.Dist <- ifelse(grepl(".weibull", this.oDist, fixed = TRUE), "Weibull", this.Dist)
      
      rc.modsum$Model[i] <- this.Model
      rc.modsum$Dist[i] <- this.Dist
      
    }
    
  } else {
    rc.modsum <- tibble(Model = "", Dist = "", Status = "", AIC = NaN, AIC_SAS = NaN, BIC = NaN, BIC_SAS = NaN) %>%
      filter(1==2)
  }
  
  ####################################################################################
  # Add factors for easy sort
  ####################################################################################
  
  Model.levels <- c("Common Shape", "Separate - Reference", "Separate - Intervention",
                    "Independent Shape - Reference", "Independent Shape - Intervention",
                    "Independent Shape",
                    "One arm - Intervention")
  
  Dist.levels <- c("Exponential", "Weibull", "Log Normal", "Generalized Gamma", "Log Logistic", "Gompertz")
  
  Param.levels <-  c("INTERCEPT", "TX(Intervention)", "SCALE", "SHAPE")
  
  stem_param <- rc.est.df %>%
    transmute(Model, ModelF = factor(Model, levels = Model.levels, ordered = TRUE),
              Dist, DistF = factor(Dist, levels = Dist.levels, ordered = TRUE),
              Param, ParamF = factor(Param, levels = Param.levels, ordered = TRUE),
              Estimate
    ) %>%
    arrange(ModelF, DistF, ParamF)
  
  stem_cov <- rc.cov.df %>%
    transmute(Model, ModelF = factor(Model, levels = Model.levels, ordered = TRUE),
              Dist, DistF = factor(Dist, levels = Dist.levels, ordered = TRUE),
              rowParam, colParam,
              rowParamF = factor(rowParam, levels = Param.levels, ordered = TRUE),
              colParamF = factor(colParam, levels = Param.levels, ordered = TRUE),
              rowNum = as.numeric(rowParamF),
              colNum = as.numeric(colParamF),
              CovEst = Value
    ) %>%
    arrange(ModelF, DistF, rowParamF, colParamF)
  
  stem_modsum <- rc.modsum %>%
    transmute(Model, ModelF = factor(Model, levels = Model.levels, ordered = TRUE),
              Dist, DistF = factor(Dist, levels = Dist.levels, ordered = TRUE),
              Status,
              AIC, AIC_SAS,
              BIC, BIC_SAS) %>%
    filter(!is.na(DistF))
  
  
  stem_boot <- stemParamDF %>%
    filter(Estimate == "Bootstrap") %>%
    transmute(Model, ModelF = factor(Model, levels = Model.levels, ordered = TRUE),
              Dist, DistF = factor(Dist, levels = Dist.levels, ordered = TRUE),
              BootSample = bootid,
              Param, ParamF = factor(Param, levels = Param.levels, ordered = TRUE),
              Estimate = Value) %>%
    arrange(ModelF, DistF, BootSample, ParamF)
  
  ####################################################################################
  # return the data
  ####################################################################################
  
  stem_return <- list(stem_param = stem_param,
                      stem_cov = stem_cov,
                      stem_modsum = stem_modsum,
                      stem_boot = stem_boot)
  
  
  return(stem_return)
}