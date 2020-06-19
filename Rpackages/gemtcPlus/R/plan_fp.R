#' Creates a fractional polynomial model plan 
#'
#' @param model.pars  A named \code{list} containing `exponents` & `t.val` when using GSD
#' @param bth.model One of "FE", "REINT",  or "RE"
#' @param ref.std A string containing the reference study
#' @param nma.ref.trt A string containing the reference treatment
#' @param engine Only `rjags` currently supported 
#' @param data.type Only "GSD" supported
#' @param feprior_mean A \code{numeric} value representing feprior_mean (default 0)
#' @param feprior_prec A \code{numeric} value representing feprior_prec default 0.0001
#' @param descr A \code{character} string to describe the model
#' @param descr_s Short description
#' @param n.chains A \code{numeric} value representing n.chains
#' @param n.iter A \code{numeric} value representing n.iter
#' @param n.burnin A \code{numeric} value representing n.burnin
#' @param n.thin A \code{numeric} value representing n.thin
#' @param bth.prior A \code{list} containing type & distr. Only required if bth.model == "FE"
#' @param rsd \code{numeric} seed to be set
#' @param model.file Path to BUGS file. If NULL file name will be built from input parameters and extracted from BUGScode folder if present
#' @param ... any other named arguments must match arguments names from \code{dic.samples} or \code{jags} functions
#'
#' @return A list containing model, engine, analysis and model params
#' @examples
#' plan_fp(model.pars = list(exponents = 0, t.eval = "midpoint"),
#'         bth.model = "FE", ref.std = "STUDY2", nma.ref.trt = "B",
#'         model.file = system.file("BUGScode", "gsd_fracpoly-1o_fe.txt", package = "gemtcPlus"))
#' @importFrom stringr str_remove
#' @importFrom purrr %||%
#' @importFrom R2jags jags
#' @export

plan_fp <- function(model.pars,
                    bth.model = c("FE", "REINT", "RE"),
                    ref.std, 
                    nma.ref.trt,
                    engine = "rjags", 
                    data.type = "GSD", 
                    feprior_mean = 0,
                    feprior_prec = 0.0001,
                    descr = "Fractional polynomial model",
                    descr_s = "FP",
                    n.chains = NULL,
                    n.iter = NULL,
                    n.burnin = NULL,
                    n.thin = NULL,
                    bth.prior = NULL,
                    rsd = 13579,
                    model.file = NULL,
                    ...
                    ){
  # Basic checks
  if(engine != "rjags") stop("rjags is currently the only supported engine")
  if(data.type != "GSD") stop("GSD is currently the only supported data type")
  if(data.type == "GSD" & !any(names(model.pars) %in% c("exponents", "t.eval"))) stop("model.pars requires a list with exponents and t.eval")
  bth.model <- match.arg(bth.model)
  if(bth.model != "FE" & is.null(bth.prior)) stop("For selected model bth.prior must not be NULL")
  if(bth.model == "FE" & !is.null(bth.prior)) warning("For Fixed Effect bth.prior will be ignored.")
  
  parameters <- if(engine == "rjags") 
    switch(bth.model,
           "FE" = c("d", "mu"),
           "REINT" = c("d", "mu", "sd"),
           "RE" = c("d", "mu", "V"))
   

  # Extract BUGS file from parameters given
  model.file <- model.file %||%
    extract_BUGS_file(data.type, bth.model, bth.prior, model.pars, model.type = "FP") %||%
    stop("No bugs script found for model parameters selected")
  
  # Get all arguments as a list
  all_args <- mget(names(formals()),sys.frame(sys.nframe()))
  all_args$... <- NULL
  all_args <- c(all_args, list(...))
  
  # Extract into separate lists using args from their destination functions
  jags_init_params <- match_args_to_func(all_args, groupedTTE_fp_pre_proc)
  
  model_params <- match_args_to_func(all_args, R2jags::jags)
  model_params$parameters <- parameters
  model_params$model.file <- model.file
  
  deviance_params <- match_args_to_func(all_args, rjags::dic.samples)
  deviance_params$type <- "pD"
  
  fit_params <- list(RE = ifelse(bth.model == "RE", TRUE, FALSE), 
                     descr_s = descr_s, 
                     descr = descr)

  out <- list(model = "FP",
            engine = engine,
            analysis = data.type,
            params = list(seed = rsd, 
                          model_params = model_params, 
                          jags_init_params = jags_init_params, 
                          deviance_params = deviance_params, 
                          fit_params = fit_params))
  return(out)
}
