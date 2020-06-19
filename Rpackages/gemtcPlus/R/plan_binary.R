#' Creates a model plan for binary data
#'
#' @param bth.model One of "FE",  or "RE"
#' @param engine Only `gemtc` currently supported 
#' @param data.type Only "BINARY" supported
#' @param binary_data_type One of "relative_effect" or "arm_level"
#' @param jags_init A \code{list} containing the jags seeds to set.
#' @param n.chain A \code{numeric} value representing n.chains
#' @param n.iter A \code{numeric} value representing n.iter
#' @param n.adapt A \code{numeric} value representing n.adpat/ n.burnin
#' @param thin A \code{numeric} value representing thin
#' @param bth.prior Output of using \code{mtc.hy.prior}. Only required if bth.model == "RE"
#' @param rsd \code{numeric} seed to be set
#' @param model defult "binom"
#' @param link default "logit"
#' @param om.scale defult is 5
#' @param ... any other names arguments must match arguments names from \code{mtc.model}
#' 
#' @return A list containing model, engine, binary_data_type and model params
#' @examples
#'plan_binary(bth.model = "RE", 
#'            n.chain = 3, 
#'            n.iter = 6000, 
#'            thin = 1,
#'            n.adapt = 1000, 
#'            link = "logit",
#'            bth.prior =  gemtc::mtc.hy.prior(type = "var", distr = "dlnorm",-4.18, 1 / 1.41 ^ 2))
#' @importFrom gemtc mtc.model mtc.run
#' @export

plan_binary <- function(
                    bth.model = c("FE", "RE"),
                    engine = "gemtc", 
                    data.type = "BINARY", 
                    binary_data_type = "arm_level",
                    jags_init = NULL,
        
                    n.chain = NULL,  #mtc.model
                    n.iter = NULL,
                    n.adapt = NULL,    ## n.Burnin
                  
                    thin = NULL,
                    bth.prior = NULL,
                    rsd = 13579,
                   model = "binom",  
                   link = "logit",
                   om.scale = 5,
                   
                   ...
){
  # Basic checks
  if(engine != "gemtc") stop("gemtc is currently the only supported engine")
  if(data.type != "BINARY") stop("Binary is currently the only supported data type")
  bth.model <- match.arg(bth.model)
  if(bth.model == "RE" & is.null(bth.prior)) stop("For selected model bth.prior must not be NULL")
  if(bth.model == "FE" & !is.null(bth.prior)) warning("For Fixed Effect bth.prior will be ignored.")
  
  if(!is.null(jags_init)){
    stopifnot(is.list(jags_init))
    stopifnot(length(jags_init) == n.chain)
  }
  
  if(is.null(jags_init))
    jags_init <- create_jags_init(n.chains = n.chain)
  

  binary_data_type <- switch(binary_data_type,
                         arm_level = "data.ab",   # Binary (could be re) 
                         relative_effect = "data.re") # CONT/HR

  # Get all arguments as a list
  all_args <- mget(names(formals()),sys.frame(sys.nframe()))
  all_args$... <- NULL
  all_args <- c(all_args, list(...))
  
  # Add linearModel to args if FE and missing
  if(!("linearModel" %in% names(all_args)) & bth.model == "FE"){
    all_args["linearModel"] <- "fixed"
  }

  # Extract into separate lists using args from their destination functions
  
  model_params <- match_args_to_func(all_args, gemtc::mtc.model) # match args we want to pass to mtc.model that are not named?

  run_params <- match_args_to_func(all_args, gemtc::mtc.run)

  
  out <- list(engine = engine,
              analysis = data.type,
              binary_data_type = binary_data_type,
              params = list(seed = rsd, 
                            model_params = model_params, 
                            run_params = run_params,
                            jags_init = jags_init ))
  return(out)
}
