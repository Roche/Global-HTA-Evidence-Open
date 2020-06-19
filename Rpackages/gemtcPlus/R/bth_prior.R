#' Creates a fractional polynomial model plan 
#'
#' @param model One of "PWE" or "FP"
#' @param bth.model If model is PWE then must be "RE". If model is FP then One of "RE" or "REINT"
#' @param type If PWE RE or FP RE then one of "sd" or "var". If FP REINT then "vcov"
#' @param distr If type is "sd" then one of "unif" or "hn". If type is "var" then "ln". If typeis "vocov" then "invwish"
#' @param param A \code{list} with ... if distibution is "unif" then named value max (numeric > 0). If distribution is hn or ln then list must contain mean and precision.
#'              Of distribution in invwish then names value "scale" (a matrix).
#' 
#' @export
#' 

bth_prior <- function(model = "PWE", bth.model = "RE", type, distr, param) {
  stopifnot(is.character(type))
  stopifnot(length(type) == 1)
  stopifnot(length(distr) == 1)
  stopifnot(is.list(param))
  
  param_names <- sort(paste(names(param), collapse = "-"))
  type_dist <- paste(type, distr, sep = "-")
  type_dist_param <- paste(type_dist, param_names, sep = "-")
  
  combinations <- c("sd-unif-max", "sd-hn-mean-prec", "var-ln-mean-prec", "vcov-invwish-scale")
  if(!type_dist_param %in% combinations) stop("Incorrect combination of type, distribution and parameters. Check documentation")

  if((model == "PWE" & bth.model == "RE") | (model == "FP" & bth.model == "REINT")){
    stopifnot(type %in% c("sd", "var"))
    obj <- list(type = type, distr = distr, args = param)
    
  } else if(model == "FP" & bth.model == "RE"){
    stopifnot(type %in% c("vcov"))
    obj <- list(type = type, distr = distr, args = param)
    
  } else{
    stop("Model type not supported")
  }

  obj
}



