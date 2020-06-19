#' Lists all available model files inside the inst directory 
#'
#' @param full.path lgl Return full path to file?
#' @return character vector containing a file name/path
#' @export
list_BUGS <- function(full.path = FALSE){
  list.files(path = file.path("inst", "BUGScode"), full.names = full.path)
}


#' Helper function to extract BUGS files for given input parameters
#' 
#' @param data.type Only GSD currently supported 
#' @param bth.model FE or RE
#' @param bth.prior list containins priors infomation
#' @param model.pars Only needed for FP model, list containing exponets to determin order (1st of 2nd)
#' @param model.type Either PWE or FP 
#' @importFrom stringr str_replace
extract_BUGS_file <- function(data.type, bth.model, bth.prior, model.pars= NULL, model.type = c("PWE", "FP")) {
  
  model.type <- match.arg(model.type)
  priors <- paste(bth.model, bth.prior$type, bth.prior$distr, sep = "-")
  
  if(model.type == "PWE"){
    file_name <- paste(paste(data.type, "piece-wise-cst", sep = "_"), priors, sep = "_")
  } else{
    order <- switch(length(model.pars$exponents), `1` = "1o",  `2` = "2o")
    file_name <- paste(paste0(data.type, "_", "fracpoly", "-", order), priors, sep = "_")
  }
  
  model.file <- stringr::str_remove(tolower(file_name), pattern = "--")  %>% # No priors for fixed effects
    paste0(., ".txt") %>% 
    system.file("BUGScode", ., package = "gemtcPlus")
  
  
  if(model.file == "")
    return(NULL)
  
  return(model.file)
}


#' create_jags_init. Helper function to create jags init list dependant on length on chains provided 
#' @param n.chains  Number of chains' 
#' @importFrom purrr map
create_jags_init <- function(n.chains = 3){
  
  inital_seeds <- c(94387, 24507, 39483)
  seeds <- inital_seeds[seq_len(n.chains)]
  
  # For longer chains  times previous value by 3 (random number)
  if (any(is.na(seeds))) {
    missing <- which(is.na(seeds))
    for (i in missing) {
      seeds[[i]] <- seeds[i - 1] * 3
    }
  }
  purrr::map(seeds,  ~list(".RNG.name" = "base::Wichmann-Hill", ".RNG.seed" = .x))
}


#' Helper function to extract named elements from a list to match the arguments of supplied function 
#'
#' @param args  A named \code{list} containing arguments to match to `func`
#' @param func A function of whose argumetns to extract
#' 
#' @importFrom stringr str_replace
#' @export
match_args_to_func <- function(args, func){
  
  function_args <- names(as.list(args(func)))
  
  if(deparse(substitute(func)) %in% c("dic.samples", "rjags::dic.samples"))
    function_args <- c(function_args, "n.thin")
  

  if(deparse(substitute(func)) %in% c("gemtc::mtc.model", "mtc.model"))
    names(args) <- stringr::str_replace(names(args), "bth.prior", "hy.prior")
  
  out <- args[names(args) %in% function_args]
  return(out)
}
