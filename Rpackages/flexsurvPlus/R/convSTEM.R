# NOT EXPORTED YET - Placeholder
# Convert survival parameters to SAS/STEM parametric forms
#
# @param parameters A vector containing parameters estimated using \code{\link{run_PSM}} or \code{\link{boot_PSM}}
#
# @details This function primarily exists for backward compatibility with older excel models where parametric extrapolation was
# performed with SAS and alternative parametric forms were used for distributions. As such only a subset of models are supported.
# Possible distributions include:
# \itemize{
#   \item Exponential ('exp')
#   \item Weibull ('weibull')
#   \item Gompertz ('gompertz')
#   \item Log-normal ('lnorm')
#   \item Log-logistic ('llogis')
#   \item Generalized gamma ('gengamma')
#   \item Gamma ('gamma')
#   }
#
# @return a data frame of converted parameters
#      
#
# @export
convSTEM <- function(parameters_vector){
  
  # check if we have something from bootPSM or runPSM
  
  rc <- par
  ameters_vector 
  
  return(rc)
  
}