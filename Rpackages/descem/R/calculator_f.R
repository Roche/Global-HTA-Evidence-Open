# Draw TTE -------------------------------------------------------------------------

#' Draw a time to event from a list of parametric survival functions
#'
#' @param n_chosen The number of observations to be drawn
#' @param dist The distribution; takes values 'lnorm','weibullPH','weibull','llogis','gompertz','gengamma','gamma','exp'
#' @param coef1 First coefficient of the distribution, defined as in the coef() output on a flexsurvreg object
#' @param coef2 Second coefficient of the distribution, defined as in the coef() output on a flexsurvreg object
#' @param coef3 Third coefficient of the distribution, defined as in the coef() output on a flexsurvreg object
#' @param hr A hazard ratio
#' @param seed An integer which will be used to set the seed for this draw. If no seed is set a random integer is used.
#'
#' @return A vector of time to event estimates from the given parameters
#'
#' @importFrom stats rlnorm rweibull rgamma rexp
#'
#' @export
#'
#' @examples
#' draw_tte(n_chosen=1,dist='exp',coef1=1,hr=1)

draw_tte <- function(n_chosen=1,dist='exp',coef1=1,coef2=NULL,coef3=NULL,hr=1,seed=NULL) {

  if(!is.numeric(seed)){
    seed <- sample(1:100000,1)
  }
  set.seed(seed)


  if (any(length(coef1)>1,length(coef2)>1,length(coef3)>1)) {
    warning("Provided a coefficient parameter that is a vector")
  }

  # if (dist %in% c('lnorm','llogis','gamma','gengamma','weibull') & hr!=1) {
  #   warning("Distribution selected is not PH, interpretation is an AFT.")
  # }
  #
  if (dist=="lnorm") {
    draw.out <- rlnorm(n_chosen,meanlog=coef1 - log(hr), sdlog=exp(coef2)) #AFT

  } else if (dist=="weibullPH") {
    draw.out <- flexsurv::rweibullPH(n_chosen, shape = exp(coef1), scale = exp(coef2 + log(hr) )) #PH

  } else if (dist=="weibull") {
    draw.out <- rweibull(n_chosen, shape = exp(coef1), scale = exp(coef2 - log(hr))) #AFT Weibull

  } else if (dist=="llogis") {
    draw.out <- flexsurv::rllogis(n_chosen, shape = exp(coef1), scale = exp(coef2 - log(hr))) # AFT

  } else if (dist=="gompertz") {
    draw.out <- flexsurv::rgompertz(n_chosen, shape = coef1, rate = exp(coef2+ log(hr))) #PH

  } else if (dist=="gengamma") {
    draw.out <- flexsurv::rgengamma(n_chosen, mu = coef1 - log(hr), sigma = exp(coef2), Q = coef3) #AFT

  } else if (dist=="gamma") {
    draw.out <- rgamma(n_chosen, shape = exp(coef1), rate = exp(coef2 + log(hr)))  #AFT

  } else if (dist=="exp") {
    draw.out <- rexp(n_chosen, rate = exp(coef1 + log(hr))) #PH

  } else {
    stop("Invalid distribution")
  }

  return(draw.out)
}

# Draws beta distribution - accepts vectors--------

#' Draw from a beta distribution based on mean and se
#'
#' @param value A vector of the mean values
#' @param se A vector of the standard errors of the means
#' @param seed An integer which will be used to set the seed for this draw. If no seed is set a random integer is used.
#'
#' @return A single estimate from the beta distribution based on given parameters
#'
#' @importFrom stats rbeta
#'
#' @export
#'
#' @examples
#' draw_beta(value=0.8,se=0.2)

draw_beta <- function(value,se,seed=NULL) {
  out <- NULL

  if(!is.numeric(seed)){
    seed <- sample(1:100000,1)
  }
  set.seed(seed)

  for (i in 1:length(value)) {
    value_ch <- value[i]
    se_ch <-  se[i]
    alpha <- ((1 - value_ch) / (se_ch^2) - (1 / value_ch)) * value_ch ^ 2
    beta <- alpha * ((1 / value_ch) - 1)
    temp <- rbeta(1,alpha,beta)
    out <- c(out,temp)
  }

  return(out)
}

# Draws gamma distribution - accepts vectors--------

#' Draw from a gamma distribution based on mean and se
#'
#' @param value A vector of the mean values
#' @param se A vector of the standard errors of the means
#' @param seed An integer which will be used to set the seed for this draw. If no seed is set a random integer is used.
#'
#' @return A single estimate from the gamma distribution based on given parameters
#'
#' @importFrom stats rgamma
#'
#' @export
#'
#' @examples
#' draw_gamma(value=0.8,se=0.2)
#'

draw_gamma <- function(value,se,seed=NULL) {
  out <- NULL

  if(!is.numeric(seed)){
    seed <- sample(1:100000,1)
  }
  set.seed(seed)

  for (i in 1:length(value)) {
    value_ch <- value[i]
    se_ch <-  se[i]
    scale <- se_ch^2 / value_ch
    shape <- value_ch / scale
    temp <- ifelse(se_ch==0,value_ch,rgamma(1,shape,scale=scale))
    out <- c(out,temp)
  }

  return(out)
}


#' Draw from a restricted Gompertz distribution
#'
#' @param n The number of observations to be drawn
#' @param shape The shape parameter of the Gompertz distribution, defined as in the coef() output on a flexsurvreg object
#' @param rate The rate parameter of the Gompertz distribution, defined as in the coef() output on a flexsurvreg object
#' @param lower_bound The lower bound of the restricted distribution
#' @param upper_bound The upper bound of the restricted distribution
#' @param seed An integer which will be used to set the seed for this draw. If no seed is set a random integer is used.
#'
#' @return Estimate(s) from the restricted Gompertz distribution based on given parameters
#'
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' draw_resgompertz(1,shape=0.05,rate=0.01,lower_bound = 50)

draw_resgompertz <- function(n, shape, rate , lower_bound = 0, upper_bound = Inf, seed=NULL){

  if(!is.numeric(seed)){
    seed <- sample(1:100000,1)
  }
  set.seed(seed)

  quantiles <- flexsurv::pgompertz(c(lower_bound, upper_bound),shape, rate)
  uniform_random_numbers <- stats::runif(n, quantiles[1], quantiles[2])
  flexsurv::qgompertz(uniform_random_numbers, shape, rate ) - lower_bound
}
