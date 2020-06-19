#' First order fractional polynomial
#'
#' @param x A vector with the dependent variable.
#' @param params A matrix with two columns giving the intercept and the "slope". If more than one rows given, the fractional polynomial is evaluated for each row.
#' @param exponent A numerical value giving the exponent (p1) of the polynomial. 
#' @param sums A function. If non-null, sums of the fractional polynomial at each x value (i.e. summaries over the different params values) are calculated.
#' @return A matrix with length(x) rows giving the FP values or FP summaries.
#' @export
#' 
get_fp_1o <- function(x, params, exponents, sums=NULL){
  
  if (exponents == 0){
    xt <- log(x)
  } else {
    xt <- x^exponents
  }
  
  X <- cbind(1, xt)
  
  if (is.vector(params)) {
    params <- matrix(params, ncol = 1)
  }
  if (dim(X)[2] != dim(params)[1]){
    params <- t(params)
  }
  
  out <- X %*% params
  if (!is.null(sums)){
    out <- t(apply(out, MAR = 1, FUN = sums))
  }
  return(out)
}

