#' Second order fractional polynomial
#'
#' @param x A vector with the dependent variable.
#' @param params A matrix with two columns giving the intercept and the "slope". If more than one rows given, the fractional polynomial is evaluated for each row.
#' @param exponents A vector giving the exponents (p1, p2) of the polynomial. 
#' @param sums A function. If non-null, sums of the fractional polynomial at each x value (i.e. summaries over the different params values) are calculated.
#' @return A matrix with length(x) rows giving the FP values or FP summaries.
#' @export
#' 
get_fp_2o <- function(x, params, exponents, sums=NULL){
  
  if (exponents[1] == 0){
    xt1 <- log(x)
  } else {
    xt1 <- x^exponents[1]
  }
  if (exponents[1] != exponents[2]){
    if (exponents[2] == 0){
      xt2 <- log(x)
    } else {
      xt2 <- x^exponents[2]
    }
  } else {
    if (exponents[2] == 0){
      xt2 <- log(x)^2
    } else {
      xt2 <- x^exponents[2] * log(x)
    }
  }
  
  X <- cbind(1, xt1, xt2)
  
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

