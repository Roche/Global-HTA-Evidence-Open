#' Utility function to return jags data and model for reporting (e.g. in appendix)
#'
#' @param sims                   rjags object
#' @param include.comments       Logical, should comments in the model by included or stripped out (default: FALSE)
#' @param input.round.function   Function to apply to data inputs - default: function(x) signif(x, digits = 4)
#'
#' @author Iain Bennett (adaptations: Sandro Gsteiger)
#'
#' @return \code{jagsInfo} object which contains jags simulation information accesed via a call to  \code{print}
#' @export
#'

get_jags_info <- function(sims, include.comments = FALSE, input.round.function = function(x) signif(x, digits = 4)){

  ### convert the model to a string (strip out comment lines if requested)
  txt.model <- sims$model$model()

  if (include.comments){
    this.model <- paste(txt.model, collapse = "\r\n")
  } else{
    # remove whole line comments
    not.comments <- which(substr(gsub(" ", "", txt.model, fixed = TRUE), 1, 1) != "#")
    txt.model <- txt.model[not.comments]

    # remove inline comments
    inline.comments <- unlist(lapply(strsplit(txt.model, ''), function(x) which(x == '#')[1]))
    text.to.keep = ifelse(is.na(inline.comments), nchar(txt.model), inline.comments - 1)
    txt.model = substr(txt.model, 0, text.to.keep)

    this.model <- paste(txt.model, collapse = "\r\n")
  }

  ### convert data to a string in bugs compatible structure

  input.data  <- sims$model$data()

  this.data <- "list(\r\n"

  for (i in seq_along(input.data)){

    objname <- as.character(names(input.data)[i])
    obj <- input.data[[i]]

    if (!is.null(dim(obj))){
      #is a matrix
      this.data <- paste(this.data, objname," = structure(.Data = ", list(t(input.round.function(obj))),",.Dim = ", list(dim(obj)), ")")
    } else {
      #is a vector or single value
      this.data <- paste(this.data, objname, " = ", list(input.round.function(obj)))
    }
    if (i!=length(input.data)){
      this.data <- paste(this.data,",\r\n")
    }
  }
  this.data<- paste(this.data,"\r\n)\r\n")

  ### put it all together and return

  rc <- paste("##############################################",
              "# DATA                                       #",
              "##############################################",
              this.data,
              "##############################################",
              "# MODEL                                      #" ,
              "##############################################",
              this.model,
              "##############################################",
              sep = "\r\n")

  attr(rc, "class") <- "jagsInfo"

  return(rc)
}

print.jagsInfo <- function(rc) {
  cat(rc)
}
