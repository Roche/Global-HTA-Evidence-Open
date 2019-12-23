# these functions are used within package

################################################
# fget
################################################
# similar to mget but starting from current frame works outwards
# to find object or returns an error
################################################
# x - character string naming an object
################################################

fget <- function(x){
  f2look <- sys.frames()

  rc <-  mget(x, ifnotfound = "NOPE", inherits = "TRUE")
  for (i in 1:length(f2look)){
    this.rc <- mget(x, envir = f2look[[i]], ifnotfound = "NOPE")
    if (this.rc != "NOPE"){
      rc <- this.rc
    }
  }

  if (rc == "NOPE"){
    stop(paste0(x, " is not found in current environments."))
  }

  rc <- rc[[1]]

  return(rc)
}

################################################
# rpsftm.formula
################################################
# returns a modified formula derived from an rpsftm
# call. Strips out the rand() component.
# Designed to work with rpsftm.data()
################################################
# x - rpsftm object
################################################


rpsftm.formula <- function(x){

  if (!("rpsftm" %in% class(x))){
    stop("Please provide an rpsftm object.")
  }

  # response
  lfrm <- as.character(x$call$formula)[2]

  # regression
  rfrm <- as.character(x$call$formula)[3] %>%
    strsplit("+", fixed = TRUE) %>%
    unlist()

  rfrm2 <- c(".arm", rfrm[!grepl("rand(", rfrm, fixed = TRUE)]) %>%
    paste(collapse = " + ")

  rc = as.formula(paste("Surv(time, cens) ~", rfrm2))

  return(rc)

}

################################################
# rpsftm.data
################################################
# returns a data frame derived from an rpsftm
# call.
# Designed to work with rpsftm.formula()
# Uses fget to search for data used in rpsftm call
# Will create error if data= was not used
# variables included:
# - .arm - 0 or 1 - randomized to experimental
# - .rx - 0 to 1 - exposure to experimental treatment
# - time - event time
# - cens - 0 for censored, 1 for event
# - censor_time - used for recensoring
# - any stratification variables used
################################################
# x - rpsftm object
################################################

rpsftm.data <- function(x){
  if (!("rpsftm" %in% class(x))){
    stop("Please provide an rpsftm object.")
  }
  if (is.null(x$call$data)){
    stop("Sorry this only works when the data= option is used with rpsftm.")
  }

  rnd <- as.data.frame(x$rand)

  df <- fget(as.character(x$call$data)) %>%
    mutate(.arm  = rnd$arm, .rx = rnd$rx)

  charsurv <- as.character(x$call$formula)[2] %>%
    strsplit(split = "Surv(", fixed = TRUE) %>%
    unlist() %>%
    strsplit(split = ",", fixed = TRUE) %>%
    unlist() %>%
    strsplit(split = ")", fixed = TRUE)  %>%
    unlist()

  tvar <- charsurv[1]
  cvar <- charsurv[2]

  time <- transmute_(df, tvar) %>%
    unlist() %>%
    as.numeric()

  cens <-  transmute_(df, cvar) %>%
    unlist() %>%
    as.numeric()

  if (!is.null(x$call$censor_time)) {

    censor_time <-  transmute_(df, x$call$censor_time) %>%
      unlist() %>%
      as.numeric()

    rc <- df %>%
      transmute( .arm, .rx, time = time, cens = cens, censor_time = censor_time)

  } else{
    rc <- df %>%
      transmute( .arm, .rx, time = time, cens = cens)
  }

  rc <- rc %>%
    mutate(t.on = time * .rx, t.off = time - t.on)

  # get strat vars if exist

  if (grepl("strata", as.character(x$call$formula)[3], fixed = TRUE)){

    stratvar <- as.character(x$call$formula)[3] %>%
      strsplit(split = "strata(", fixed = TRUE) %>%
      unlist()

    stratvar <- stratvar[!grepl("rand(", stratvar, fixed = TRUE)] %>%
      strsplit(split = ")", fixed = TRUE) %>%
      unlist()

    stratvar <- stratvar[!grepl("+", stratvar, fixed = TRUE)]

    rc <- rc %>%
      cbind(df[stratvar])
  }

  return(rc)

}
