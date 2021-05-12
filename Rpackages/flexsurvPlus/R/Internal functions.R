# Internal functions - Not exported ---------------------------------------
# Format data for analysis
Format_data_separate <- function(data, time_var, event_var, strata_var, int_name, ref_name) {
  validate_standard_data(data = data, time_var = time_var, event_var = event_var, strata_var = strata_var, int_name = int_name, ref_name = ref_name)
  dat <- data[,c(time_var, event_var, strata_var)]
  colnames(dat) <- c("Time", "Event", "ARM")
  dat.int <- dat %>% filter(ARM==int_name)
  dat.ref <- dat %>% filter(ARM==ref_name)
  return(list(dat.int=dat.int,dat.ref=dat.ref))
}

# Format data for analysis
Format_data_onearm <- function(data, time_var, event_var, int_name) {
  validate_standard_data_one_arm(data = data, time_var = time_var, event_var = event_var, int_name = int_name)
  dat <- data[,c(time_var, event_var)] 
  colnames(dat) <- c("Time", "Event")
  dat.int <- dat %>%
    dplyr::mutate(ARM=int_name)
    return(dat.int)
}

# with variable for ARM
Format_data <- function(data, time_var, event_var, strata_var, int_name, ref_name) {
  validate_standard_data(data = data, time_var = time_var, event_var = event_var, strata_var = strata_var, int_name = int_name, ref_name = ref_name)
  
  dat <- data[,c(time_var, event_var, strata_var)]
  colnames(dat) <- c("Time", "Event", "ARM")
  
  dat <- dat %>%
    filter(ARM %in% c(int_name, ref_name)) %>% 
    mutate(ARM = ifelse(ARM==int_name, "Int", "Ref"),
           ARM = factor(ARM, levels = c("Ref", "Int")),
           ARM = relevel(ARM, ref = "Ref"))
  
  return(dat)
}


# validate the standard data

# validate the data 

validate_standard_data <- function(data, time_var, event_var, strata_var, ref_name, int_name){
  
  assertthat::assert_that(
    time_var %in% names(data),
    msg = paste0("time_var = ", time_var, " is not found in data.")
  )
  
  assertthat::assert_that(
    event_var %in% names(data),
    msg = paste0("event_var = ", event_var, " is not found in data.")
  )
  
  assertthat::assert_that(
    strata_var %in% names(data),
    msg = paste0("strata_var = ", strata_var, " is not found in data.")
  )
  
  dat <- data[,c(time_var, event_var, strata_var)] 
  colnames(dat) <- c("Time", "Event", "ARM")
  
  filt_dat <- dat %>%
    dplyr::filter(ARM %in% c(ref_name, int_name))
  
  included.trts <- unique(dat$ARM) %>%
    as.character()
  
  this.msg = paste0("int_name = '", int_name, "' is not found in ", strata_var,
                    ". Possible values are: '", paste(included.trts, collapse = "', '"), "'")
  
  assertthat::assert_that(
    all(int_name %in% included.trts),
    msg = this.msg
  )
  
  this.msg = paste0("ref_name = '", ref_name, "' is not found in ", strata_var, 
                    ". Possible values are: '", paste(included.trts, collapse = "', '"), "'")
  
  assertthat::assert_that(
    all(ref_name %in% included.trts),
    msg = this.msg
  )
  
  assertthat::assert_that(
    all(dat$Time > 0),
    msg = paste0("Invalid time values found. All values of time_var = ", time_var, " must be greater than 0")
  )
  
  assertthat::assert_that(
    all(dat$Event %in% c(0,1)),
    msg = paste0("Invalid event values found. All values of event_var = ", event_var, " must be 0 or 1 only. With 1 indicating event.")
  )
  
}

# validates data for one-arm models - doesn't need validation around the name
validate_standard_data_one_arm <- function(data, time_var, event_var, int_name){
  
  assertthat::assert_that(
    time_var %in% names(data),
    msg = paste0("time_var = ", time_var, " is not found in data.")
  )
  
  assertthat::assert_that(
    event_var %in% names(data),
    msg = paste0("event_var = ", event_var, " is not found in data.")
  )
  

  assertthat::assert_that(
    all(data$Time > 0),
    msg = paste0("Invalid time values found. All values of time_var = ", time_var, " must be greater than 0")
  )
  
  assertthat::assert_that(
    all(data$Event %in% c(0,1)),
    msg = paste0("Invalid event values found. All values of event_var = ", event_var, " must be 0 or 1 only. With 1 indicating event.")
  )
  
}



# modify the param_out data frame to exp coefs on the log scale
# this data frame is created in all the run... functions
# however, coef has some values on log scale so need to post process

post_process_param_out <- function(param_out){
  
  # these parameters are returned on log scale by coef.flexsurvreg so need update
  
  logpars <- c(
    "exp.rate", 
    "weibull.shape","weibull.scale",
    "gompertz.rate",
    "lnorm.sdlog",
    "llogis.shape","llogis.scale",
    "gengamma.sigma",
    "gamma.shape", "gamma.rate",
    "genf.sigma", "genf.P") 
  
  logpars.ref <- paste0(logpars, ".ref") 
  logpars.int <- paste0(logpars, ".int")
  
  # identify columns needing changes
  columns_to_exp <- names(param_out) %in% c(logpars.ref, logpars.int)
  
  # exponentiate those values
  rc <- param_out
  rc[,columns_to_exp] <- exp(rc[,columns_to_exp])
  
  return(rc)
  
}

