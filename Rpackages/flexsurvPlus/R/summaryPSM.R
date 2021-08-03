#' Extract information about fitted parametric survival models
#'
#' @param x An object created by calling \code{\link{runPSM}}
#' @param types A list of statistics to extract - see \code{\link{summary.flexsurvreg}} for details
#' @param t The time points to be used - see \code{\link{summary.flexsurvreg}} for details
#' @param ci Should a confidence interval be returned - see \code{\link{summary.flexsurvreg}} for details
#' @param se Should a standard error be returned - see \code{\link{summary.flexsurvreg}} for details
#'
#' 
#' @return A data frame containing the following values
#' \itemize{
#'   \item Model - as specified in \code{\link{runPSM}} model.type
#'   \item Dist - as specified in \code{\link{runPSM}} distr
#'   \item Strata - Either Intervention or Reference
#'   \item StrataName - As specified by int_name and ref_name respectively in \code{\link{runPSM}}
#'   \item type - as defined by the types parameter see \code{\link{summary.flexsurvreg}} for details
#'   \item variable - "est", "lcl", "ucl", "se" respectively
#'   \item time - either NA or the time the statistic is evaluated at
#'   \item value - estimated value
#'   }
#'   
#' @examples
#' 
#' require(dplyr)
#' require(ggplot2)
#' 
#' PFS_data <- sim_adtte(seed = 2020, rho = 0.6) %>%
#' filter(PARAMCD=="PFS") %>%
#' transmute(USUBJID,
#'             ARMCD,
#'             PFS_days = AVAL,
#'             PFS_event = 1- CNSR
#' )
#' 
#' psm_pfs <- runPSM(
#'   data = PFS_data,
#'   time_var = "PFS_days",
#'   event_var = "PFS_event",
#'   strata_var = "ARMCD",
#'   int_name = "A",
#'   ref_name = "B")
#' 
#' summaryPSM(psm_pfs, types = c("mean","rmst"), t = c(100,2000)) %>%
#'    filter(Dist == "gengamma", Strata == "Intervention")
#' 
#' summaryPSM(psm_pfs, types = "survival", t = seq(0,2000,100)) %>%
#'   ggplot(aes(x=time, y = value, color = StrataName, linetype = Model)) +
#'   geom_line()+
#'   facet_grid(~Dist)
#'
#' summaryPSM(psm_pfs, types = "hazard", t = seq(0,5000,100)) %>%
#'   ggplot(aes(x=time, y = value, color = StrataName, linetype = Model)) +
#'   geom_line()+
#'   facet_grid(~Dist) +
#'   coord_cartesian(ylim = c(0,0.02))
#'
#' summaryPSM(psm_pfs, types = "cumhaz", t = seq(0,5000,100)) %>%
#'   ggplot(aes(x=time, y = value, color = StrataName, linetype = Model)) +
#'   geom_line()+
#'   facet_grid(~Dist) +
#'   coord_cartesian(ylim = c(0,100))
#'
#' @export
summaryPSM <- function(x,
                          types = c("mean", "survival", "hazard", "cumhaz", "median", "rmst"),
                          t = NULL,
                          ci = FALSE,
                          se = FALSE
                          ){
 
  # check x is an object from runPSM
  assertthat::assert_that(
    any(names(x) == c("parameters_vector")) & any(names(x) == c("model_summary")),
    msg = "x must be created using runPSM"
  )
  
  # impute t if not provided
  if (is.null(t)){
    t <- seq(0, max(x$models[[1]]$data$Y[,"time"]), length.out = 10)
  }
  
  # extract metadata from the PSM model
  int_name <- x$model_summary$Intervention_name[1]
  if("Reference_name" %in% names(x$model_summary)){
    ref_name <- x$model_summary$Reference_name[1]
  } else{
    ref_name <- NA
  }

  # distinguish between models with and without treatment effects
  noarm.models <- x$models[which(grepl("onearm." ,names(x$models)) |grepl("sep." ,names(x$models)))]
  arm.models <- x$models[which(grepl("shp." ,names(x$models)))]
  
  # build appropriate new data for the models with and without parameters
  arm.newdata <- data.frame(ARM = c("Int", "Ref"))

  temp.df <- tibble(mdl = "", ARM = "", type = "", variable = "", time = 1, value = 1) %>%
    filter(1 ==2)
  
  ##################################################
  # cycle through the types and extract information
  ##################################################
  
  for (j in 1:length(types)){
  
    this.type <- types[j]
    
    # for mean and median need to force t to be null 
    
    if(this.type %in% c("mean", "median")){
      # cycle through the models
      lst1 <- lapply(noarm.models, FUN = tryFlexSurvPlusSummary, t = NULL, ci=ci, se=se, type = this.type) 
      lst2 <- lapply(arm.models, FUN = tryFlexSurvPlusSummary, t = NULL, ci=ci, se=se, type = this.type, newdata=arm.newdata)
    } else{
      # cycle through the models
      lst1 <- lapply(noarm.models, FUN = tryFlexSurvPlusSummary, t = t, ci=ci, se=se, type = this.type) 
      lst2 <- lapply(arm.models, FUN = tryFlexSurvPlusSummary, t = t, ci=ci, se=se, type = this.type, newdata=arm.newdata)
      }
    
    # combine and get the names of the models as define distributions etc
    this.lst <- c(lst1,lst2) 
    this.lbls <- names(this.lst)
    
    for (i in 1:length(this.lbls)){
      this.lst[[i]]$mdl = this.lbls[i]
    }
    
    this.df <- bind_rows(this.lst)
    
    # append to other results
    temp.df <- temp.df %>%
      bind_rows(this.df)
  }
  
  ##################################################
  # decode the model names
  ##################################################
  
  s1 <- strsplit(temp.df$mdl, split = ".", fixed = TRUE)
  
  final.df <- temp.df %>%
    mutate(
      mdl1 = sapply(s1, function(x){x[1]}),
      mdl2 = sapply(s1, function(x){x[2]}),
      mdl3 = sapply(s1, function(x){x[3]}),
      Model = mdl1,
      Dist = ifelse(mdl1 == "onearm", mdl3, mdl2),
      ARM2 = ifelse(mdl1 == "onearm", mdl2, mdl3),
      # decodes
      Strata = ifelse(is.na(ARM2), 
                      ifelse(ARM=="Int", "Intervention", "Reference"),
                      ifelse(ARM2 == "int", "Intervention", "Reference")
                      )
    )
  
  
  rc <- final.df %>%
    transmute(
      Model = ifelse(Model == "sep","Separate",
                     ifelse(Model == "comshp", "Common shape", 
                            ifelse(Model == "indshp", "Independent shape", "One arm"))),
      Dist,
      Strata,
      StrataName = ifelse(Strata == "Intervention", int_name, ref_name),
      type,
      variable,
      time,
      value) 
  
  return(rc)
  
}

############################################################################################################

# helper function that adds error handling to summary.flexsurvreg 
# and standardises return format
# just used internally
# creates tidy data returns even if models do not converge to simplify later processing


tryFlexSurvPlusSummary <- function(object, type, newdata = NULL, ci = TRUE, se = FALSE, t = NULL){
  
  # define a structure for return
  rc_struct <- tibble(
    ARM = "",
    type = "",
    variable = "",
    time = NaN,
    value = NaN) %>%
    filter(1 == 2)
  
  
  # check object is ok
  assertthat::assert_that(
    class(object) == "flexsurvreg",
    msg = "object must be flexsurvreg"
  )
  
  # check newdata matches expected format
  assertthat::assert_that(
    ifelse(is.null(newdata), TRUE, names(newdata) == "ARM"),
    msg = "Only models defined with flexsurvplus can be handled"
  )
  
  # try and get the summary
  rc.summ <- tryCatch(summary(object, type = type, newdata=newdata, t=t, ci = ci, se = se, tidy = TRUE), error = function(e) {e})
  
  stat_levels <- c("est", "lcl", "ucl", "se")[c(TRUE,ci,ci,se)]
  
  
  # catch the error and return an empty data frame
  if ("error" %in% class(rc.summ)){
    # check expected levels as a function of newdata and t
    # build a data frame matching format
    ARM_levels = ifelse(is.null(newdata), NA, newdata$ARM)
    t_levels = ifelse(is.null(t), NA, t)
    
    rc <- expand.grid(ARM = ARM_levels, time = t_levels, variable = stat_levels) 
    
    # no error so build a data frame of statistics 
  } else{
    
    # estimate
    rc <- rc.summ %>%
      transmute(variable = "est",
                value = est,
                ARM = "",
                time = NA) 
    
    # cis requested
    if(ci){
      rc.lcl <- rc.summ %>%
        transmute(ARM = "",
                  time = NA,
                  variable = "lcl",
                  value = lcl) 
      rc.ucl <- rc.summ %>%
        transmute(ARM = "",
                  time = NA,
                  variable = "ucl",
                  value = ucl)
      rc <- rc %>%
        bind_rows(rc.lcl, rc.ucl)
    }
    
    # se requested
    if(se){
      rc.se <- rc.summ %>%
        transmute(ARM = "",
                  time = NA,
                  variable = "se",
                  value = se) 
      rc <- rc %>%
        bind_rows(rc.se)
    }
    
    
    if("ARM" %in% names(rc.summ)){
      rc <- rc %>%
        mutate(ARM = rep(as.character(rc.summ$ARM), times = 1+2*ci+se))
    }
    
    if("time" %in% names(rc.summ)){
      rc <- rc %>%
        mutate(time = rep(rc.summ$time, times = 1+2*ci+se))
    }
    
  }
  
  # fix the format
  rc <- rc_struct %>%
    bind_rows(rc) 
  
  rc$type <- type
  
  return(rc)
  
}