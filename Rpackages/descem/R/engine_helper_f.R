# Initial event list --------------------------------------------------------------------------------------------------------------------------------------

#' Execute the initial time to events and separate the events from other inputs that are stored
#'
#' @param trt_name A character string of the name of the intervention
#' @param input_list_trt A list of simulation inputs
#'
#' @return A named vector of initial event times, and a named vector of other inputs to be stored
#'
#' @examples
#' InitEventList(trt = "int",input_list_trt = input_list_trt)
#'
#' @keywords internal
#' @noRd


InitEventList <- function(trt_name,input_list_trt){
  position <- which(trt_name==names(input_list_trt$init_event_list))
  # time_data <- eval(init_event_list[[position]][["expr"]],input_list_trt)
  time_data <- local({
    evts_v <- input_list_trt$init_event_list[[position]][["evts"]]
    othert_v <- input_list_trt$init_event_list[[position]][["other_inp"]]
    list2env(mget(c(evts_v,othert_v),ifnotfound=Inf), envir=environment()) #initialize
    eval(input_list_trt$init_event_list[[position]][["expr"]]) #run script
    evttime <- lapply(mget(evts_v,ifnotfound=Inf),unname) #get event times and make sure they are unnamed
    othertime <- if(!is.null(othert_v)){mget(othert_v,ifnotfound=Inf)} else{NULL}  #get other inputs times
    out <- list(evttime=evttime, othertime=othertime)
  },input_list_trt)

  #Event data
  cur_evtlist <- unlist(time_data$evttime)
  cur_evtlist <- sort(cur_evtlist)
  return(list(cur_evtlist = cur_evtlist, time_data = unlist(time_data$othertime)))
}



# Get next event ------------------------------------------------------------------------------------------------------------------------------------------

#' Identify which event to process next from a list of events
#'
#' @param evt_list A list of possible events with event times
#'
#' @return Two lists: one containing the name and time of the next event, the other with the remaining events to be processed
#'
#' @examples
#' GetNxtEvt(evt_list = input_list_trt$cur_evtlist)
#'
#' @keywords internal
#' @noRd

GetNxtEvt <- function(evt_list){                  # This function identifies which event is to be processed next for each patient, depending on intervention

  if (length(evt_list)>0) {
    cur_evtlist <- list(out = list(evt = names(evt_list[1]), evttime = evt_list[[1]]), evt_list = evt_list[-1])
  } else {
    cur_evtlist <- NULL
  }


  return(cur_evtlist)
}



# Reaction to Event ---------------------------------------------------------------------------------------------------------------------------------------

#' Evaluates the reactions of the event identified by GetNextEvt
#'
#' @param thisevt A two element list containing the first list from GetNextEvt: evt and evttime
#' @param trt A character string of the name of the intervention currently being processed
#' @param input_list_trt A list of simulation inputs
#'
#' @return The updated input list with after the reaction to the event is evaluated
#'
#' @examples
#' ReactEvt(thisevt="evt1",trt="int",input_list_trt=input_list_trt)
#'
#' @keywords internal
#' @noRd

ReactEvt <- function(thisevt,trt,input_list_trt=NULL){      # This function processes the next event (as identified in the GetNextEvt function)
  evt <- thisevt$evt                  # Identify event type
  prevtime <- input_list_trt$curtime                 # Identify time of previous event
  curtime <- thisevt$evttime         # Identify time of next event
  
  input_list_trt[["curtime"]] <- curtime
  input_list_trt[["evt"]] <- evt
  input_list_trt[["prevtime"]] <- prevtime
  input_list_trt[["trt"]] <- trt
  
  # Create costs and utilities for event --------------------------------------------------
  evt_trt <- paste(evt,trt,sep = "_")
  
  input_list_trt[c("cost_ongoing", "cost_instant", "cost_cycle", "cycle_l", "cycle_starttime", 
                   "utilmlt", "util_instant", "util_cycle", "util_cycle_l", "util_cycle_starttime")] <- lapply(list(
                     cost_ongoing         = get_input(input_list_trt$uc_lists$cost_ongoing_list,ifnull=0,type="cost",evt_trt_i=evt_trt,input_list_trt_i=input_list_trt),
                     cost_instant         = get_input(input_list_trt$uc_lists$cost_instant_list,ifnull=0,type="cost",evt_trt_i=evt_trt,input_list_trt_i=input_list_trt),
                     cost_cycle           = get_input(input_list_trt$uc_lists$cost_cycle_list,ifnull=0,type="cost",evt_trt_i=evt_trt,input_list_trt_i=input_list_trt),  #can be a vector
                     cycle_l              = get_input(input_list_trt$uc_lists$cost_cycle_list,ifnull=1,type="cycle_l",evt_trt_i=evt_trt,input_list_trt_i=input_list_trt), #can be a vector
                     cycle_starttime      = get_input(input_list_trt$uc_lists$cost_cycle_list,ifnull=0,type="cycle_starttime",evt_trt_i=evt_trt,input_list_trt_i=input_list_trt), #can be a vector
                     utilmlt              = get_input(input_list_trt$uc_lists$util_ongoing_list,ifnull=0,type="util",evt_trt_i=evt_trt,input_list_trt_i=input_list_trt), 
                     util_instant         = get_input(input_list_trt$uc_lists$util_instant_list,ifnull=0,type="util",evt_trt_i=evt_trt,input_list_trt_i=input_list_trt),
                     util_cycle           = get_input(input_list_trt$uc_lists$util_cycle_list,ifnull=0,type="util",evt_trt_i=evt_trt,input_list_trt_i=input_list_trt),    #can be a vector
                     util_cycle_l         = get_input(input_list_trt$uc_lists$util_cycle_list,ifnull=1,type="cycle_l",evt_trt_i=evt_trt,input_list_trt_i=input_list_trt), 
                     util_cycle_starttime = get_input(input_list_trt$uc_lists$util_cycle_list,ifnull=0,type="cycle_starttime",evt_trt_i=evt_trt,input_list_trt_i=input_list_trt) #can be a vector
                   ),unname)
  
  
  # Evaluate event ------------------------------------------------------------------------------------------------------------------------------------------
  input_list_trt <- eval_reactevt(input_list_trt$evt_react_list, evt,input_list_trt)
  
  return(input_list_trt)

}


# Evaluate event ------------------------------------------------------------------------------------------------------------------------------------------


#' Calculates the expression which has been defined in the reaction of the event
#'
#' @param x The evt_react_list from the input_list_trt object. It contains the reactions to events.
#' @param evt_name The current event being processed
#' @param input_list_trt A list of simulation inputs
#'
#' @return The modified input list with the updates after executing the corresponding reactions
#'
#' @examples
#' eval_reactevt(x = input_list_trt$evt_react_list,evt_name ="evt1",input_list_trt=input_list_trt)
#'
#' @keywords internal
#' @noRd

eval_reactevt <-  function(x,evt_name,input_list_trt=NULL){
  position <- which(evt_name==names(x))
  pos_l <- length(position)
  if (pos_l==0 | pos_l>1 ) {
    stop("Reaction to event ", evt_name, " not recognised or more than one reaction found. Make sure that only one reaction has been defined for the event")    
  } else{
    #compute outputs
    input_list_trt <- compute_outputs(cost_ongoing_i = input_list_trt[["cost_ongoing"]],
                                      cost_instant_i =input_list_trt[["cost_instant"]],
                                      cost_cycle_i=input_list_trt[["cost_cycle"]],
                                      util_cycle_i=input_list_trt[["util_cycle"]],
                                      util_instant_i=input_list_trt[["util_instant"]],
                                      utilmlt_i=input_list_trt[["utilmlt"]],
                                      cycle_l_i = input_list_trt[["cycle_l"]],
                                      util_cycle_l_i = input_list_trt[["util_cycle_l"]],
                                      prevtime_i=input_list_trt[["prevtime"]],
                                      curtime_i=input_list_trt[["curtime"]],
                                      starttime_i=input_list_trt[["cycle_starttime"]],
                                      util_starttime_i=input_list_trt[["util_cycle_starttime"]],
                                      input_list_trt = input_list_trt)
    
    #evaluate reaction
    
    input_list_trt <- local({
      input_list_trt <- input_list_trt
      eval(x[[position]][["react"]]) #run script
      out <- input_list_trt
    },input_list_trt)
    
    
    
  }
  return(input_list_trt)
  

}


# Get Inputs for Event -----------------------------------------------------------------------------------------------------------------------------------------------

#' Evaluates the cost/utility/cycle unevaluated expressions to be processed by the simulation engine
#'
#' @param x The specific cost/utility and its type (ongoing, instant...) to be used, created through add_cost/add_util
#' @param ifnull Value to be used if the input has not been defined
#' @param type Identifies what type of input is being used. Can be "cost", "util", "cycle_l" (cycle length) and "cycle_starttime" (starting time of the cycle)
#' @param evt_trt_i The event-intervention identifier to understand which specific input to use, separated by an underscore
#' @param input_list_trt_i  A list of simulation inputs
#'
#' @return A numeric vector of evaluated costs/utilities/cycle lengths/starting times for the specific event and intervention defined
#'
#' @examples
#' get_input(x = input_list_trt$uc_lists$cost_ongoing_list,ifnull=0,type="cost",evt_trt_i="evt1_int",input_list_trt_i=input_list_trt)
#'
#' @keywords internal
#' @noRd

get_input <-  function(x,ifnull=0,type,evt_trt_i =evt_trt, input_list_trt_i=input_list_trt){
  out <- NULL
  items <- x[names(x)==evt_trt_i]
  items_l <- length(items)
  if (items_l==0) {
    out <-  c(out,ifnull)
  } else{
    #If length is 1, then don't do loop as it's slow
    if (items_l==1) {
      out <-  c(out,
                if(is.null(items[[1]][[type]])){
                  ifnull
                } else{
                  eval(items[[1]][[type]],input_list_trt_i)
                } #lazy eval will give error on null, so just put 0 in that case
      )
      #otherwise do loop per item
    } else{
      for (i in 1:items_l) {
        out <-  c(out,
                  if(is.null(items[[i]][[type]])){
                    ifnull
                  } else{
                    eval(items[[i]][[type]],input_list_trt_i)
                  } #lazy eval will give error on null, so just put 0 in that case
        )
      }
    }

  }
  return(out)

}


# Compute outputs -----------------------------------------------------------------------------------------------------------------------------------------


#' Computes the discounted costs/qalys/lys using the evaluated expressions from get_input
#'
#' @param cost_ongoing_i A numeric with the continuous cost to be processed and discounted
#' @param cost_instant_i A numeric with the instantaneous cost to be processed and discounted
#' @param cost_cycle_i A numeric with the cycled cost to be processed and discounted
#' @param util_cycle_i A numeric with the cycled utility to be processed and discounted
#' @param util_instant_i A numeric with the instantaneous cost to be processed and discounted
#' @param utilmlt_i A numeric with the continuous utility to be processed and discounted
#' @param cycle_l_i A numeric vector with the cycle lengths to be used for the costs
#' @param util_cycle_l_i A numeric vector with the cycle lengths to be used for the utility
#' @param prevtime_i A numeric with the time of the previous event
#' @param curtime_i A numeric with the time of the current event
#' @param starttime_i A numeric vector with the starting times of the cycled costs. This is used to understand what is the corresponding time of the cycle for the current event.
#' @param util_starttime_i A numeric vector with the starting times of the cycled utilities. This is used to understand what is the corresponding time of the cycle for the current event.
#' @param input_list_trt The list with all the inputs stored
#'
#' @return The modified input list with the updated discounted outputs
#'
#' @examples
#' compute_outputs(cost_ongoing_i = 10, cost_instant_i = 0, cost_cycle_i = 0,util_cycle_i = 0, util_instant_i = 0, utilmlt_i = 0.7, cycle_l_i = 1, util_cycle_l_i = 0,prevtime_i = 0,curtime_i = 1,starttime_i = 0,=util_starttime_i = 0,input_list_trt = input_list_trt)
#'
#' @keywords internal
#' @noRd

compute_outputs <- function(cost_ongoing_i = cost_ongoing,
                            cost_instant_i =cost_instant,
                            cost_cycle_i=cost_cycle,
                            util_cycle_i=util_cycle,
                            util_instant_i=util_instant,
                            utilmlt_i=utilmlt,
                            cycle_l_i = cycle_l,
                            util_cycle_l_i = util_cycle_l,
                            prevtime_i=prevtime,
                            curtime_i=curtime,
                            starttime_i=cycle_starttime,
                            util_starttime_i=util_cycle_starttime,
                            input_list_trt = input_list_trt){
  drq <- input_list_trt$drq
  drc <- input_list_trt$drc
  additionals <-     AddOngoing(lcldrq=drq, lcldrc=drc,lclprvtime=prevtime_i, lclcurtime=curtime_i, lclvalq=utilmlt_i, 
                                lclvalc= cost_ongoing_i)
  instadditionals <- AddInstant(lcldrq=drq, lcldrc=drc,lclcurtime=curtime_i, lclvalq=util_instant_i,lclvalc=cost_instant_i)
  
  #Cycle costs and utilities
  if (length(cost_cycle_i)==1 & length(util_cycle_i)==1 & util_cycle_i==0 & cost_cycle_i==0) {
    cycleadditionals <- list(addcycleqalys=0, addcyclecosts=0)
  } else{
    cycleadditionals <- AddCycle(lcldrq=drq, lcldrc=drc,lclprvtime=prevtime_i, cyclelength = cycle_l_i,lclcurtime=curtime_i, lclvalq=0, lclvalc= cost_cycle_i,starttime = starttime_i) #cycles of 1 week
    cycleadditionals$addcycleqalys <- AddCycle(lcldrq=drq, lcldrc=drc,lclprvtime=prevtime_i, cyclelength = util_cycle_l_i,lclcurtime=curtime_i, lclvalq=util_cycle_i, lclvalc= 0,starttime = util_starttime_i)$addcycleqalys #cycles of 1 week
  }
  additional_ly <- AddOngoing(lcldrq=drq, lcldrc=drc,lclprvtime=prevtime_i, lclcurtime=curtime_i, lclvalq=1,lclvalc=0)$addqalys
  
  input_list_trt[["thsqalys"]] <-  input_list_trt$thsqalys + additionals$addqalys + instadditionals$addinstqalys + cycleadditionals$addcycleqalys
  input_list_trt[["thscosts"]] <- input_list_trt$thscosts + additionals$addcosts + instadditionals$addinstcosts + cycleadditionals$addcyclecosts
  input_list_trt[["thslys"]] <- input_list_trt$thslys + additional_ly
  input_list_trt[["itemlys"]] <- additional_ly
  input_list_trt[["itemcosts"]] <- additionals$addcosts + instadditionals$addinstcosts + cycleadditionals$addcyclecosts
  input_list_trt[["itemqalys"]] <- additionals$addqalys + instadditionals$addinstqalys + cycleadditionals$addcycleqalys
  
  return(input_list_trt)
  
}

# Helper function to create mean and 95% interval -------------------------

#' Calculate mean and 95% CI from samples
#'
#' @param x  The output_psa data frame from the list object returned by `RunSim()`
#' @param element Variable for which mean and 95% CIs are computed (single string)
#' @param trt Treatment for which mean and 95% CIs are computed (single string)
#' @param round_digit Number of digits to round outputs
#'
#' @return Mean and 95% CI from the PSA samples
#' @importFrom purrr map_dbl
#' @importFrom stats quantile
#'
#' @examples
#' interval_out(x=results$output_psa,element="costs.",trt="int",round_digit=3)
#'
#' @keywords internal
#' @noRd

interval_out <- function(x, element, trt,round_digit=2) {
  out <- paste0(round(mean(map_dbl(x,paste0(element,trt)),na.rm=TRUE),round_digit),
                "(",
                round(quantile(map_dbl(x,paste0(element,trt)),0.025,na.rm=TRUE),round_digit) ,
                ", ",
                round(quantile(map_dbl(x,paste0(element,trt)),0.975,na.rm=TRUE),round_digit),
                ")"
  )
  return(out)
}


# Continuous and instantaneous discounting ----------------------------------------------------------------------------------------------------------------

#' Calculate discounted costs and qalys between events
#'
#' @param lcldrq The discount rate for clinical outcomes
#' @param lcldrc The discount rate for cost outcomes
#' @param lclprvtime The time of the previous event in the simulation
#' @param lclcurtime The time of the current event in the simulation
#' @param lclvalq The LY/QALY value to be discounted
#' @param lclvalc The cost value to be discounted
#'
#' @return A list of additional costs and qalys based on continuous time discounting
#'
#' @examples AddOngoing(lcldrq=0.035, lcldrc=0.035, lclprvtime=0.5, lclcurtime=3, lclvalq=0.75, lclvalc=2500)
#'
#' @keywords internal
#' @noRd

AddOngoing <- function(lcldrq=0.035, lcldrc=0.035, lclprvtime, lclcurtime, lclvalq, lclvalc){

  Instantdrq <- log(1+lcldrq)
  Instantdrc <- log(1+lcldrc)

  # calculate additional qalys

  if (lcldrq==0) {
    addqalys <- lclvalq*(lclcurtime - lclprvtime)
  } else{
    addqalys <- ((lclvalq)/(0 - Instantdrq)) * (exp(lclcurtime * ( 0 - Instantdrq)) - exp(lclprvtime * (0 - Instantdrq)))

  }

  # calculate additional costs

  if (lcldrc==0) {
    addcosts <- lclvalc*(lclcurtime - lclprvtime)
  } else{
    addcosts <- ((lclvalc)/(0 - Instantdrc)) * (exp(lclcurtime * ( 0 - Instantdrc)) - exp(lclprvtime * (0 - Instantdrc)))

  }

  # combine additional costs and additional qalys in a list
  output <- list(addqalys=addqalys, addcosts=addcosts)

  return(output)
}


#' Calculate instantaneous discounted costs or qalys
#'
#' @param lcldrq The discount rate for clinical outcomes
#' @param lcldrc The discount rate for cost outcomes
#' @param lclcurtime The time of the current event in the simulation
#' @param lclvalq The LY/QALY value to be discounted
#' @param lclvalc The cost value to be discounted
#'
#' @return A list of additional costs and qalys based on discrete time discounting
#'
#' @examples AddInstant(lcldrq=0.035, lcldrc=0.035, lclcurtime=3, lclvalq=0.75, lclvalc=2500)
#'
#' @keywords internal
#' @noRd

AddInstant <- function(lcldrq=0.035, lcldrc=0.035, lclcurtime, lclvalq, lclvalc){


  addinstqalys <- lclvalq * ((1+lcldrq)^(-lclcurtime))          # Note use of DISCRETE TIME discounting for instantaneous costs and benefits
  addinstcosts <- lclvalc * ((1+lcldrc)^(-lclcurtime))
  # combine additional costs and additional qalys in a list
  output <- list(addinstqalys=addinstqalys, addinstcosts=addinstcosts)

  return(output)
}

# Cycle discounting -------------------------------------------------------

#' Cycle discounting
#'
#' @param lcldrq The discount rate for clinical outcomes
#' @param lcldrc The discount rate for cost outcomes
#' @param lclprvtime The time of the previous event in the simulation
#' @param cyclelength The cycle length
#' @param lclcurtime The time of the current event in the simulation
#' @param lclvalq The LY/QALY value to be discounted
#' @param lclvalc The cost value to be discounted
#' @param starttime The start time for accrual of cycle costs (if not 0)
#'
#' @return A list of additional costs and qalys based on cycle discounting
#'
#' @examples
#'
#' @keywords internal
#' @noRd

AddCycle <- function(lcldrq=0.035, lcldrc=0.035,lclprvtime=0, cyclelength,lclcurtime, lclvalq, lclvalc,starttime=0){

  addcycleqalys <- 0
  addcyclecosts <- 0

  #Note this makes the cycle utilities work weird, so do not use cycle utilities for now!
  for (i in 1:length(lclvalc)) {
    lclvalc_i <- lclvalc[i]
    lclvalq_i <- lclvalq[i]
    starttime_i <- starttime[i]
    cyclelength_i <- cyclelength[i]

    # cycles <- lclcurtime %/% cyclelength
    # cycles.done <- lclprvtime %/% cyclelength

    if (lclvalq_i==0 & lclvalc_i==0) {
      addcycleqalys <- sum(addcycleqalys,0)
      addcyclecosts <- sum(addcyclecosts,0)

    } else{

      cycle.time.total <- ifelse(starttime_i>= lclcurtime,0,seq(from=starttime_i, to = lclcurtime , by= cyclelength_i)) #all cycles that happened until current time of event

      # cycle.time.total <- seq(from=starttime, to = lclcurtime , by= cyclelength) #all cycles that happened until current time of event

      #If the cost starts at the selected starttime or at time 0, then include that time, otherwise exclude it
      if (lclprvtime==0) {
        cycle.time <- cycle.time.total[cycle.time.total >= lclprvtime]  #times at which the cycles take place during this event, put this condition to count also time 0
        n_cycles <- length(cycle.time)
        s <- (1+lcldrq)^cyclelength_i -1
        addcycleqalys <- sum(addcycleqalys,lclvalq_i * (1 - (1+s)^-n_cycles)/(s*(1+s)^-1) )
        s <- (1+lcldrc)^cyclelength_i -1
        addcyclecosts <- sum(addcyclecosts,lclvalc_i * (1 - (1+s)^-n_cycles)/(s*(1+s)^-1) )

      } else{
        if (starttime_i ==lclprvtime) {
          cycle.time <- cycle.time.total[cycle.time.total >= lclprvtime]  #times at which the cycles take place during this event, put this condition to count also time 0
        } else{
          cycle.time <- cycle.time.total[cycle.time.total > lclprvtime]  #times at which the cycles take place during this event
        }
        n_cycles_remaining <- length(cycle.time)
        d <- lclprvtime/cyclelength_i
        s <- (1+lcldrq)^cyclelength_i -1
        addcycleqalys <- sum(addcycleqalys, lclvalq_i * (1 - (1+s)^-n_cycles_remaining)/(s*(1+s)^(d)) )
        s <- (1+lcldrc)^cyclelength_i -1
        addcyclecosts <- sum(addcyclecosts, lclvalc_i * (1 - (1+s)^-n_cycles_remaining)/(s*(1+s)^(d)) )
      }

      # addcyclecosts <- sum( unlist( sapply(cycle.time,  function(x)  lclvalc * ((1+lcldrc)^(-x)) ) ) )#for each cycle, apply and discount cost
      # addcyclecosts <- sum( unlist( sapply(cycle.time,  function(x)  lclvalc * ((1+lcldrc)^(-x)) ) ) )#for each cycle, apply and discount cost
      #If starting from 0, can be changed substituting interest rate such that s = (1+r)^cyclelength - 1, and using the formula that lclvalq * (1 - (1+s)^-n_cycles)/(s*(1+s)^-1)
      #If starting from time t, then compute transformed time as d = t/cyclelength and use lclvalq * (1 - (1+s)^-n_cycles_remaining)/(s*(1+s)^(d-1)), where
      #n_cycles_remaining is the n_cycles - d (so the remaining cycles to be considered), e.g. if 13 cycles (From t=0), and delay 6 periods, then n_cycles_remaining = 7 and d=6

      # combine additional costs and additional qalys in a list

    }

  }


  output <- list(addcycleqalys=addcycleqalys, addcyclecosts=addcyclecosts)

  return(output)
}
