#' Run the simulation engine
#'
#' @param trt_list A vector of the names of the interventions evaluated in the simulation
#' @param common_pt_inputs A list of inputs that change across patients but are not affected by the intervention
#' @param unique_pt_inputs A list of inputs that change across each intervention
#' @param input_list A list of all other inputs: drc,drq,psa_bool,init_event_list,evt_react_list,
#' uc_lists = list(util_ongoing_list,util_instant_list,util_cycle_list,cost_ongoing_list,cost_instant_list,cost_cycle_list),
#' input_out,ipd,trt_list,simulation,npats,n_sim
#'
#' @return A data frame with the simulation results
#' @importFrom foreach %dopar% foreach
#' @importFrom purrr map map_dbl
#' @importFrom data.table rbindlist :=
#'
#' @keywords internal


RunEngine <- function(trt_list,
                      common_pt_inputs=NULL,
                      unique_pt_inputs=NULL,
                      input_list = NULL){
  # Create treatment list --------------------------
  trt_list <- trt_list
  simulation <- input_list$simulation
  n_sim <- input_list$n_sim
  npats <- input_list$npats
  psa_bool <- input_list$psa_bool

  #1 Loop per patient ----------------------------------------------------------
  PatData <- vector("list", length=npats) # empty list with npats elements
  # Outer loop, repeat for each patient
  PatData <- foreach(i = 1:npats,
                     .packages = (.packages()),
                     .export = c("input_list",ls(.GlobalEnv),ls(parent.env(environment())),ls(environment())),
                     .combine = 'c') %dopar% {


                       #Create empty pat data for each trt
                       this.PatData <- list()

                       input_list_pt <- c(input_list,list(i=i))
                       #extract the inputs that are common for each patient across interventions
                       if(!is.null(common_pt_inputs)){
                         for (inp in 1:length(common_pt_inputs)) {
                           list.common_pt_inputs <- lapply(common_pt_inputs[inp],function(x) eval(x, input_list_pt))
                           if (!is.null(names(list.common_pt_inputs[[1]]))) {
                             warning("Item ", names(list.common_pt_inputs), " is named. It is strongly advised to assign unnamed objects if they are going to be processed in the model, as they can create errors depending on how they are used within the model")
                           }
                           input_list_pt <- c(input_list_pt,list.common_pt_inputs)
                         }
                       }

                       #Make sure there are no duplicated inputs in the model, if so, take the last one
                       duplic <- duplicated(names(input_list_pt),fromLast = T)
                       if (sum(duplic)>0) { warning("Duplicated items detected, using the last one added")  }
                       input_list_pt <- input_list_pt[!duplic]

                       #2 Loop per treatment ------------------------------------------------------

                       for (trt in trt_list) {
                         # current time,  LYS, QALYs and costs for this patient
                         output_list <- list(curtime = 0, thslys = 0, thsqalys = 0, thscosts = 0, itemlys = 0, itemqalys = 0, itemcosts = 0)
                         input_list_trt <- NULL
                         input_list_trt <- c(input_list_pt,list(trt=trt))
                         if(!is.null(unique_pt_inputs)){
                           for (inp in 1:length(unique_pt_inputs)) {
                             list.unique_pt_inputs <- lapply(unique_pt_inputs[inp],function(x) eval(x, input_list_trt))
                             if (!is.null(names(list.unique_pt_inputs[[1]]))) {
                               warning("Item ", names(list.unique_pt_inputs), " is named. It is strongly advised to assign unnamed objects if they are going to be processed in the model, as they can create errors depending on how they are used within the model")
                             }
                             input_list_trt <- c(input_list_trt,list.unique_pt_inputs)

                           }
                         }
                         #Make sure there are no duplicated inputs in the model, if so, take the last one
                         duplic <- duplicated(names(input_list_trt),fromLast = T)
                         if (sum(duplic)>0) { warning("Duplicated items detected, using the last one added")  }
                         input_list_trt <- input_list_trt[!duplic]

                         # Generate event list
                         #if noeventlist, then just make start at 0
                         if (is.null(input_list_trt$init_event_list)) {
                           evt_list <- list(cur_evtlist = setNames(0,"start"), time_data = NULL)
                         } else{
                           evt_list <- do.call("InitEventList",list(trt,input_list_trt))
                         }

                         input_list_trt <- c(input_list_trt,evt_list$time_data,evt_list["cur_evtlist"])

                         # 3 Loop per event --------------------------------------------------------
                         #Main environment of reference
                         list_env <- list(list_env = environment())

                         input_list_trt <- c(input_list_trt, list_env)
                         this.PatData[[trt]]$evtlist <- NULL

                         input_list_trt <- c(input_list_trt,output_list)

                         n_evt <- 0
                         while(input_list_trt$curtime < Inf){
                           # Get next event, process, repeat
                           output_nxtevt <- GetNxtEvt(input_list_trt[["cur_evtlist"]])
                           Evt <- output_nxtevt$out
                           input_list_trt[['cur_evtlist']] <- output_nxtevt[["evt_list"]]

                           n_evt <- n_evt +1

                           if (is.null(Evt)==F){

                             input_list_trt <- ReactEvt(Evt, trt, input_list_trt)
                             #Save actual event list and times
                             extra_data <- input_list_trt[which(names(input_list_trt) %in% input_list_trt$input_out )]
                             if (length(extra_data)==0) {
                               this.PatData[[trt]]$evtlist[[n_evt]] <-   list(evtname = Evt$evt ,
                                                                              evttime = Evt$evttime,
                                                                              cost = input_list_trt[['itemcosts']],
                                                                              qaly = input_list_trt[['itemqalys']],
                                                                              ly = input_list_trt[['itemlys']],
                                                                              pat_id = i,
                                                                              trt = trt
                               )

                             } else{
                               this.PatData[[trt]]$evtlist[[n_evt]] <- c(evtname = Evt$evt ,
                                                                         evttime = Evt$evttime,
                                                                         cost = input_list_trt[['itemcosts']],
                                                                         qaly = input_list_trt[['itemqalys']],
                                                                         ly = input_list_trt[['itemlys']],
                                                                         pat_id = i,
                                                                         trt = trt,
                                                                         input_list_trt[which(names(input_list_trt) %in% input_list_trt$input_out )]
                               )
                             }


                           } else {input_list_trt$curtime <- Inf}

                           this.PatData[[trt]]$thslys <- input_list_trt$thslys
                           this.PatData[[trt]]$thsqalys <- input_list_trt$thsqalys
                           this.PatData[[trt]]$thscosts <- input_list_trt$thscosts
                         }

                       }


                       return(list(this.PatData))
                     }

  # Organize and create output -----------------------------------------------------------

  final_output <- list()

  #Create total measures from IPD
  for (trt in trt_list) {
    assign(paste0("lys.",trt), sum(unlist(map(map(PatData,trt),"thslys")))/npats)
    assign(paste0("qalys.",trt), sum(unlist(map(map(PatData,trt),"thsqalys")))/npats)
    assign(paste0("costs.",trt), sum(unlist(map(map(PatData,trt),"thscosts")))/npats)


  }

  counter <- 0
  for (trt in trt_list) {
    for (element in c("lys.","qalys.","costs."
    )) {
      counter <- counter +1

      final_output <- append(final_output,get(paste0(element,trt)))
      names(final_output)[counter] <-paste0(element,trt)

    }
  }

  final_output$trt_list <- trt_list
  #If we want to export the IPD of last iteration as well
  if (input_list$ipd==TRUE) {
    merged_df <- NULL
    for (trt in trt_list) {
      merged_df <- rbindlist(list(merged_df,rbindlist(unlist(map(map(PatData,trt),"evtlist"), recursive = FALSE))))
    }

    for (trt_ch in trt_list) {
      thscosts_pat <- map_dbl(map(PatData,trt_ch),"thscosts")
      thslys_pat <- map_dbl(map(PatData,trt_ch),"thslys")
      thsqalys_pat <- map_dbl(map(PatData,trt_ch),"thsqalys")
      names(thscosts_pat) <- 1:npats
      names(thsqalys_pat) <- 1:npats
      names(thslys_pat) <- 1:npats
      merged_df[trt==trt_ch,total_costs:= thscosts_pat[match(merged_df[trt==trt_ch,pat_id],names(thscosts_pat))]]
      merged_df[trt==trt_ch,total_qalys:= thsqalys_pat[match(merged_df[trt==trt_ch,pat_id],names(thsqalys_pat))]]
      merged_df[trt==trt_ch,total_lys:= thslys_pat[match(merged_df[trt==trt_ch,pat_id],names(thslys_pat))]]
    }

    final_output$merged_df <- merged_df

  }

  return(final_output)

}
