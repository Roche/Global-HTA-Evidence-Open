#' Run the simulation
#'
#' @param trt_list A vector of the names of the interventions evaluated in the simulation
#' @param common_all_inputs A list of inputs common across patients that do not change within a simulation
#' @param common_pt_inputs A list of inputs that change across patients but are not affected by the intervention
#' @param unique_pt_inputs A list of inputs that change across each intervention
#' @param init_event_list A list of initial events and event times. If no initial events are given, a "Start" event at time 0 is created automatically
#' @param evt_react_list A list of event reactions
#' @param util_ongoing_list A list of utilities that are accrued at an ongoing basis
#' @param util_instant_list A list of utilities that are accrued instantaneously at an event
#' @param util_cycle_list A list of utilities that are accrued in cycles
#' @param cost_ongoing_list A list of costs that are accrued at an ongoing basis
#' @param cost_instant_list A list of costs that are accrued instantaneously at an event
#' @param cost_cycle_list A list of costs that are accrued in cycles
#' @param npats The number of patients to be simulated
#' @param n_sim The number of simulations to run per patient
#' @param psa_bool A boolean to determine if PSA should be conducted. If n_sim > 1 and psa_bool = FALSE, the differences between simulations will be due to sampling
#' @param ncores The number of cores to use for parallel computing
#' @param drc The discount rate for costs
#' @param drq The discount rate for LYs/QALYs
#' @param input_out A vector of variables to be returned in the output data frame
#' @param ipd A boolean to determine if individual patient data should be returned. If set to false, only the main aggregated outputs will be returned (slightly speeds up code)
#' @param debug A boolean to determine if non-parallel RunEngine function should be used, which facilitates debugging. Setting this option to true will ignore the value of ncores
#'
#' @return A list of data frames with the simulation results
#' @importFrom doParallel registerDoParallel
#'
#' @export
#'
#' @examples
#' \dontrun{
#' RunSim(trt_list=c("int","noint"),
#' common_all_inputs = common_all_inputs,
#' common_pt_inputs = common_pt_inputs,
#' unique_pt_inputs = unique_pt_inputs,
#' init_event_list = init_event_list,
#' evt_react_list = evt_react_list,
#' util_ongoing_list = util_ongoing_list,
#' util_instant_list = util_instant_list,
#' cost_ongoing_list = cost_ongoing_list,
#' cost_instant_list = cost_instant_list,
#' npats = 500,
#' n_sim = 1,
#' psa_bool = FALSE,
#' ncores = 1,
#' drc = 0.035,
#' drq = 0.035,
#' ipd = TRUE)
#' }

RunSim <- function(trt_list=c("int","noint"),
                   common_all_inputs=NULL,
                   common_pt_inputs=NULL,
                   unique_pt_inputs=NULL,
                   init_event_list = NULL,
                   evt_react_list = evt_react_list,
                   util_ongoing_list = NULL,
                   util_instant_list = NULL,
                   util_cycle_list = NULL,
                   cost_ongoing_list = NULL,
                   cost_instant_list = NULL,
                   cost_cycle_list = NULL,
                   npats=500,
                   n_sim=1,
                   psa_bool = NULL,
                   ncores=1,
                   drc=0.035,
                   drq=0.035,
                   input_out = NULL,
                   ipd = TRUE,
                   debug = FALSE){

  # Not needed?
  # if (is.null(psa_bool)) {
  #   warning("psa_bool not defined in RunSim.")
  # }

  registerDoParallel(ncores)

  trt_list <- trt_list

  output_psa <- list()

  start_time_simulations <-  proc.time()
  for (simulation in 1:n_sim) {

    print(paste0("Simulation number: ",simulation))
    start_time <-  proc.time()

    input_list <- list(drc = drc,
                       drq = drq,
                       psa_bool = psa_bool,
                       init_event_list = init_event_list,
                       evt_react_list = evt_react_list,
                       uc_lists = list(util_ongoing_list = util_ongoing_list,
                                       util_instant_list = util_instant_list,
                                       util_cycle_list = util_cycle_list,
                                       cost_ongoing_list = cost_ongoing_list,
                                       cost_instant_list = cost_instant_list,
                                       cost_cycle_list = cost_cycle_list),
                       input_out = input_out,
                       ipd = ipd,
                       trt_list = trt_list,
                       simulation = simulation,
                       npats = npats,
                       n_sim = n_sim

    )

    #5.3 Draw Common parameters  -------------------------------
    if(!is.null(common_all_inputs)){
      for (inp in 1:length(common_all_inputs)) {
        set.seed(simulation)
        list.common_all_inputs <- lapply(common_all_inputs[inp],function(x) eval(x, input_list))
        if (!is.null(names(list.common_all_inputs[[1]]))) {
          warning("Item ", names(list.common_all_inputs), " is named. It is strongly advised to assign unnamed objects if they are going to be processed in the model, as they can create errors depending on how they are used within the model")
        }
        input_list <- c(input_list,list.common_all_inputs)
      }
    }

    #Make sure there are no duplicated inputs in the model, if so, take the last one
    duplic <- duplicated(names(input_list),fromLast = T)
    if (sum(duplic)>0) { warning("Duplicated items detected, using the last one added")  }
    input_list <- input_list[!duplic]

    #5.5 Run engine ----------------------------------------------------------

    #Run engine

    if (debug==TRUE) {
      final_output <- RunEngine_debug(trt_list=trt_list,
                                      common_pt_inputs=common_pt_inputs,
                                      unique_pt_inputs=unique_pt_inputs,
                                      input_list = input_list)                    # run simulation
    } else{
      final_output <- RunEngine(trt_list=trt_list,
                                common_pt_inputs=common_pt_inputs,
                                unique_pt_inputs=unique_pt_inputs,
                                input_list = input_list)                    # run simulation
    }

    if (input_list$ipd==TRUE) {

      final_output$merged_df$simulation <- simulation
    }


    output_psa[[simulation]] <- final_output





    print(paste0("Time to run iteration ", simulation,": ",  round(proc.time()[3]- start_time[3] , 2 ), "s"))
  }
  print(paste0("Total time to run: ",  round(proc.time()[3]- start_time_simulations[3] , 2), "s"))




  # Export results ----------------------------------------------------------


  results <- list(final_output=final_output,output_psa=output_psa)

  return(results)

}
