# Summary for deterministic/last created output for specific treatment ---------------------------

#' Deterministic results for a specific treatment
#'
#' @param out The final_output data frame from the list object returned by `RunSim()`
#' @param trt The reference treatment for calculation of incremental outcomes
#'
#' @return A dataframe with absolute costs, LYs, QALYs, and ICER and ICUR for each intervention
#' @export
#'
#' @examples
#' \dontrun{
#' summary_results_det(results$final_output,trt="int")
#' }

summary_results_det <- function(out = final_output, trt=NULL){

  trt <- ifelse(is.null(trt),out$trt_list[1],trt)

  other_trt_list <- out$trt_list[out$trt_list!=trt] #For any other treatment that is not the reference one, add the reference trt costs/lys/qalys

  for (other_trt in other_trt_list) { #add reference trt costs/lys/qalys to compare with the other trts
    out[[paste0("dlys.",other_trt)]] <-  out[[paste0("lys.",trt)]] - out[[paste0("lys.",other_trt)]]
    out[[paste0("dqalys.",other_trt)]] <-   out[[paste0("qalys.",trt)]] - out[[paste0("qalys.",other_trt)]]
    out[[paste0("dcosts.",other_trt)]] <-   out[[paste0("costs.",trt)]] - out[[paste0("costs.",other_trt)]]

    out[[paste0("ICER.",other_trt)]] <-   out[[paste0("dcosts.",other_trt)]] / out[[paste0("dlys.",other_trt)]]
    out[[paste0("ICUR.",other_trt)]] <-   out[[paste0("dcosts.",other_trt)]] / out[[paste0("dqalys.",other_trt)]]

  }

  out[[paste0("dlys.",trt)]] <-   0
  out[[paste0("dqalys.",trt)]] <-   0
  out[[paste0("dcosts.",trt)]] <-   0

  out[[paste0("ICER.",trt)]] <-   NA
  out[[paste0("ICUR.",trt)]] <-   NA


  data <- data.frame()
  for (trt in out$trt_list) {
    temp <- data.frame(
      trt = trt,
      costs = out[[paste0("costs.",trt)]],
      lys = out[[paste0("lys.",trt)]],
      qalys = out[[paste0("qalys.",trt)]],
      ICER = out[[paste0("ICER.",trt)]],
      ICUR = out[[paste0("ICUR.",trt)]]
    )

    data <- rbind(data,temp)
  }

  names <-  data[,1]

  # Transpose everything other than the first column
  data <- as.data.frame(as.matrix(t(data[,-1])))

  # Assign first column as the column names of the transposed dataframe
  colnames(data) <- names

  data <-round(data,2)

  return(data)

}

# Summary for PSA output for specific treatment --------------------------------------------------

#' Summary of PSA outputs for a treatment
#'
#' @param out The output_psa data frame from the list object returned by `RunSim()`
#' @param trt The reference treatment for calculation of incremental outcomes
#'
#' @return A data frame with mean and 95% CI of absolute costs, LYs, QALYs, ICER and ICUR for each intervention from the PSA samples
#' @export
#'
#' @examples
#' \dontrun{
#' summary_results_psa(results$output_psa, trt="int")
#' }

summary_results_psa <- function(out = output_psa, trt=NULL){

  trt <- ifelse(is.null(trt),out[[1]]$trt_list[1],trt)

  other_trt_list <- out[[1]]$trt_list[out[[1]]$trt_list!=trt] #For any other treatment that is not the reference one, add the reference trt costs/lys/qalys

  for (sim in 1:length(out)) {

    for (other_trt in other_trt_list) { #add reference trt costs/lys/qalys to compare with the other trts
      out[[sim]][[paste0("dlys.",other_trt)]] <-  out[[sim]][[paste0("lys.",trt)]] - out[[sim]][[paste0("lys.",other_trt)]]
      out[[sim]][[paste0("dqalys.",other_trt)]] <-   out[[sim]][[paste0("qalys.",trt)]] - out[[sim]][[paste0("qalys.",other_trt)]]
      out[[sim]][[paste0("dcosts.",other_trt)]] <-   out[[sim]][[paste0("costs.",trt)]] - out[[sim]][[paste0("costs.",other_trt)]]

      out[[sim]][[paste0("ICER.",other_trt)]] <-   out[[sim]][[paste0("dcosts.",other_trt)]] / out[[sim]][[paste0("dlys.",other_trt)]]
      out[[sim]][[paste0("ICUR.",other_trt)]] <-   out[[sim]][[paste0("dcosts.",other_trt)]] / out[[sim]][[paste0("dqalys.",other_trt)]]

    }

    out[[sim]][[paste0("dlys.",trt)]] <-   0
    out[[sim]][[paste0("dqalys.",trt)]] <-   0
    out[[sim]][[paste0("dcosts.",trt)]] <-   0

    out[[sim]][[paste0("ICER.",trt)]] <-   NA
    out[[sim]][[paste0("ICUR.",trt)]] <-   NA

  }

  data <- data.frame()
  for (trt in out[[1]]$trt_list) {
    temp <- data.frame(
      trt = trt,
      costs = interval_out(out,"costs.",trt,0),
      lys = interval_out(out,"lys.",trt,2),
      qalys =  interval_out(out,"qalys.",trt,2),
      ICER =  interval_out(out,"ICER.",trt,0),
      ICUR =  interval_out(out,"ICUR.",trt,0)
    )

    data <- rbind(data,temp)
  }

  names <-  data[,1]

  # Transpose everything other than the first column
  data <- as.data.frame(as.matrix(t(data[,-1])))

  # Assign first column as the column names of the transposed dataframe
  colnames(data) <- names

  return(data)

}


# Extract all specific PSA result -------------------------------------------------------------------------------------------------------------------------

#' Extract PSA results from a treatment
#'
#' @param x The output_psa data frame from the list object returned by `RunSim()`
#' @param element Variable for which PSA results are being extracted (single string)
#' @param trt Intervention for which PSA results are being extracted (single string)
#'
#' @return A dataframe with PSA results from the specified intervention
#' @export
#'
#' @examples
#' \dontrun{
#' extract_psa_result(results$output_psa,"costs","int")
#' }

extract_psa_result <- function(x, element,trt) {
  out <- purrr::map_dbl(x,paste0(paste0(element,"."),trt))

  out <- data.frame(element = element, trt = trt , simulation = 1:length(out),value=out)
  return(out)
}





# CEAC -------------------------------------------------------------------------------------------------------------------------

#' Calculate the cost-effectiveness acceptability curve (CEAC) for a DES model with a PSA result
#'
#' @param wtp Vector of length >=1 with the willingness to pay
#' @param results The list object returned by `RunSim()`
#' @param interventions A character vector with the names of the interventions to be used for the analysis
#'
#' @return A data frame with the CEAC results
#' @export
#'
#' @examples
#' \dontrun{
#' ceac_des(seq(from=10000,to=500000,by=10000),results)
#' }

ceac_des <- function(wtp, results, interventions = NULL) {

  if (is.null(interventions)) {
    interventions <- results$final_output$trt_list
  }

  nmb <- data.frame()
  for (comparator in interventions) {

     nmb_i <- data.frame(
       sapply(wtp, function(wtp_i) wtp_i * extract_psa_result(results$output_psa,"qalys",comparator)$value - extract_psa_result(results$output_psa,"costs",comparator)$value),
       stringsAsFactors = FALSE)

     names(nmb_i) <- format(wtp, scientific=F)
     nmb_i$iteration <-  1:nrow(nmb_i)
     nmb_i <- nmb_i %>% gather(key="wtp",value="nmb",-iteration)
     nmb_i$comparator <- comparator
     nmb_i$wtp <- as.numeric(nmb_i$wtp)

    nmb <- rbind(nmb,nmb_i)
  }

  nmb <- nmb %>%
    group_by(wtp,iteration) %>%
    mutate(best_nmb= ifelse(max(nmb)>0,comparator[nmb==max(nmb)],NA))

  ceac <- nmb %>%
    group_by(wtp,comparator) %>%
    summarise(prob_best= sum(best_nmb==comparator)/n()) %>%
    mutate(prob_best = ifelse(is.na(prob_best),0,prob_best))


  return(ceac)
}


# EVPI -------------------------------------------------------------------------------------------------------------------------

#' Calculate the Expected Value of Perfect Information (EVPI) for a DES model with a PSA result
#'
#' @param wtp Vector of length >=1 with the willingness to pay
#' @param results The list object returned by `RunSim()`
#' @param interventions A character vector with the names of the interventions to be used for the analysis
#'
#' @return A data frame with the EVPI results
#' @export
#'
#' @examples
#' \dontrun{
#' evpi_des(seq(from=10000,to=500000,by=10000),results)
#' }

evpi_des <- function(wtp, results, interventions = NULL) {

  if (is.null(interventions)) {
    interventions <- results$final_output$trt_list
  }


  nmb <- data.frame()
  for (comparator in interventions) {

    nmb_i <- data.frame(
      sapply(wtp, function(wtp_i) wtp_i * extract_psa_result(results$output_psa,"qalys",comparator)$value - extract_psa_result(results$output_psa,"costs",comparator)$value),
      stringsAsFactors = FALSE)

    names(nmb_i) <- format(wtp, scientific=F)
    nmb_i$iteration <-  1:nrow(nmb_i)
    nmb_i <- nmb_i %>% gather(key="wtp",value="nmb",-iteration)
    nmb_i$comparator <- comparator
    nmb_i$wtp <- as.numeric(nmb_i$wtp)

    nmb <- rbind(nmb,nmb_i)
  }

  nmb <-nmb %>%
    group_by(wtp,comparator) %>%
    mutate(mean_nmb=mean(nmb)) %>%
    group_by(wtp,iteration) %>%
    mutate(max_nmb=max(nmb)) %>%
    group_by(wtp) %>%
    mutate(max_mean_nmb = max(mean_nmb),
           mean_max_nmb = mean(max_nmb)) %>%
    ungroup() %>%
    mutate(evpi = mean_max_nmb - max_mean_nmb) %>%
    select(wtp,evpi)


  return(nmb)
}
