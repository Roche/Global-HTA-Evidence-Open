#' Bootstrapping for MAIC weighted hazard ratios
#'
#' @param intervention_data  A data frame containing individual patient data
#'   from the intervention study.
#' @param matching A character vector giving the names of the covariates to use
#'   in matching. These names must match the column names in intervention_data.
#' @param i Index used to select a sample within \code{\link{boot}}.
#' @param model A model formula in the form 'Surv(Time, Event==1) ~ ARM'.
#'   Variable names need to match the corresponding columns in
#'   intervention_data.
#' @param comparator_data A data frame containing pseudo individual patient data
#'   from the comparator study needed to derive the relative treatment effect.
#'   The outcome variables names must match intervention_data.
#' @param min_weight A numeric value that defines the minimum weight allowed. 
#'   This value (default 0.0001) will replace weights estimated at 0 in a sample.
#'
#' @details This function is intended to be used in conjunction with the
#'   \code{\link{boot}} function to return the statistic to be
#'   bootstrapped. In this case by performing MAIC weighting using
#'   {\link{estimate_weights}} and returning a weighted hazard ratio (HR) from a
#'   Cox proportional hazards model. This is used as the 'statistic' argument in
#'   the boot function.
#'
#' @return The HR as a numeric value.
#'
#' @seealso \code{\link{estimate_weights}}, \code{\link{boot}}
#'
#' @example inst/examples/MAIC_example_analysis.R
#'
#' @export
bootstrap_HR <- function(intervention_data, matching, i, model, comparator_data, min_weight = 0.0001){

  # Samples the data
  bootstrap_data <- intervention_data[i,]

  # Estimates weights
  perform_wt <- estimate_weights(intervention_data=bootstrap_data, matching_vars=matching)

  # Give comparator data weights of 1
  comparator_data_wts <- comparator_data %>% dplyr::mutate(wt=1, wt_rs=1, ARM="Comparator")

  # Add the comparator data
  combined_data <- dplyr::bind_rows(perform_wt$analysis_data, comparator_data_wts)
  combined_data$ARM <- relevel(as.factor(combined_data$ARM), ref="Comparator")

  # set weights that are below eta to eta to avoid issues with 0 values
  combined_data$wt <- ifelse(combined_data$wt < min_weight, min_weight, combined_data$wt)
  
  # survival data stat
  cox_model <- survival::coxph(model, data = combined_data, weights = wt)
  HR <- exp(cox_model$coefficients)
}


#' Bootstrapping for MAIC weighted odds ratios
#'
#' @param intervention_data  A data frame containing individual patient data
#'   from the intervention study.
#' @param matching A character vector giving the names of the covariates to use
#'   in matching. These names must match the column names in intervention_data.
#' @param i Index used to select a sample within \code{\link{boot}}.
#' @param model A model formula in the form 'endpoint ~ treatment_var'. Variable
#'   names need to match the corresponding columns in intervention_data.
#' @param comparator_data A data frame containing pseudo individual patient data
#'   from the comparator study needed to derive the relative treatment effect.
#'   The outcome variables names must match intervention_data.
#' @param min_weight A numeric value that defines the minimum weight allowed. 
#'   This value (default 0.0001) will replace weights estimated at 0 in a sample.
#'
#' @details This function is intended to be used in conjunction with the
#'   \code{\link{boot}} function to return the statistic to be
#'   bootstrapped. In this case by performing MAIC weighting using
#'   {\link{estimate_weights}} and returning a weighted odds ratio (OR) from a
#'   logistic regression model. This is used as the 'statistic' argument in
#'   the boot function.
#'
#' @return The OR as a numeric value.
#'
#' @seealso \code{\link{estimate_weights}}, \code{\link{boot}}
#'
#' @example inst/examples/MAIC_example_analysis.R
#'
#' @export
bootstrap_OR <- function(intervention_data, matching, i, model, comparator_data, min_weight = 0.0001){

  # Samples the data
  bootstrap_data <- intervention_data[i,]

  # Estimates weights
  perform_wt <- estimate_weights(intervention_data=bootstrap_data, matching_vars=matching)

  # Give comparator data weights of 1
  comparator_data_wts <- comparator_data %>% dplyr::mutate(wt=1, wt_rs=1, ARM="Comparator")

  # Add the comparator data
  combined_data <- dplyr::bind_rows(perform_wt$analysis_data, comparator_data_wts)
  combined_data$ARM <- relevel(as.factor(combined_data$ARM), ref="Comparator")

  # set weights that are below eta to eta to avoid issues with 0 values
  combined_data$wt <- ifelse(combined_data$wt < min_weight, min_weight, combined_data$wt)
  
  # Perform logistic regression and extract the OR estimate
  logistic.regr <- suppressWarnings(glm(formula = model, family=binomial(link="logit"), data = combined_data, weight = wt))
  OR <- exp(as.numeric(coef(logistic.regr)[2]))
}
