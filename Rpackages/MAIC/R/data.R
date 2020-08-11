#' est_weights
#'
#' An object containing weighted intervention data (est_weights$analysis_data)
#' and a vector of the centered matching variables (est_weights$matching_vars).
#'
#' @format est_weights$analysis_data is a data frame with 500 rows and 16 variables:
#' \describe{
#'   \item{USUBJID}{unique patient number}
#'   \item{ARM}{Name of the arm of the trial the patient was randomized to}
#'   \item{AGE}{Patient age, years}
#'   \item{SEX}{Patient gender: 1 for males and 0 for females}
#'   \item{SMOKE}{Variable to indicate whether the patients smokes: 1 for yes and 0 for no}
#'   \item{ECOG0}{ECOOG PS: 1 for ECOG PS 0 and 0 for ECOG PS >=1}
#'   \item{response}{Objective response}
#'   \item{Time}{Time to overall survival event/censor}
#'   \item{Event}{Overall survival event = 1, censor = 0}
#'   \item{response}{Objective response}
#'   \item{Age_centered}{Patient's age minus average age in the comparator population}
#'   \item{Age_squared_centered}{Patient's age squared - (Mean age in the target population squared + Standard deviation of age in the target population squared)}
#'   \item{Sex_centered}{SEX variable minus the proportion of males in the comparator population}
#'   \item{Smoke_centered}{SMOKE variable minus the proportion of smokers in the comparator population}
#'   \item{ECOG0_centered}{ECOG0 variable minus the proportion of patients with ECOG0 in the comparator population}
#'   \item{wt}{Propensity weight assigned to patient}
#'   \item{wt_rs}{Propensity weight rescaled}
#'
#' }
"est_weights"

#' target_pop_standard
#'
#' A data frame containing the aggregate baseline characteristics in the
#' comparator population.
#'
#' @format A data frame with 1 row and 6 variables:
#' \describe{
#'   \item{N}{Total number of patients in the comparator population}
#'   \item{Treatment}{Treatment received in the comparator population}
#'   \item{AGE}{Mean age}
#'   \item{SEX}{Proportion of males}
#'   \item{SMOKE}{Proportion of patients who smoke}
#'   \item{ECOG0}{Proportion of patients with ECOG 0}
#'
#'
#' }
"target_pop_standard"
