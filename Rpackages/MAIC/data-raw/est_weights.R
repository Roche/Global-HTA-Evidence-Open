
library(dplyr)
library(MAIC)


#### Prepare the data ----------------------------------------------------------

### Intervention data

# Read in relevant ADaM data and rename variables of interest
adsl <- read.csv(system.file("extdata", "adsl.csv", package = "MAIC",
                             mustWork = TRUE))
adrs <- read.csv(system.file("extdata", "adrs.csv", package = "MAIC",
                             mustWork = TRUE))
adtte <- read.csv(system.file("extdata", "adtte.csv", package = "MAIC",
                              mustWork = TRUE))

adsl <- adsl %>% # Data containing the matching variables
  mutate(SEX=ifelse(SEX=="Male", 1, 0)) # Coded 1 for males and 0 for females

adrs <- adrs %>% # Response data
  filter(PARAM=="Response") %>%
  transmute(USUBJID, ARM, response=AVAL)

adtte <- adtte %>% # Time to event data (overall survival)
  filter(PARAMCD=="OS") %>%
  mutate(Event=1-CNSR) %>% #Set up coding as Event = 1, Censor = 0
  transmute(USUBJID, ARM, Time=AVAL, Event)

# Combine all intervention data
intervention_input <- adsl %>%
  full_join(adrs, by=c("USUBJID", "ARM")) %>%
  full_join(adtte, by=c("USUBJID", "ARM"))

# List out the variables in the intervention data that have been identified as
# prognostic factors or treatment effect modifiers and will be used in the
# matching
match_cov <- c("AGE",
               "SEX",
               "SMOKE",
               "ECOG0")

## Baseline data from the comparator trial
# Baseline aggregate data for the comparator population
target_pop <- read.csv(system.file("extdata", "aggregate_data.csv",
                                   package = "MAIC", mustWork = TRUE))

# Rename target population cols to be consistent with match_cov
target_pop_standard <- target_pop %>%
  rename(
    N=N,
    Treatment=ARM,
    AGE=age.mean,
    SEX=prop.male,
    SMOKE=prop.smoke,
    ECOG0=prop.ecog0
  ) %>%
  transmute(N, Treatment, AGE, SEX, SMOKE, ECOG0)

usethis::use_data(target_pop_standard, overwrite = TRUE)


#### Estimate weights ----------------------------------------------------------

### Center baseline characteristics
# (subtract the aggregate comparator data from the corresponding column of
# intervention PLD)
intervention_data <- intervention_input %>%
  mutate(
    Age_centered = AGE - target_pop$age.mean,
    # matching on both mean and standard deviation for continuous variables (optional)
    Age_squared_centered = (AGE^2) - (target_pop$age.mean^2 + target_pop$age.sd^2),
    Sex_centered = SEX - target_pop$prop.male,
    Smoke_centered = SMOKE - target_pop$prop.smoke,
    ECOG0_centered = ECOG0 - target_pop$prop.ecog0)

## Define the matching covariates
cent_match_cov <- c("Age_centered",
                    "Age_squared_centered",
                    "Sex_centered",
                    "Smoke_centered",
                    "ECOG0_centered")

## Optimization procedure
# Following the centering of the baseline characteristics of the intervention
# study, patient weights can be estimated using estimate_weights
# The function output is a list containing (1) a data set of the individual
# patient data with the assigned weights "analysis_data" and (2) a vector
# containing the matching variables "matching_vars"
est_weights <- estimate_weights(intervention_data = intervention_data,
                                matching_vars = cent_match_cov)

usethis::use_data(est_weights, overwrite = TRUE)
