



test_that("MAIC full example", {
  require(dplyr)
  
  # load example data
  
  adsl <- read.csv(system.file("extdata", "adsl.csv", package = "MAIC", mustWork = TRUE))
  adrs <- read.csv(system.file("extdata", "adrs.csv", package = "MAIC", mustWork = TRUE))
  adtte <- read.csv(system.file("extdata", "adtte.csv", package = "MAIC", mustWork = TRUE))
  
  adsl <- adsl %>% # Data containing the matching variables
    mutate(SEX=ifelse(SEX=="Male", 1, 0)) # Coded 1 for males and 0 for females
  
  adrs <- adrs %>% # Response data
    filter(PARAM=="Response") %>%
    transmute(USUBJID, ARM, response=AVAL)
  
  adtte <- adtte %>% # Time to event data (overall survival)
    filter(PARAMCD=="OS") %>%
    mutate(Event=1-CNSR) %>% #Set up coding as Event = 1, Censor = 0
    transmute(USUBJID, ARM, Time=AVAL, Event)
  
  intervention_input <- adsl %>%
    full_join(adrs, by=c("USUBJID", "ARM")) %>%
    full_join(adtte, by=c("USUBJID", "ARM"))
  
  #####################################################
  # check data loads correctly
  expect_equal(nrow(intervention_input), 500)
  #####################################################
  
  # Baseline aggregate data for the comparator population
  target_pop <- read.csv(system.file("extdata", "Aggregate data.csv",
                                     package = "MAIC", mustWork = TRUE))
  
  # estimate weights
  match_cov <- c("AGE", "SEX", "SMOKE", "ECOG0")
  
  target_pop_standard <- target_pop %>%
    #EDIT
    rename(N=N,
           Treatment=ARM,
           AGE=age.mean,
           SEX=prop.male,
           SMOKE=prop.smoke,
           ECOG0=prop.ecog0
    ) %>%
    transmute(N, Treatment, AGE, SEX, SMOKE, ECOG0)
  
  intervention_data <- intervention_input %>%
    mutate(Age_centered = AGE - target_pop$age.mean,
           # matching on both mean and standard deviation for continuous variable (optional)
           Age_squared_centered = (AGE^2) - (target_pop$age.mean^2 + target_pop$age.sd^2),
           Sex_centered = SEX - target_pop$prop.male,
           Smoke_centered = SMOKE - target_pop$prop.smoke,
           ECOG0_centered = ECOG0 - target_pop$prop.ecog0)
  
  # Set matching covariates
  cent_match_cov <- c("Age_centered",
                      "Age_squared_centered",
                      "Sex_centered",
                      "Smoke_centered",
                      "ECOG0_centered")
  
  est_weights <- estimate_weights(intervention_data = intervention_data,
                                  matching_vars = cent_match_cov)
  
  
  #####################################################
  # check number of weights correct
  expect_equal(nrow(est_weights$analysis_data), 500)
  #####################################################
  
  ESS <- estimate_ess(est_weights$analysis_data)
  
  #####################################################
  # check ESS is as expected
  expect_equal(ESS, 157.07, tolerance = 0.01)
  #####################################################
  
  weight_summ <- summarize_wts(est_weights$analysis_data)
  
  #####################################################
  # check mean of weights are as expected
  expect_equal(weight_summ$mean, c(0.376,1), tolerance = 0.01)
  #####################################################
  
  
})
