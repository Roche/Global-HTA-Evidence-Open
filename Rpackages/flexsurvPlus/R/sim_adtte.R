#' A function to simulate Survival data
#'
#' This function simulates survival data with correlated time to progression and 
#' overall survival times. Optionally crossover from treatment arms can be simulated.
#' 
#' The simulation times are derived from formulas in Austin 2012 adapted to enable correlations between endpoints.
#' Austin, P.C. (2012), Generating survival times to simulate Cox proportional hazards models with time‐varying covariates. Statist. Med., 31: 3946-3958. https://doi.org/10.1002/sim.5452
#' 
#' @param rc_siminfo Should simulation params be included in simulated dataframe (logical). Defaults to FALSE.
#' @param rc_origos Should OS without switching be included in simulated dataframe (logical). Defaults to FALSE.
#' @param id Identifer added to simulated dataframe. Defaults to 1.
#' @param seed Seed used for random number generator. Defaults to 1234.
#' @param rho correlation coefficient between TTP and OS. Defaults to 0.
#' @param pswitch proportion of patients who switch. Defaults to 0.
#' @param proppd proportion of patients with PFS before switch allowed. Defaults to 0.
#' @param beta_1a treatment effect (as log(Hazard Ratio)) for OS pre PFS. Defaults to log(0.7).
#' @param beta_1b treatment effect (as log(Hazard Ratio)) for OS post PFS. Defaults to log(0.7).
#' @param beta_2a treatment effect (as log(Hazard Ratio)) for OS pre PFS (switch). Defaults to log(0.7).
#' @param beta_2b treatment effect (as log(Hazard Ratio)) for OS post PFS (switch). Defaults to log(0.7).
#' @param beta_pd treatment effect on progression (as log(HR)). Defaults to log(0.4).
#' @param arm_n patients per arm. Defaults to 250.
#' @param enroll_start start of enrollment. Defaults to 0.
#' @param enroll.end end of enrollment. Defaults to 1.
#' @param admin.censor end of trial. Defaults to 2.
#' @param os_gamma weibull shape - for OS. Defaults to 1.2.
#' @param os_lambda weibull scale - for OS. Defaults to 0.3.
#' @param ttp_gamma weibull shape - for TTP. Defaults to 1.5.
#' @param ttp_lambda weibull scale - for TTP. Defaults to 2.
#' @keywords Survival, Simulation
#' @export
#' @examples
#' require(survival)
#' require(dplyr)
#'
#' ADTTE <- sim_adtte()
#' survfit(Surv(AVAL, event = CNSR ==  0) ~ ARM, data = filter(ADTTE, PARAMCD == "PFS")) %>%
#'   plot()
#'
#' survfit(Surv(AVAL, event = CNSR ==  0) ~ ARM, data = filter(ADTTE, PARAMCD == "OS")) %>%
#'   plot()
sim_adtte <- function(
  rc_siminfo    = FALSE, # should simulation info be included?
  rc_origos     = FALSE, # should original OS without crossover be included?
  # params modified by sim 
  id            = 1, # identifier
  seed          = 1234 , # seed
  rho           = 0, # correlation coefficient between switch time and os
  pswitch       = 0, # proportion switch 
  proppd        = 0, # proportion of patients with PFS before switch allowed
  beta_1a       = log(0.7), # treatment effect (as log(Hazard Ratio))
  beta_1b       = log(0.7), # treatment effect (as log(Hazard Ratio))
  beta_2a       = log(0.7), # treatment effect (as log(Hazard Ratio))
  beta_2b       = log(0.7), # treatment effect (as log(Hazard Ratio))
  # params not changed across all sims
  beta_pd       = log(0.4),  # treatment effect on prog (as log(HR))  
  arm_n         = 250,       # patients per arm
  enroll_start  = 0,         # start of enrollment
  enroll_end    = 1,         # end of enrollment
  admin_censor  = 2,         # end of trial
  os_gamma      = 1.2,       # weibull shape - for OS
  os_lambda     = 0.3,       # weibull scale - for OS
  ttp_gamma     = 1.5,       # weibull shape - for TTP
  ttp_lambda    = 2          # weibull scale - for TTP
){
  set.seed(seed)  
  # combine so can select based on x 
  n_switch <- arm_n * pswitch  
  # u1 is just used to setup then not used
  # u2 and u3 are correlated uniform random variables used for OS and switch time
  # u4 used for censoring time (entry time)
  # u5 used for selecting switchers
  u <- matrix(nrow = arm_n*2, ncol = 5,data = runif(arm_n*2*5,0,1))
  # setup correlations using normal variables then convert back to uniform
  u[,3] <- pnorm(rho* qnorm(u[,2],0,1) + ((1-rho^2)^0.5)*qnorm(u[,1],0,1),0,1)  
  # generate covariates
  x.trt    <- rep(c(0,1), each = arm_n)  
  # generate os without treatment
  os.basis.t  <- (-(log(u[,2]) / (os_lambda)))^(1/os_gamma)  
  # generate ttp 
  ttp.untreated.t <- (-(log(u[,3]) / (ttp_lambda)))^(1/ttp_gamma)  
  # apply treatment effect
  ttp.treated.t <-(-(log(u[,3]) / (ttp_lambda*exp(beta_pd))))^(1/ttp_gamma)  
  # deive basis ttp
  ttp.basis.t <- ifelse(x.trt==1, ttp.treated.t, ttp.untreated.t)
  # generate pfs
  pfs.basis.t <- pmin(os.basis.t, ttp.basis.t)
  # generate entry time
  t.entry <- enroll_start + runif(u[,4])*(enroll_end-enroll_start)
  # generate follow up time
  t.censor <- admin_censor - t.entry
  # define a time in trial after which switch happens
  t.start.switch <- as.numeric(quantile(t.entry + pfs.basis.t, proppd))
  # who can switch - have ttp before os and censor
  can.switch <- as.numeric(
    ttp.basis.t < os.basis.t &
      ttp.basis.t < t.censor &
      t.entry+ttp.basis.t >= t.start.switch
  )[x.trt==0]
  
  rank <- rep(arm_n,arm_n)
  # choose randomly from those who can switch to get the reqd number of switchers
  rank[can.switch==1] <- order(u[x.trt==0 & can.switch==1,5])
  x.switch <- c(as.numeric(rank<=n_switch), rep(0,arm_n))
  # calculate how many actually switch 
  actual.p.switch <- sum(x.switch) / arm_n
  # generate switch time
  t.switch <- rep(NA,arm_n*2)
  t.switch[x.switch==1] <- ttp.basis.t[x.switch==1]
  
  # get control non switch os
  os.orig.t <- rep(NA,arm_n*2)
  os.orig.t[x.trt == 0] <- (-(log(u[x.trt==0,2])) / (os_lambda ))^(1/os_gamma)
  
  # get experimental arm os
  t0  <- ttp.basis.t 
  t0v <- t0 ^ os_gamma
  c1 <- os_lambda * exp(beta_1a)
  c2 <- os_lambda * exp(beta_1b)
  v1 <- 1/os_gamma
  os.orig.t[x.trt == 1] <- ifelse(-log(u[x.trt==1,2])  < c1 * t0v[x.trt==1],
                                  (-log(u[x.trt==1,2]) / c1) ^ v1,
                                  ((-log(u[x.trt==1,2]) - c1*t0v[x.trt==1] + c2*t0v[x.trt==1]) / c2) ^ v1
  )
  
  # get switch treatment (start from orig)
  os.cross.t <- os.orig.t
  t1 <- ttp.basis.t
  t2 <- ttp.basis.t + ttp.treated.t
  t1v <- t1^os_gamma
  t2v <- t2^os_gamma
  c3 <- os_lambda*exp(beta_2a)
  c4 <- os_lambda*exp(beta_2b)
  d1 <- x.switch==1 & -log(u[,2]) < os_lambda*t1v
  d3 <- x.switch==1 & -log(u[,2]) >= os_lambda*t1v + c3*(t2-t1)
  d2 <- x.switch==1 & -log(u[,2]) >= os_lambda*t1v & !d3
  os.cross.t[d1] <- (-log(u[d1,2]) / os_lambda )^v1
  os.cross.t[d2] <- ((-log(u[d2,2])-os_lambda*t1v[d2] + c3*t1v[d2]) / c3 )^v1
  os.cross.t[d3] <- ((-log(u[d3,2])-os_lambda*t1v[d3] - c3*(t2v[d3] - t1v[d3]) +c4*t2v[d3]) / c4 )^v1
  
  # apply censoring
  os.b.t <- pmin(os.basis.t, t.censor)
  os.b.c <- as.numeric(os.b.t==t.censor)
  os.o.t <- pmin(os.orig.t, t.censor)
  os.o.c <- as.numeric(os.o.t==t.censor)
  os.t <- pmin(os.cross.t, t.censor)
  os.c <- as.numeric(os.t==t.censor)
  # store ttp
  ttp.t <- pmin(ttp.basis.t, t.censor)
  ttp.c <- as.numeric(ttp.t==t.censor)
  # derive PFS (min of os and ttp) 
  pfs.t <- pmin(os.t, ttp.t)
  pfs.c <- as.numeric(pfs.t==t.censor)
  # create a data frame
  patid <- 1:(arm_n*2)  
  
  df_siminfo <- data.frame(
    sim_seed     = seed,
    sim_rho      = rho,
    sim_proppd = proppd,
    sim_pswitch = pswitch,
    sim_beta_1a = beta_1a,
    sim_beta_1b = beta_1b,
    sim_beta_2a = beta_2a,
    sim_beta_2b = beta_2b,
    sim_id   = id,
    sim_actpswitch = actual.p.switch
  )
  
  df_pfs <- data.frame(
    USUBJID = patid,
    ARMCD = ifelse(x.trt == 1, "B", "A"),
    ARM = ifelse(x.trt == 1, "Intervention Arm A", "Reference Arm B"),
    PARAMCD = "PFS",
    PARAM = "Progression Free Survival",
    AVAL = round(pfs.t * 365),
    AVALU = "DAYS",
    CNSR = pfs.c
  )
  
  df_os <- data.frame(
    USUBJID = patid,
    ARMCD = ifelse(x.trt == 1, "B", "A"),
    ARM = ifelse(x.trt == 1, "Intervention Arm A", "Reference Arm B"),
    PARAMCD = "OS",
    PARAM = "Overall Survival",
    AVAL = round(os.t * 365),
    AVALU = "DAYS",
    CNSR = os.c
  )
  
  df_os_exsw <- data.frame(
    USUBJID = patid,
    ARMCD = ifelse(x.trt == 1, "B", "A"),
    ARM = ifelse(x.trt == 1, "Intervention Arm A", "Reference Arm B"),
    PARAMCD = "OSORIG",
    PARAM = "Overall Survival Without Switching",
    AVAL = round(os.o.t * 365),
    AVALU = "DAYS",
    CNSR = os.o.c
  )
  
  rc <- rbind(df_pfs, df_os)
  
  if (rc_origos == TRUE){
    rc <- rbind(rc, df_so_exsw)
  }
  
  if (rc_siminfo == TRUE){
    rc <- cbind(rc, df_siminfo)
  }
  
  return(rc)
}