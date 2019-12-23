## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  # install package from github
#  devtools::install_github(
#    'Roche/Global-HTA-Evidence-Open', subdir = "Rpackages/rpsftmPlus"
#  )

## ----setup--------------------------------------------------------------------
library(rpsftmPlus)
library(rpsftm)
library(survival)
library(ggplot2)
library(dplyr)
library(reshape2)

## ---- results = "asis"--------------------------------------------------------
immdef <- rpsftm::immdef
immdef$rx <- with(immdef, 1 - xoyrs/progyrs)
pander::pander(immdef[1:5,])

## -----------------------------------------------------------------------------
imm.fit <- rpsftm(Surv(progyrs, prog) ~ rand(imm, rx),
                  data=immdef,
                  censor_time=censyrs)

## -----------------------------------------------------------------------------
ggexposure(imm.fit)

## -----------------------------------------------------------------------------
ggatrisk(imm.fit)

## -----------------------------------------------------------------------------
rpsftm.atrisk(imm.fit, eval.times = c(0,1,2,3)) %>%
  dcast(Arm + Status ~ Time, value.var = "Count") %>%
  pander::pander()

## -----------------------------------------------------------------------------
ggdiscount(imm.fit)

## -----------------------------------------------------------------------------
ggpsi(imm.fit)

## -----------------------------------------------------------------------------
gglatent(imm.fit)

## -----------------------------------------------------------------------------
ggcfact(imm.fit)

## -----------------------------------------------------------------------------
rpsftm.coxph(imm.fit) %>%
  summary()

## -----------------------------------------------------------------------------
# 50% efficacy for switchers
imm.fit.s1 <- rpsftm.refit(imm.fit, k = 0.5) 
imm.fit.s1 %>% rpsftm.coxph() %>% summary()

## -----------------------------------------------------------------------------
# 80% efficacy for switchers
imm.fit.s2 <- rpsftm.refit(imm.fit, k = 0.8) 
imm.fit.s2 %>% rpsftm.coxph() %>% summary()

