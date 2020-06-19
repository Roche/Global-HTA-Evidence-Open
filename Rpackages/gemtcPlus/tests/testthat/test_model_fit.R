context("Must fit a range of models from supplied data and plan")

test_that("fit using jags",{
  model_plan <- plan_pwe(model.pars = list(cut.pts =  c(3, 10)),
                         bth.model = "FE", ref.std = "STUDY2", nma.ref.trt = "B",
                         n.chains = 2,
                         n.iter = 6000,
                         n.burnin = 1000,
                         n.thin = 1)
  model_input <- nma_pre_proc(grouped_TTE, model_plan)
  model  <- nma_fit(model_input = model_input)
  
  expect_is(model$model, "jags")  
  expect_named(model$model, c("ptr", "data", "model", "state", "nchain", "iter", "sync", 
                              "recompile"))
  expect_is(model$BUGSoutput, "bugs")
  expect_named(model$BUGSoutput,c("n.chains", "n.iter", "n.burnin", "n.thin", "n.keep", "n.sims", 
                                  "sims.array", "sims.list", "sims.matrix", "summary", "mean", 
                                  "sd", "median", "root.short", "long.short", "dimension.short", 
                                  "indexes.short", "last.values", "program", "model.file", "isDIC", 
                                  "DICbyR", "pD", "DIC"))
})


test_that("fit using mtc.model",{
  model_plan <-  plan_hr(bth.model = "FE",
                         n.chain = 3,
                         n.iter = 6000,
                         thin = 1,
                         n.adapt = 1000,
                         link = "identity",
                         linearModel = "fixed")
  model_input <- nma_pre_proc(hr_data, model_plan)
  model  <- nma_fit(model_input = model_input)
  

  expect_is(model, "mtc.result")  
  expect_named(model, c("samples", "deviance", "model"))
  
})