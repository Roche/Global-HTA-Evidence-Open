context("Must be able to pre-process data")


test_that("Pre-processing grouped survival data", {

  model_plan <- plan_pwe(model.pars = list(cut.pts =  c(3, 10)),
                         bth.model = "FE", ref.std = "STUDY2", nma.ref.trt = "B",
                         n.chains = 2,
                         n.iter = 6000,
                         n.burnin = 1000,
                         n.thin = 1)

  model_input <- nma_pre_proc(grouped_TTE, model_plan)
  
  expect_is(model_input$network, "network")
  expect_is(model_input$fitting_data, "list")

})


test_that("Pre-processing HR data", {

  model_plan <-  plan_hr(bth.model = "FE", 
                         n.chain = 3, 
                         n.iter = 6000, 
                         thin = 1,
                         n.adapt = 1000, 
                         link = "identity",
                         linearModel = "fixed")
  
  model_input <- nma_pre_proc(hr_data, model_plan)
  expect_named(model_input$fitting_data, c("description", "treatments", "data.re"))
 
})