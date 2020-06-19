context("Must create a list containing input elements for a range of models")

test_that("Fractional polynomial model plan is created using `plan_fp`", {

  model_plan <-  plan_fp(model.pars = list(exponents = 0, t.eval = "midpoint"),
                         bth.model = "FE", ref.std = "STUDY2", nma.ref.trt = "B")
  
  expect_named(model_plan, c("model", "engine", "analysis", "params"))
  expect_named(model_plan$params, c("seed", "model_params", "jags_init_params", 
                                    "deviance_params", "fit_params"))
  expect_named(model_plan$params$model_params, 
               c("n.chains", "n.iter", "n.burnin", "n.thin", "model.file", "parameters"))
  expect_named(model_plan$params$jags_init_params, c("model.pars", "ref.std", "nma.ref.trt", "feprior_mean", "feprior_prec", 
                                                     "bth.prior"))
  expect_named(model_plan$params$fit_params, c("RE", "descr_s", "descr"))
  
  expect_equal(model_plan$model, "FP")
  expect_equal(model_plan$engine, "rjags")
  expect_equal(model_plan$analysis, "GSD")
  
  expect_error(plan_fp(model.pars = list(exponents = 0, t.eval = "midpoint"), bth.model = "RE",
                       model.file = system.file("BUGScode", "gsd_fracpoly-1o_fe.txt", package = "gemtcPlus"),
                       ref.std = "STUDY2",  nma.ref.trt = "B"), 
               "For selected model bth.prior must not be NULL")
  
})


test_that("piecewise exponential model plan is created using `plan_pwe`", {
  model_plan <- plan_pwe(model.pars = list(cut.pts =  c(3, 10)),
                         bth.model = "FE", ref.std = "STUDY2", nma.ref.trt = "B",
                         n.chains = 2,
                         n.iter = 6000,
                         n.burnin = 1000,
                         n.thin = 1)
  
  expect_named(model_plan, c("model", "engine", "analysis", "params"))
  expect_named(model_plan$params, c("seed", "model_params", "jags_init_params", 
                                    "deviance_params", "fit_params"))
  expect_named(model_plan$params$model_params, 
               c("n.chains", "n.iter", "n.burnin", "n.thin", "model.file", "parameters"))
  expect_named(model_plan$params$jags_init_params, c("model.pars", "ref.std", "nma.ref.trt", "feprior_mean", "feprior_prec", 
                                                     "bth.prior"))
  expect_named(model_plan$params$fit_params, c("RE", "descr_s", "descr"))
  
  expect_equal(model_plan$model, "PWE")
  expect_equal(model_plan$engine, "rjags")
  expect_equal(model_plan$analysis, "GSD")
  
  expect_error(plan_pwe(model.pars = list(cut.pts =  c(3, 10)),
                        bth.model = "RE", ref.std = "STUDY2", nma.ref.trt = "B",
                        n.chains = 2,
                        n.iter = 6000,
                        n.burnin = 1000,
                        n.thin = 1, bth.prior = bth_prior(type = "sd", distr = "hn", param = list(mean = 1, prec = 1.5))), 
               "No bugs script found for model parameters selected")
  
})



test_that("Hazard Ratio model plan is created using `plan_hr`", {
  model_plan <-  plan_hr(bth.model = "FE",
                         n.chain = 3,
                         n.iter = 6000,
                         thin = 1,
                         n.adapt = 1000,
                         link = "identity",
                         linearModel = "fixed")
  expect_named(model_plan, c("engine", "analysis", "binary_data_type", "params"))
  expect_named(model_plan$params, c("seed", "model_params", "run_params", "jags_init"))
  expect_named(model_plan$params$model_params,
               c("n.chain", "hy.prior", "link", "om.scale", "linearModel"))
  expect_named(model_plan$params$run_params, c("n.iter", "n.adapt", "thin", "model"))
  expect_length(model_plan$params$jags_init, 3)

  expect_equal(model_plan$engine, "gemtc")
  expect_equal(model_plan$analysis, "HR")

  expect_error(plan_hr(bth.model = "REINT",
                               n.chain = 3,
                               n.iter = 6000,
                               thin = 1,
                               n.adapt = 1000,
                               link = "identity",
                               linearModel = "fixed")) # "'arg' should be one of “FE”, “RE”

})


test_that("Binary model plan is created using `plan_binary`", {
  model_plan <- plan_binary(bth.model = "RE", 
                            n.chain = 3, 
                            n.iter = 6000, 
                            thin = 1,
                            n.adapt = 1000, 
                            link = "logit",
                            bth.prior =  gemtc::mtc.hy.prior(type = "var",
                                                      distr = "dlnorm",-4.18, 1 / 1.41 ^ 2)
  )
  expect_named(model_plan, c("engine", "analysis", "binary_data_type", "params"))
  expect_named(model_plan$params, c("seed", "model_params", "run_params", "jags_init"))
  expect_named(model_plan$params$model_params, 
               c("n.chain", "hy.prior", "link", "om.scale"))
  expect_named(model_plan$params$run_params, c("n.iter", "n.adapt", "thin", "model"))
  expect_length(model_plan$params$jags_init, 3)
  
  expect_equal(model_plan$engine, "gemtc")
  expect_equal(model_plan$analysis, "BINARY")
  
  expect_error(plan_binary(bth.model = "FE", 
                       n.chain = 4, 
                       n.iter = 6000, 
                       thin = 1,
                       n.adapt = 1000, 
                       link = "identity",
                       linearModel = "fixed", 
                       jags_init = list(list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 94387), 
                                        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 24507), 
                                        list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 39483))))
  
})