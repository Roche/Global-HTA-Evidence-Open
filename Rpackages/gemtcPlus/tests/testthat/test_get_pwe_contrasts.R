context("Must extract hazard ratio estimates from piece-wise exponential model fit from an `r2jags` object with control of reference using arugment `ref`")

test_that("`get_pwe_contrasts`", {
  # load rjags fit list object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/rjags_generation.R
  rjags_list_ex <- readRDS(file.path("data", "rjags_output_list.RDS"))

  treatments <- unique(rjags_list_ex[[1]]$data.arms$treatment[order(rjags_list_ex[[1]]$data.arms$treatmentn)])
  rjags_list_ex[[1]]$fit$model.pars$cut.pts <- rjags_list_ex[[1]]$model.pars$cut.pts # cut points pulled from fit
  output <- get_pwe_contrasts(
                              fit = rjags_list_ex[[1]]$fit,
                              treatments = treatments,
                              ref = "B",
                              digits = 3,
                              exponentiate = FALSE,
                              reverse = TRUE
            )

  # check class
  expect_is(output, "data.frame")
  # check names
  expect_named(output, c("Comparison", "Segment",
                         "Median", "lCrI",
                         "uCrI", "x",
                         "xend"))
  # check values
  expect_equal(output$Comparison, c("B vs A", "B vs A", "B vs A",
                                    "B vs C", "B vs C", "B vs C",
                                    "B vs D", "B vs D", "B vs D",
                                    "B vs E", "B vs E", "B vs E",
                                    "B vs F", "B vs F", "B vs F"))

  expect_equal(output$Segment, factor(c(1L, 2L, 3L,
                                        1L, 2L, 3L,
                                        1L, 2L, 3L,
                                        1L, 2L, 3L,
                                        1L, 2L, 3L),
                                      label = c("[0,3)", "[3,10)", "[10,Inf)"),
                                      ordered = TRUE))

  expect_equal(output$Median, c(0.234, 0.255, -0.003,
                                0.383, -0.175, -0.032,
                                -0.934, -0.171, -0.084,
                                -0.602, 0.096, -0.043,
                                0.587, -0.955, 0.052))

  expect_equal(output$lCrI, c(-0.333, -0.025, -0.178,
                              -0.249, -0.579, -0.336,
                              -1.677, -0.542, -0.343,
                              -1.437, -0.335, -0.347,
                              -0.785, -1.704, -0.394))

  expect_equal(output$uCrI, c(0.833, 0.538, 0.172,
                              1.03, 0.221, 0.275,
                              -0.259, 0.181, 0.174,
                              0.196, 0.517, 0.25,
                              1.992, -0.237, 0.514))

  expect_equal(output$x, c(0, 3, 10,
                           0, 3, 10,
                           0, 3, 10,
                           0, 3, 10,
                           0, 3, 10))


  expect_equal(output$xend, c(3, 10, 24,
                              3, 10, 24,
                              3, 10, 24,
                              3, 10, 24,
                              3, 10, 24))

})

context("Should reverse the ordering of 'ref vs others' with argument `reverse`")

test_that("`get_pwe_contrasts`", {
    # load rjags fit list object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/rjags_generation.R
  rjags_list_ex <- readRDS(file.path("data", "rjags_output_list.RDS"))

  treatments <- unique(rjags_list_ex[[1]]$data.arms$treatment[order(rjags_list_ex[[1]]$data.arms$treatmentn)])
  rjags_list_ex[[1]]$fit$model.pars$cut.pts <- rjags_list_ex[[1]]$model.pars$cut.pts # cut points pulled from fit

  # Here we wish to test the `reverse` arugment so I generate two outputs:
  output_1 <- get_pwe_contrasts(
                                fit = rjags_list_ex[[1]]$fit,
                                treatments = treatments,
                                ref = "B",
                                digits = 3,
                                exponentiate = TRUE,
                                reverse = TRUE
            )

  output_2 <- get_pwe_contrasts(
                                fit = rjags_list_ex[[1]]$fit,
                                treatments = treatments,
                                ref = "B",
                                digits = 3,
                                exponentiate = TRUE,
                                reverse = FALSE
            )

  comp_1 <- output_1$Comparison
  comp_2 <- output_2$Comparison
  # Some string manipulation to get the sepearate levels in each comparison
  comp_1_A <- gsub("^(.+?) vs (.+?)$", "\\1", x = comp_1)
  comp_1_B <- gsub("^(.+?) vs (.+?)$", "\\2", x = comp_1)

  comp_2_A <- gsub("^(.+?) vs (.+?)$", "\\1", x = comp_2)
  comp_2_B <- gsub("^(.+?) vs (.+?)$", "\\2", x = comp_2)

  # checking the comparisons are the reverse of each other
  expect_equal(comp_1_A, comp_2_B)
  expect_equal(comp_1_B, comp_2_A)


})

context("Should exponentiate the contrast estimates with argument `exponentiate`")

test_that("`get_pwe_contrasts`", {
  # load rjags fit list object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/rjags_generation.R
  rjags_list_ex <- readRDS(file.path("data", "rjags_output_list.RDS"))

  treatments <- unique(rjags_list_ex[[1]]$data.arms$treatment[order(rjags_list_ex[[1]]$data.arms$treatmentn)])
  
  rjags_list_ex[[1]]$fit$model.pars$cut.pts <- rjags_list_ex[[1]]$model.pars$cut.pts # cut points pulled from fit
  # Create output with 'exponentiate = TRUE'
  output <- get_pwe_contrasts(
                              fit = rjags_list_ex[[1]]$fit,
                              treatments = treatments,
                              ref = "B",
                              digits = 3,
                              exponentiate = TRUE,
                              reverse = TRUE
            )


    # check values
  expect_equal(output$Comparison, c("B vs A", "B vs A", "B vs A",
                                    "B vs C", "B vs C", "B vs C",
                                    "B vs D", "B vs D", "B vs D",
                                    "B vs E", "B vs E", "B vs E",
                                    "B vs F", "B vs F", "B vs F"))

  expect_equal(output$Segment, factor(c(1L, 2L, 3L,
                                        1L, 2L, 3L,
                                        1L, 2L, 3L,
                                        1L, 2L, 3L,
                                        1L, 2L, 3L),
                                      label = c("[0,3)", "[3,10)", "[10,Inf)"),
                                      ordered = TRUE))

  expect_equal(output$Median, c(1.264, 1.29, 0.997,
                                1.467, 0.839, 0.969,
                                0.393, 0.843, 0.919,
                                0.548, 1.101, 0.958,
                                1.799, 0.385, 1.054))

  expect_equal(output$lCrI, c(0.717, 0.975, 0.837,
                              0.779, 0.561, 0.714,
                              0.187, 0.582, 0.709,
                              0.238, 0.715, 0.707,
                              0.456, 0.182, 0.674))

  expect_equal(output$uCrI, c(2.301, 1.713, 1.187,
                              2.801, 1.248, 1.317,
                              0.772, 1.199, 1.19,
                              1.216, 1.677, 1.284,
                              7.332, 0.789, 1.673))

  expect_equal(output$x, c(0, 3, 10,
                           0, 3, 10,
                           0, 3, 10,
                           0, 3, 10,
                           0, 3, 10))


  expect_equal(output$xend, c(3, 10, 24,
                              3, 10, 24,
                              3, 10, 24,
                              3, 10, 24,
                              3, 10, 24))
})
