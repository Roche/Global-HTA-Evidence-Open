context("Must extract and create summary statistics table from a `mtc.result` object which is the output of a `mtc.run`")

test_that("`get_mtc_sum`", {
  # load mtc.result object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/mtcResultGeneration.R
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  output <- get_mtc_sum(mtc_ex)

  # check values
  expect_equal(output$DIC, 10.47)
  expect_equal(output$pD, 4.98)
  expect_equal(output$resDev, 5.48)
  expect_equal(output$dataPoints, 7)
})


context("Should allow for numerical rounding of output with argument `digits`")

test_that("`get_mtc_sum`", {
  # load mtc.result object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/mtcResultGeneration.R
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # output with more digits this time
  output <- get_mtc_sum(mtc_ex, digits = 5)

  # check values
  expect_equal(output$DIC, 10.4672)
  expect_equal(output$pD, 4.98298)
  expect_equal(output$resDev, 5.48421)
  expect_equal(output$dataPoints, 7)
})
