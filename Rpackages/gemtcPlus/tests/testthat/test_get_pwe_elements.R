context("Must extract model name, exponents, comments, DIC abd pD from a `rjags` object containing a peicewise exponential model")

test_that("`get_pwe_elements`", {
  # load rjags fit list object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/rjags_generation.R
  rjags_list_ex <- readRDS(file.path("data", "rjags_output_list_grouped.RDS"))

  # get standard output (notice I must extract single element from list)
  output <- get_pwe_elements(rjags_list_ex[[1]])

  # check class
  expect_is(output, "list")
  # names
  expect_named(output, c("Model", "CutPoints", "DIC", "pD", "meanDev"))
  # values
  expect_equal(output, list(Model = "PWE, FE",
                            CutPoints = "3, 10",
                            DIC = 2665.5,
                            pD = 36.2,
                            meanDev = 2629.7
                           )
              )
})

