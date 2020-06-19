context("Must extract model name, exponents, comments, DIC abd pD from a list of `rjags` objects containing peicewise exponential models")

test_that("`get_pwe_comparison`", {
  # load rjags fit list object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/rjags_generation.R
  rjags_list_ex <- readRDS(file.path("data", "rjags_output_list_grouped.RDS"))

  output <- get_pwe_comparison(rjags_list_ex)

  # check class
  expect_is(output, "matrix")
  # values
  out_ex <-  matrix(list("PWE, FE", "PWE, RE (Turner prior)", 
                            "3, 10", "3, 10", 2665.5, 2667.5, 36.2, 37.1, 
                            2629.7, 2630.6), nrow = 2)
  colnames(out_ex) <- c("Model", "CutPoints", "DIC", "pD", "meanDev")
  expect_equal(output, out_ex)

})



