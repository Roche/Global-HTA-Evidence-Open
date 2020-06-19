context("Must generate table of effects estimates for each comparator from a `mtc.result` object which is the output of a `mtc.run`")

test_that("`get_mtc_allVsNew`", {

  # load mtc.result object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/mtc_result_generation.R
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # The example data contains treatement labels A, B, C, D, E, and F
  # Generate the output for treatment comparisson all vs "A"
  output <- get_mtc_allVsNew(mtc_ex, "A")

  # check each col for each comparison output

  expect_equal(output$Comparator,
               factor(c("B", "C", "D", "E", "F")))

  expect_equal(output$Med,
               c(0.127697933769324, -0.0701137292014444, -0.0355360718621047,
                 -0.162633567408904, 0.171512166437021))

  expect_equal(output$CIlo,
               c(0.0199843388482558, -0.264416712097963, -0.34038629514823,
                 -0.398085588147478, -0.318883946392043))

  expect_equal(output$CIup,
               c(0.235508322368515, 0.12732392864761, 0.275328477897011,
                 0.0725694649629136, 0.663220357841695))

})


test_that("`get_mtc_newVsAll`", {

  # load mtc.result object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/mtc_result_generation.R
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # The example data contains treatement labels A, B, C, D, E, and F
  # Generate the output for treatment "A" vs all vs "A"
  output <- get_mtc_newVsAll(mtc_ex, "A")

  # check each col for each comparison output

  expect_equal(output$Comparator,
               factor(c("B", "C", "D", "E", "F")))

  expect_equal(output$Med,
               c(-0.127697933769324, 0.0701137292014444, 0.0355360718621047,
                 0.162633567408904, -0.171512166437021))

  expect_equal(output$CIlo,
               c(-0.235508322368515, -0.12732392864761, -0.275328477897011,
                 -0.0725694649629136, -0.663220357841695))

  expect_equal(output$CIup,
               c(-0.0199843388482558, 0.264416712097963, 0.34038629514823,
                 0.398085588147478, 0.318883946392043))
})


context("Must allow specification of intervention label with argument `new.lab`")

test_that("`get_mtc_allVsNew`", {
  # load mtc.result object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/mtc_result_generation.R
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # The example data contains treatement labels A, B, C, D, E, and F
  # To test the comparator functionality we will generate all combinations
  output <- lapply(LETTERS[1:5], function(x){get_mtc_allVsNew(mtc_ex, x)})

  # As the previous tests have built in the expectation for the numerical
  # output of this function here I shall only test the comparator values
  expect_equal(output[[1]]$Comparator,
               factor(c("B", "C", "D", "E", "F")))

  expect_equal(output[[2]]$Comparator,
               factor(c("A", "C", "D", "E", "F")))

  expect_equal(output[[3]]$Comparator,
               factor(c("A", "B", "D", "E", "F")))

  expect_equal(output[[4]]$Comparator,
               factor(c("A", "B", "C", "E", "F")))

  expect_equal(output[[5]]$Comparator,
               factor(c("A", "B", "C", "D", "F")))

})


test_that("`get_mtc_newVsAll`", {
  # load mtc.result object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/mtc_result_generation.R
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # The example data contains treatement labels A, B, C, D, E, and F
  # To test the comparator functionality we will generate all combinations
  output <- lapply(LETTERS[1:5], function(x){get_mtc_newVsAll(mtc_ex, x)})

  # As the previous tests have built in the expectation for the numerical
  # output of this function here I shall only test the comparator values
  expect_equal(output[[1]]$Comparator,
               factor(c("B", "C", "D", "E", "F")))

  expect_equal(output[[2]]$Comparator,
               factor(c("A", "C", "D", "E", "F")))

  expect_equal(output[[3]]$Comparator,
               factor(c("A", "B", "D", "E", "F")))

  expect_equal(output[[4]]$Comparator,
               factor(c("A", "B", "C", "E", "F")))

  expect_equal(output[[5]]$Comparator,
               factor(c("A", "B", "C", "D", "F")))
})

context("Should allow specification of of transformation with `transform` argument")

test_that("`get_mtc_allVsNew`", {
  # load mtc.result object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/mtc_result_generation.R
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # Generate output with exponential transform argument "exp"
  output_exp <- get_mtc_allVsNew(mtc_ex,
                                 new.lab = "A",
                                 transform = "exp")

  # Generate non transform output for comparison
  output_cmp <-  get_mtc_allVsNew(mtc_ex,
                                  new.lab = "A")


  # check output is exponentiated
  expect_true(all(exp(output_cmp$Med) == output_exp$Med))
  expect_true(all(exp(output_cmp$CIlo) == output_exp$CIlo))
  expect_true(all(exp(output_cmp$CIup) == output_exp$CIup))

})


test_that("`get_mtc_newVsAll`", {
  # load mtc.result object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/mtc_result_generation.R
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # Generate output with exponential transform argument "exp"
  output_exp <- get_mtc_newVsAll(mtc_ex,
                                 new.lab = "A",
                                 transform = "exp")

  # Generate non transform output for comparison
  output_cmp <-  get_mtc_newVsAll(mtc_ex,
                                  new.lab = "A")


  # check output is exponentiated
  expect_true(all(exp(output_cmp$Med) == output_exp$Med))
  expect_true(all(exp(output_cmp$CIlo) == output_exp$CIlo))
  expect_true(all(exp(output_cmp$CIup) == output_exp$CIup))
})
