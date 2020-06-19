context("Must generate table probablity of being better for each comparator from  a `mtc.result` object which is the output of a `mtc.run`")

test_that("`get_mtc_probBetter`, `mtc.prob.better.table`", {
  # load mtc.result object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/mtcResultGeneration.R
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # # TESTING `get_mtc_probBeter`

  # The example data contains treatement labels A, B, C, D, E, and F
  # Generate the output for treatment comparisson all vs "A"
  output_1 <- get_mtc_probBetter(mtc_ex, "A")

  # check new treatment
  expect_equal(unique(output_1$New), "A")

  # check compartor
  expect_equal(output_1$Comparator, LETTERS[2:6])

  # check values
  expect_equal(output_1$probNewBetter, c(0.989, 0.241, 0.411, 0.088, 0.755))

  # # TESTING `get_mtc_probBeter`
  # Now testing mtc.prob.better which doens't include a reference treatment
  output_2 <- mtc.prob.better.table(mtc_ex, smaller.is.better = TRUE)

  # checking class
  expect_is(output_2, "mtc.prob.better.table")

  # checking dimensions
  expect_equal(dim(output_2), c(6, 6))

  # checking dimnames
  expect_equal(dimnames(output_2), list(c("A", "B", "C", "D", "E", "F"),
                                        c("A", "B", "C", "D", "E", "F")))

  # checking some values (no need for exhaustive check)
  expect_equal(output_2["A", ], c("A" = NA,
                                  "B" = 0.0105333333333333,
                                  "C" = 0.7591,
                                  "D" = 0.588833333333333,
                                  "E" = 0.912166666666667,
                                  "F" = 0.2448 ))

})

context("Must allow setting a threshold against which contrasts
        are being compared with argument `threshold`")

test_that("`get_mtc_probBetter`, `mtc.prob.better.table`", {
  # load mtc.result object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/mtcResultGeneration.R
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # The example data contains treatement labels A, B, C, D, E, and F
  # Generate the output for treatment comparisson all vs "A" and
  # threshold of 0.6
  output_1 <- get_mtc_probBetter(mtc_ex, "A", threshold = 0.6)

  # check values
  expect_equal(output_1$probNewBetter, c(0, 0, 0, 0, 0.045))

  # # TESTING `get_mtc_probBeter` with threshold 0.6
  output_2 <- mtc.prob.better.table(mtc_ex,
                                    smaller.is.better = TRUE,
                                    threshold = 0.6)

  # checking some values (no need for exhaustive check)
  expect_equal(output_2["A", ], c("A" = NA,
                                  "B" = 1,
                                  "C" = 1,
                                  "D" = 1,
                                  "E" = 1,
                                  "F" = 0.955366666666667))

})

context("Should allow specification of the convention of whether
        samples being smaller or larger than threshold is of interest
        with argument `smaller.is.better`")

test_that("`get_mtc_probBetter`, `mtc.prob.better.table`", {
  # load mtc.result object from tests/data
  # for details on how this object was generated refer to the generation script:
  # tests/data/mtcResultGeneration.R
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # The example data contains treatement labels A, B, C, D, E, and F
  # Generate the output for treatment comparisson all vs "A" and
  # setting smaller.is.better to FALSE (default TRUE)
  output_1_sml <- get_mtc_probBetter(mtc_ex, "A", smaller.is.better = FALSE)

  # create comparison output with smaller.is.better left as TRUE
  output_1_cmp <- get_mtc_probBetter(mtc_ex, "A", smaller.is.better = TRUE)

  # check compartor
  expect_equal(output_1_sml$Comparator, LETTERS[2:6])

  # check values are consistent with P_sml = 1 - P_cmp
  expect_equal(output_1_sml$probNewBetter, 1 - output_1_cmp$probNewBetter)


  # # TESTING `get_mtc_probBeter` with smaller.is.better set to both TRUE/FALSE
  output_2_sml <- mtc.prob.better.table(mtc_ex,
                                        smaller.is.better = TRUE)

  output_2_cmp <- mtc.prob.better.table(mtc_ex,
                                        smaller.is.better = FALSE)


  # check values are consistent with P_sml = 1 - P_cmp
  expect_equal(output_2_sml["A", ], 1 - output_2_cmp["A", ])

})
