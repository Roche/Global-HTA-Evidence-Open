

context("Must create forest plot from `data.frame` output from `get_mtc_allVsNew`, `get_mtc_newVsAll`, `get_mtc_probBetter`"
)

test_that("`plot_mtc_forest`", {
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # The example data contains treatement labels A, B, C, D, E, and F
  # Generate the output for treatment comparisson all vs "A"
  output <- get_mtc_newVsAll(mtc_ex,
                             new.lab = "A",
                             transform = "exp",
                             digits = 2)

  p_out <-  plot_mtc_forest(output,
                            do.log = FALSE)

  # checking class
  expect_is(p_out, "ggplot")
  # checking values - ggplot object stores equivalent df of the input
  expect_equivalent(p_out$data, output)
  # checking labels as these should be consistent with the input object
  expect_equal(p_out$labels, list(yintercept = "yintercept",
                                  ymin = "CIlo", ymax = "CIup",
                                  x = "Comparator", y = "Med"))


})


context("Should allow for sorting the estimates by either 'name' (default) or 'effect' with argument `sort.by`")

test_that("`plot_mtc_forest`", {
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # The example data contains treatement labels A, B, C, D, E, and F
  # Generate the output for treatment comparisson all vs "A"
  output <- get_mtc_newVsAll(mtc_ex, new.lab = "A", transform = "exp", digits = 2)

  # unsorted plot
  p_1 <-  plot_mtc_forest(x = output,
                          do.log = FALSE)
  # sorted plot
  p_2 <-  plot_mtc_forest(x = output,
                          do.log = FALSE,
                          sort.by = "effect")

  # expect that the orders are NOT equal
  expect_false(all(order(p_1$data$Comparator) == order(p_2$data$Comparator)))
  # check actual ordering
  expect_equal(order(p_1$data$Comparator), 5:1)
})


context("Should allow setting of custom x-axis label with arugment `lab`")

test_that("`plot_mtc_forest`", {
  mtc_ex <- readRDS(file.path("data", "mtc_result_output.RDS"))

  # The example data contains treatement labels A, B, C, D, E, and F
  # Generate the output for treatment comparisson all vs "A"
  output <- get_mtc_newVsAll(mtc_ex, new.lab = "A", transform = "exp", digits = 2)

  p_out <-  plot_mtc_forest(x = output,
                            do.log = FALSE,
                            lab = paste("Hazard ratio",
                                        attr(output, "comparison")))

  expect_equal(p_out$labels$y, "Hazard ratio A vs other")


})



