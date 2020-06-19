context("Must create factor vector of segments from supplied cut points in argument `cut.pts`")

test_that("`get_segments`", {
  output <- get_segments(c(3, 10))
  # check output
  expect_equal(output, factor(c("[0,3)", "[3,10)", "[10,Inf)"),
                              levels = c("[0,3)", "[3,10)", "[10,Inf)"),
                              ordered = TRUE))
})
