context("Must calculate the cumulative hazard over [0, tmax] for piecewise constant model")

test_that("`pwe_H`", {
  # Generate output - function vectorises with 'time' arg, so we use a vector
  # of length > 1 to test this functionality
  output <- pwe_Hu(time = 1, cut.pts = c(0, 3, 10, Inf), haz.rates = 1:5)

  # check class
  expect_is(output, "numeric")
  # check values
  expect_equal(output, c(2))
})
