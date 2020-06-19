context("Must calculate the survivor function for piecewise constant model")

test_that("`pwe_S`", {
  # Generate output - function vectorises with 'time' arg, so we use a vector
  # of length > 1 to test this functionality
  output <- pwe_S(time = 1:10, cut.pts = c(0, 3, 10, Inf), haz.rates = 1:5)

  # check class
  expect_is(output, "numeric")
  # check values
  expect_equal(output, c(0.135335283236613, 0.0183156388887342,
                         0.00247875217666636, 0.00012340980408668,
                         6.14421235332821e-06, 3.05902320501826e-07,
                         1.52299797447127e-08, 7.58256042791191e-10,
                         3.7751345442791e-11, 1.87931371994987e-12))
})
