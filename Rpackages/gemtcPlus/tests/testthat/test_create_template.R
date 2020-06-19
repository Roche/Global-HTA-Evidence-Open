context("Must be able to create a template rmarkdown file")


test_that("A template can be created", {

  create_template(type = "binary")
  expect_true(file.exists("binary_model.Rmd"))
  file.remove("binary_model.Rmd")
})
