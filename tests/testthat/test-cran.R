
context("CRAN")

test_that("available_packages", {

  skip_on_cran()

  pkgs <- available_packages(
    repos = c(CRAN = "http://cran.rstudio.com"),
    type = "source"
  )

  expect_true(inherits(pkgs, "matrix"))
})
