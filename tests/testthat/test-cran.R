
context("CRAN")

test_that("available_packages", {

  skip_on_cran()

  pkgs <- available_packages(
    repos = getOption("repos"),
    type = getOption("pkgType")
  )

  expect_equal(class(pkgs), "matrix")
  
})
