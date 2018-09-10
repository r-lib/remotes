
context("Installing package dependencies")

test_that("installing packages with dependencies", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  install_github("cran/desc", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("desc", lib.loc = lib))
  expect_equal(
    packageDescription("desc", lib.loc = lib)$RemoteRepo,
    "desc"
  )
})
