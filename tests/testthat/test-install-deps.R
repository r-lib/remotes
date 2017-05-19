
context("Installing package dependencies")

test_that("installing packages with dependencies", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

  install_github(
    "cran/desc",
    lib = lib,
    quiet = TRUE
  )

  expect_silent(packageDescription("desc"))
  expect_equal(packageDescription("desc")$RemoteRepo, "desc")
  expect_silent(packageDescription("R6"))
  expect_silent(packageDescription("crayon"))

})
