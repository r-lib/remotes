
context("Installing from local files")

test_that("install_local", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

  dir <- tempfile()
  on.exit(unlink(dir), add = TRUE)
  dir.create(dir)
  pkg <- download.packages("falsy", dir, type = "source")

  install_local(pkg[, 2], lib = lib)

  expect_silent(packageDescription("falsy"))
  expect_equal(packageDescription("falsy")$RemoteType, "local")

})
