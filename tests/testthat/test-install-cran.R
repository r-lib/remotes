context("Installing from CRAN")

test_that("", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

  install_cran("pkgconfig", lib = lib, force = TRUE)

  expect_silent(packageDescription("pkgconfig", lib.loc = lib))
})
