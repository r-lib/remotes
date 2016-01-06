
context("Install from GitHub")

test_that("install_github", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)
  install_github("cran/falsy", lib = lib, quiet = TRUE)

  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)
  expect_silent(packageDescription("falsy"))
  expect_equal(packageDescription("falsy")$RemoteRepo, "falsy")

})
