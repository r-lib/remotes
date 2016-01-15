
context("Install from Bitbucket")

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

  install_bitbucket("csardigabor/showimage", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("showimage"))
  expect_equal(packageDescription("showimage")$RemoteRepo, "showimage")

})
