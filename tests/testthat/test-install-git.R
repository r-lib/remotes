
context("Install from git repo")

test_that("install_git", {

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

  url <- "https://github.com/cran/falsy.git"
  install_git(url, lib = lib, quiet = TRUE)

  expect_silent(packageDescription("falsy"))
  expect_equal(packageDescription("falsy")$RemoteUrl, url)

})
