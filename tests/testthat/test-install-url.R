
context("Installing from URLs")

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

  url <- "https://github.com/mangothecat/simplegraph/archive/master.zip"
  install_url(url, lib = lib)

  expect_silent(packageDescription("simplegraph", lib.loc = lib))
  expect_equal(
    packageDescription("simplegraph", lib.loc = lib)$RemoteType,
    "url")
  expect_equal(
    packageDescription("simplegraph", lib.loc = lib)$RemoteUrl,
    url)
})
