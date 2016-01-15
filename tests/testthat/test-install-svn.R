
context("Install from SVN repositories")

test_that("install_svn", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

  install_svn("https://github.com/mangothecat/simplegraph")

  expect_silent(packageDescription("simplegraph"))
  expect_equal(packageDescription("simplegraph")$RemoteType, "svn")

})
