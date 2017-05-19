
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

  repos <- getOption("repos")
  if (length(repos) == 0) repos <- character()
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com"

  pkg <- download.packages("pkgconfig", dir, repos = repos, type = "source")

  install_local(pkg[, 2], lib = lib)

  expect_silent(packageDescription("pkgconfig"))
  expect_equal(packageDescription("pkgconfig")$RemoteType, "local")

})
