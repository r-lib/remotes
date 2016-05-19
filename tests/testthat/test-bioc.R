
context("BioConductor packages")

test_that("installing bioc packages", {

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

  install_github("Bioconductor-mirror/Biobase", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("Biobase"))
  expect_equal(packageDescription("Biobase")$RemoteRepo, "Biobase")

  expect_silent(packageDescription("BiocGenerics"))

})
