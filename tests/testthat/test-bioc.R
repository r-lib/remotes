
context("BioConductor packages")

test_that("installing bioc packages", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  withr::with_libpaths(
    lib,
    install_git("https://git.bioconductor.org/packages/Biobase", lib = lib, quiet = TRUE)
  )

  expect_silent(packageDescription("Biobase", lib.loc = lib))
  expect_equal(
    packageDescription("Biobase", lib.loc = lib)$RemoteUrl,
    "https://git.bioconductor.org/packages/Biobase")

  expect_silent(packageDescription("BiocGenerics", lib.loc = lib))
})
