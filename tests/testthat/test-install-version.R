
context("Install a specific version from CRAN")

test_that("install_version", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

  repos <- getOption("repos")
  if (length(repos) == 0) repos <- character()
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com"

  install_version("pkgconfig", "1.0.0", lib = lib, repos = repos)

  expect_silent(packageDescription("pkgconfig"))
  expect_equal(packageDescription("pkgconfig")$Version, "1.0.0")

})

test_that("package_find_repo() works correctly with multiple repos", {

  skip_on_cran()
  skip_if_offline()

  repos <- c(CRANextras = "http://www.stats.ox.ac.uk/pub/RWin", CRAN = "http://cran.rstudio.com")
  # ROI.plugin.glpk is the smallest package in the CRAN archive
  package <- "ROI.plugin.glpk"
  res <- package_find_repo(package, repos = repos)

  expect_equal(NROW(res), 1L)
  expect_equal(res$repo, "http://cran.rstudio.com")
  expect_match(rownames(res), package)
})
