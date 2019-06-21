
context("Installing from local files")

test_that("install_local", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  dir <- tempfile()
  on.exit(unlink(dir), add = TRUE)
  dir.create(dir)

  repos <- getOption("repos")
  if (length(repos) == 0) repos <- character()
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com"

  pkg <- download.packages("pkgconfig", dir, repos = repos, type = "source", quiet = TRUE)

  install_local(pkg[, 2], lib = lib, quiet = TRUE)

  expect_silent(packageDescription("pkgconfig", lib.loc = lib))
  expect_equal(
    packageDescription("pkgconfig", lib.loc = lib)$RemoteType,
    "local")

  remote <- package2remote("pkgconfig", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "local_remote")
  expect_equal(format(remote), "local")
  expect_equal(remote$path, normalizePath(pkg[, 2]))
  expect_true(!is.na(remote$sha) && nzchar(remote$sha))
})

test_that("local remotes do not fail if the local install no longer exists", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  dir <- tempfile()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir.create(dir)

  repos <- getOption("repos")
  if (length(repos) == 0) repos <- character()
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com"

  pkg <- download.packages("pkgconfig", dir, repos = repos, type = "source", quiet = TRUE)

  install_local(pkg[, 2], lib = lib, quiet = TRUE)

  unlink(dir, recursive = TRUE)

  withr::with_libpaths(lib,
    expect_error(update_packages(pkg[, 1], lib = lib, upgrade = TRUE, quiet = TRUE), NA)
  )
})
