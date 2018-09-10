
context("Installing from URLs")

test_that("install_url", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  url <- "https://github.com/mangothecat/simplegraph/archive/master.zip"
  install_url(url, lib = lib, quiet = TRUE)

  expect_silent(packageDescription("simplegraph", lib.loc = lib))
  expect_equal(
    packageDescription("simplegraph", lib.loc = lib)$RemoteType,
    "url")
  expect_equal(
    trim_ws(packageDescription("simplegraph", lib.loc = lib)$RemoteUrl),
    url)

  remote <- package2remote("simplegraph", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "url_remote")
  expect_equal(format(remote), "URL")
  expect_equal(remote$url, url)
  expect_equal(remote$subdir, NULL)
})
