test_that("install_svn subdir", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  mockery::stub(
    install_svn,
    "install_remotes",
    function(remotes, ...) remotes)

  rem <- install_svn(
    "https://github.com/dmlc/xgboost/trunk",
    subdir = "R-package"
  )

  expect_equal(rem[[1]]$svn_subdir, "R-package")
})

test_that("remote_download.svn_remote error", {

  skip_on_cran()

  x <- list(url = "http://foo.bar.com")

  mockery::stub(remote_download.svn_remote, "system2", 1)
  expect_error(
    remote_download.svn_remote(x),
    "There seems to be a problem retrieving"
  )
})

test_that("downloading a wrong SVN revision", {

  skip_on_cran()
  skip_if_offline()

  x <- list(
    url = "https://github.com/mangothecat/simplegraph/trunk",
    revision = "xxx"
  )

  expect_error(
    remote_download.svn_remote(x)
  )
})


test_that("svn_path", {

  tmp <- tempfile()
  expect_error(
    svn_path(tmp),
    "does not exist"
  )

  cat("Hello", file = tmp)
  expect_equal(svn_path(tmp), tmp)

  mockery::stub(svn_path, "Sys.which", "")
  mockery::stub(svn_path, "os_type", "windows")
  mockery::stub(svn_path, "file.exists", FALSE)
  expect_error(
    svn_path(),
    "SVN does not seem to be installed on your system"
  )
})
