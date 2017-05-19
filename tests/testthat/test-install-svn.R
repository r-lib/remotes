
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

  install_svn("https://github.com/mangothecat/simplegraph/trunk")

  expect_silent(packageDescription("simplegraph"))
  expect_equal(packageDescription("simplegraph")$RemoteType, "svn")

})

test_that("install_svn branch", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

  install_svn(
    "https://github.com/mangothecat/simplegraph",
    subdir = "branches/remotes-test"
  )

  expect_silent(packageDescription("simplegraph"))
  expect_equal(packageDescription("simplegraph")$RemoteType, "svn")

})

test_that("install_svn subdir", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

  with_mock(
    `remotes::install` = function(dir, ...) { print(list.files(dir)); TRUE },
    expect_output(
      install_svn(
        "https://github.com/dmlc/xgboost/trunk",
        subdir = "R-package"
      ),
      "DESCRIPTION"
    )
  )
})

test_that("remote_download.svn_remote error", {

  x <- list(url = "http://foo.bar.com")

  with_mock(
    `base::system2` = function(...) { 1 },
    expect_error(
      remote_download.svn_remote(x),
      "There seems to be a problem retrieving"
    )
  )
})

test_that("downloading an SVN revision", {

  skip_on_cran()
  skip_if_offline()

  x <- list(
    url = "https://github.com/mangothecat/simplegraph/trunk",
    revision = "r28"
  )

  bundle <- remote_download.svn_remote(x)
  on.exit(unlink(bundle), add = TRUE)

  expect_output(
    print(list.files(bundle)),
    "DESCRIPTION"
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

  with_mock(
    `base::Sys.which` = function(...) "",
    `remotes::os_type` = function() "windows",
    `base::file.exists` = function(...) FALSE,
    expect_error(
      svn_path(),
      "SVN does not seem to be installed on your system"
    )
  )

})
