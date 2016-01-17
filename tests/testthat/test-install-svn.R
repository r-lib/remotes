
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
    branch = "remotes-test"
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
        "https://github.com/dmlc/xgboost",
        subdir = "R-package"
      ),
      "DESCRIPTION"
    )
  )
})

