
context("Install from git repo")

test_that("install_git", {

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

  url <- "https://github.com/gaborcsardi/pkgconfig.git"
  expect_message(
    install_git(url, lib = lib, quiet = FALSE, branch = "travis"),
    "Downloading git repo"
  )

  expect_silent(packageDescription("pkgconfig"))
  expect_equal(packageDescription("pkgconfig")$RemoteUrl, url)

})


test_that("install_git with command line git", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()
  if (is.null(git_path())) skip("git is not installed")

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

  url <- "https://github.com/cran/falsy.git"
  expect_message(
    install_git(url, git = "external", lib = lib, quiet = FALSE),
    "Downloading git repo"
  )

  expect_silent(packageDescription("falsy"))
  expect_equal(packageDescription("falsy")$RemoteUrl, url)

})


test_that("remote_metadata.git2r_remote", {

  r <- remote_metadata.git2r_remote(
    list(url = "foo", subdir = "foo2", ref = "foo3")
  )

  e <- list(
    RemoteType = "git",
    RemoteUrl = "foo",
    RemoteSubdir = "foo2",
    RemoteRef = "foo3",
    RemoteSha = NULL
  )

  expect_equal(r, e)
})

context("Install git submodules")

goldStandard <- list(name = "testRepo",
                     path = "testRepo",
                     url = "http://github.com/user/testRepo",
                     branch = NULL)

gitmodule <- '[submodule "testRepo"]
	path = testRepo
url = http://github.com/user/testRepo'

test_that("parse_module returns null branch when none is given", {
  expect_equal(parse_module(gitmodule), goldStandard)
})

goldStandard <- list(name = "testRepo",
                     path = "testRepo",
                     url = "http://github.com/user/testRepo",
                     branch = "devel")

gitmodule <- '[submodule "testRepo"]
	path = testRepo
url = http://github.com/user/testRepo
branch = devel'

test_that("parse_module returns devel branch when devel is given", {
  expect_equal(parse_module(gitmodule), goldStandard)
})
