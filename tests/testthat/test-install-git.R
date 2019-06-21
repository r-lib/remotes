
context("Install from git repo")

test_that("install_git with git2r", {

  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("git2r")

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  url <- "https://github.com/gaborcsardi/pkgconfig.git"
  install_git(url, lib = lib, git = "git2r", quiet = TRUE)

  expect_silent(packageDescription("pkgconfig", lib.loc = lib))
  expect_equal(
    packageDescription("pkgconfig", lib.loc = lib)$RemoteUrl,
    url
  )

  remote <- package2remote("pkgconfig", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "git2r_remote")
  expect_equal(format(remote), "Git")
  expect_equal(remote$url, url)
  expect_equal(remote$ref, NULL)
  expect_equal(remote_sha(remote), remote$sha)
  expect_true(!is.na(remote$sha) && nzchar(remote$sha))
})

test_that("install_git with git2r and ref", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()
  skip_if_not_installed("git2r")

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  url <- "https://github.com/gaborcsardi/pkgconfig.git"
  install_git(url, lib = lib, ref = "travis", git = "git2r", quiet = TRUE)

  expect_silent(packageDescription("pkgconfig", lib.loc = lib))
  expect_equal(
    packageDescription("pkgconfig", lib.loc = lib)$RemoteUrl,
    url
  )

  remote <- package2remote("pkgconfig", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "git2r_remote")
  expect_equal(format(remote), "Git")
  expect_equal(remote$url, url)
  expect_equal(remote$ref, "travis")
  expect_equal(remote_sha(remote), remote$sha)
  expect_true(!is.na(remote$sha) && nzchar(remote$sha))
})


test_that("install_git with command line git", {

  skip_on_cran()
  skip_if_offline()
  if (is.null(git_path())) skip("git is not installed")

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  url <- "https://github.com/cran/falsy.git"
  install_git(url, git = "external", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("falsy", lib.loc = lib))
  expect_equal(packageDescription("falsy", lib.loc = lib)$RemoteUrl, url)

  remote <- package2remote("falsy", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "xgit_remote")
  expect_equal(format(remote), "Git")
  expect_equal(remote$url, url)
  expect_equal(remote$ref, NULL)
  expect_true(!is.na(remote$sha) && nzchar(remote$sha))
})

test_that("install_git with command line git and tag ref", {

  skip_on_cran()
  skip_if_offline()
  if (is.null(git_path())) skip("git is not installed")

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  url <- "https://github.com/cran/falsy.git"
  install_git(url, ref = "1.0", git = "external", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("falsy", lib.loc = lib))
  expect_equal(packageDescription("falsy", lib.loc = lib)$RemoteUrl, url)

  remote <- package2remote("falsy", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "xgit_remote")
  expect_equal(format(remote), "Git")
  expect_equal(remote$url, url)
  expect_equal(remote$ref, "1.0")
  expect_true(!is.na(remote$sha) && nzchar(remote$sha))
})

test_that("install_git with command line git and full SHA ref", {

  skip_on_cran()
  skip_if_offline()
  if (is.null(git_path())) skip("git is not installed")

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  url <- "https://github.com/cran/falsy.git"
  install_git(url, ref = "0f39d9eb735bf16909831c0bb129063dda388375", git = "external", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("falsy", lib.loc = lib))
  expect_equal(packageDescription("falsy", lib.loc = lib)$RemoteUrl, url)

  remote <- package2remote("falsy", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "xgit_remote")
  expect_equal(format(remote), "Git")
  expect_equal(remote$url, url)
  expect_equal(remote$ref, "0f39d9eb735bf16909831c0bb129063dda388375")
  expect_true(!is.na(remote$sha) && nzchar(remote$sha))
})

test_that("remote_sha.xgit remote returns the SHA if it exists", {
  skip_on_cran()
  skip_if_offline()
  if (is.null(git_path())) skip("git is not installed")

  url <- "https://github.com/cran/falsy.git"

  # works with tags
  remote <- git_remote(url, ref = "1.0", git = "external")
  expect_equal(remote_sha(remote), "0f39d9eb735bf16909831c0bb129063dda388375")

  # works with full SHAs
  remote <- git_remote(url, ref = "26a36cf957a18569e311ef75b6f61f822de945ef", git = "external")
  expect_equal(remote_sha(remote), "26a36cf957a18569e311ef75b6f61f822de945ef")
})

test_that("remote_metadata.xgit_remote", {

  r <- remote_metadata.xgit_remote(
    list(url = "foo", subdir = "foo2", ref = "foo3")
  )

  e <- list(
    RemoteType = "xgit",
    RemoteUrl = "foo",
    RemoteSubdir = "foo2",
    RemoteRef = "foo3",
    RemoteSha = NULL,
    RemoteArgs = NULL
  )

  expect_equal(r, e)
})

test_that("remote_metadata.git2r_remote", {

  r <- remote_metadata.git2r_remote(
    list(url = "foo", subdir = "foo2", ref = "foo3")
  )

  e <- list(
    RemoteType = "git2r",
    RemoteUrl = "foo",
    RemoteSubdir = "foo2",
    RemoteRef = "foo3",
    RemoteSha = NULL
  )

  expect_equal(r, e)
})
