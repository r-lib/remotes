
context("Install from Bitbucket")

test_that("", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

  install_bitbucket("csardigabor/showimage", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("showimage", lib.loc = lib))
  expect_equal(
    packageDescription("showimage", lib.loc = lib)$RemoteRepo,
    "showimage")
})


test_that("remote_download.bitbucket_remote", {

  x <- list(username = "csardigabor", repo = "pkgconfig", ref = "master")

  mockery::stub(
    remote_download.bitbucket_remote, "download", function(...) { })
  expect_message(
    remote_download.bitbucket_remote(x),
    "Downloading bitbucket repo csardigabor/pkgconfig@master"
  )
})


test_that("remote_metadata.bitbucket_remote", {

  expect_equal(
    remote_metadata.bitbucket_remote(list(sha = "foobar"))$RemoteSha,
    "foobar"
  )

  expect_null(
    remote_metadata.bitbucket_remote(list())$RemoteSha
  )
})


test_that("bitbucket passwords", {

  if (Sys.getenv("BITBUCKET_PASSWORD") == "") {
    skip("Need BitBucket credentials")
  }

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

  install_bitbucket(
    "csardigabor/falsy", lib = lib, quiet = TRUE,
    password = Sys.getenv("BITBUCKET_PASSWORD")
  )

  expect_silent(packageDescription("falsy", lib.loc = lib))
  expect_equal(
    packageDescription("falsy", lib.loc = lib)$RemoteRepo,
    "falsy")
})


test_that("more bitbucket password", {

  x <- list(
    username = "username",
    repo = "repo",
    ref = "master",
    auth_user = "user",
    password = "pass"
  )

  mockery::stub(
    remote_download.bitbucket_remote,
    "download",
    function(dest, src, basic_auth) basic_auth)

  expect_equal(
    remote_download.bitbucket_remote(x),
    list(user = "user", password = "pass")
  )
})
