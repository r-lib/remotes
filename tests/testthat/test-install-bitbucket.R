
context("Install from Bitbucket")

test_that("", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  install_bitbucket("jimhester/withr", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("withr", lib.loc = lib))
  expect_equal(
    packageDescription("withr", lib.loc = lib)$RemoteRepo,
    "withr")

  remote <- package2remote("withr", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "bitbucket_remote")
  expect_equal(format(remote), "Bitbucket")
  expect_equal(remote$host, "api.bitbucket.org/2.0")
  expect_equal(remote$repo, "withr")
  expect_equal(remote$username, "jimhester")
  expect_equal(remote$ref, "master")
  expect_true(!is.na(remote$sha) && nzchar(remote$sha))
})


test_that("remote_download.bitbucket_remote", {

  x <- list(username = "csardigabor", repo = "pkgconfig", ref = "master",
    host = "api.bitbucket.org/2.0")

  mockery::stub(
    remote_download.bitbucket_remote, "download", function(...) { }
  )

  mockery::stub(
    remote_download.bitbucket_remote, "bitbucket_download_url", function(...) { }
  )

  expect_message(
    remote_download.bitbucket_remote(x),
    "Downloading bitbucket repo csardigabor/pkgconfig@master"
  )
})


test_that("remote_metadata.bitbucket_remote", {

  expect_equal(
    remote_metadata.bitbucket_remote(list(), sha = "foobar")$RemoteSha,
    "foobar"
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

  install_bitbucket("jimhester/falsy", lib = lib, quiet = TRUE)

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
    auth_user = "foo",
    password = "pass",
    host = "api.bitbucket.com/2.0"
  )

  mockery::stub(
    remote_download.bitbucket_remote,
    "download",
    function(dest, src, basic_auth) basic_auth)

  mockery::stub(
    remote_download.bitbucket_remote,
    "bitbucket_download_url",
    function(...) { })

  expect_equal(
    remote_download.bitbucket_remote(x),
    list(user = "foo", password = "pass")
  )
})
