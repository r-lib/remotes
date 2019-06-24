
context("Install from GitHub")

test_that("github_resolve_ref.github_release", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  expect_error(
    github_resolve_ref.github_release(
      NA,
      list(username = "hadley", repo = "devtools"),
      host = "api.github.com"
    ),
    NA
  )
})

test_that("github_resolve_ref.NULL", {

  expect_equal(
    github_resolve_ref(NULL, list()),
    list(ref = "master")
  )
})

test_that("github_resolve_ref.github_pull", {

  expect_error(
    github_resolve_ref(
      github_pull("1"),
      list(userame = "gaborcsardi", repo = "pkgconfig")
    ),
    "Cannot find GitHub pull request"
  )
})

test_that("github_resolve_ref.github_release", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  expect_error(
    github_resolve_ref(
      github_release(),
      list(userame = "gaborcsardi", repo = "xxxxxxxxxx")
    ),
    "Cannot find repo"
  )

})

test_that("github_release", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  install_github(
    "gaborcsardi/falsy",
    ref = github_release(),
    lib = lib,
    quiet = TRUE
  )

  expect_silent(packageDescription("falsy", lib.loc = lib))
  expect_equal(
    packageDescription("falsy", lib.loc = lib)$RemoteRepo,
    "falsy")
})

test_that("install_github", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  install_github("cran/falsy", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("falsy", lib.loc = lib))
  expect_equal(
    packageDescription("falsy", lib.loc = lib)$RemoteRepo,
    "falsy")

  remote <- package2remote("falsy", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "github_remote")
  expect_equal(format(remote), "GitHub")
  expect_equal(remote$host, "api.github.com")
  expect_equal(remote$username, "cran")
  expect_equal(remote$repo, "falsy")
  expect_equal(remote$ref, "master")
  expect_equal(remote$subdir, NULL)
  expect_true(!is.na(remote$sha) && nzchar(remote$sha))
})

test_that("error if not username, warning if given as argument", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  expect_error(
    install_github("falsy", lib = lib, quiet = TRUE),
    "Invalid git repo specification"
  )
})

test_that("remote_download.github_remote messages", {

  mockery::stub(remote_download.github_remote, "download", TRUE)
  expect_message(
    remote_download.github_remote(
      list(
        host = "api.github.com",
        username = "cran",
        repo = "falsy",
        ref = "master"
      )
    ),
    "Downloading GitHub repo"
  )
})

test_that("remote_metadata.github_remote", {

  expect_equal(
    remote_metadata.github_remote(list(), sha = "foobar")$RemoteSha,
    "foobar"
  )
})


test_that("remote_sha.github_remote", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  expect_equal(
    remote_sha.github_remote(
      list(
        username = "cran",
        repo = "falsy",
        ref = "1.0",
        host = "api.github.com"
      )
    ),
    "0f39d9eb735bf16909831c0bb129063dda388375"
  )
})

test_that("github_remote with deleted branch", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  # skip this test unless we are using curl
  skip_if(is_standalone() || !pkg_installed("curl"))


  expect_equal(
    remote_sha.github_remote(
      list(
        username = "tidyverse",
        repo = "purrr",
        ref = "rc-0.3.1",
        host = "api.github.com"
      )
    ),
    NA_character_
  )
})

test_that("github_pull", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  install_github(
    "r-lib/desc",
    ref = github_pull(64),
    lib = lib,
    quiet = TRUE
  )

  expect_silent(packageDescription("desc", lib.loc = lib))
  expect_equal(
    packageDescription("desc", lib.loc = lib)$RemoteRepo,
    "desc")
})

test_that("remote_sha.github_remote errors if remote doesn't exist", {
  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  expect_error(remote_sha(github_remote("arst/arst")))
})

test_that("remote_sha.github_remote returns expected value if remote does exist", {
  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  expect_equal(remote_sha(github_remote("r-lib/devtools@v1.8.0")), "ad9aac7b9a522354e1ff363a86f389e32cec181b")
})
