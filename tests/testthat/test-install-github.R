
context("Install from GitHub")

test_that("github_resolve_ref.github_release", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  expect_error(
    github_resolve_ref.github_release(
      NA,
      list(username = "hadley", repo = "devtools")
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
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

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
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

  install_github("cran/falsy", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("falsy", lib.loc = lib))
  expect_equal(
    packageDescription("falsy", lib.loc = lib)$RemoteRepo,
    "falsy")
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
    "Unknown username"
  )

  expect_warning(
    install_github("falsy", username = "cran", lib = lib, quiet = TRUE),
    "Username parameter is deprecated"
  )
})

test_that("remote_download.github_remote", {

  mockery::stub(remote_download.github_remote, "github_has_submodules", TRUE)
  mockery::stub(remote_download.github_remote, "download", TRUE)
  expect_warning(
    remote_download.github_remote(
      list(
        host = "api.github.com",
        username = "cran",
        repo = "falsy",
        ref = "master"
      )
    ),
    "GitHub repo contains submodules"
  )
})

test_that("remote_download.github_remote messages", {

  mockery::stub(remote_download.github_remote, "github_has_submodules", FALSE)
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

test_that("github_has_submodules with broken libcurl", {

  mockery::stub(
    github_has_submodules,
    "download",
    function(path, ...) {
      cat('{ "message": "Not Found",',
          '  "documentation_url": "https://developer.github.com/v3"',
          '}', file = path)
    }
  )
  expect_false(
    github_has_submodules(
      list(
        host = "api.github.com",
        username = "cran",
        repo = "falsy",
        ref = "master"
        )
    )
  )
})

test_that("remote_metadata.github_remote", {

  expect_equal(
    remote_metadata.github_remote(list(sha = "foobar"))$RemoteSha,
    "foobar"
  )

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  expect_equal(
    remote_metadata.github_remote(
      list(
        username = "cran",
        repo = "falsy",
        ref = "1.0"
      )
    )$RemoteSha,
    "0f39d9eb735bf16909831c0bb129063dda388375"
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
  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)

  install_github(
    "MangoTheCat/pkgsnap",
    ref = github_pull(10),
    lib = lib,
    quiet = TRUE
  )

  expect_silent(packageDescription("pkgsnap", lib.loc = lib))
  expect_equal(
    packageDescription("pkgsnap", lib.loc = lib)$RemoteRepo,
    "pkgsnap")
})

test_that("remote_sha.github_remote errors if remote doesn't exist", {
  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  expect_error(remote_sha(github_remote("arst/arst")), "cannot open URL")
})

test_that("remote_sha.github_remote returns expected value if remote does exist", {
  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  expect_equal(remote_sha(github_remote("r-lib/devtools@v1.8.0")), "ad9aac7b9a522354e1ff363a86f389e32cec181b")
})
