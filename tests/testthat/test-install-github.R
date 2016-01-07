
context("Install from GitHub")

test_that("install_github", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)
  install_github("cran/falsy", lib = lib, quiet = TRUE)

  libpath <- .libPaths()
  on.exit(.libPaths(libpath), add = TRUE)
  .libPaths(lib)
  expect_silent(packageDescription("falsy"))
  expect_equal(packageDescription("falsy")$RemoteRepo, "falsy")

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

  with_mock(
    `remotes::github_has_submodules` = function(...) TRUE,
    `remotes::download` = function(...) TRUE,
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
  )
})

test_that("remote_download.github_remote messages", {

  with_mock(
    `remotes::github_has_submodules` = function(...) FALSE,
    `remotes::download` = function(...) TRUE,
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
  )
})
