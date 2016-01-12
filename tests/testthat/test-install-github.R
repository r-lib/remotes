
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

  expect_silent(packageDescription("falsy"))
  expect_equal(packageDescription("falsy")$RemoteRepo, "falsy")

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

test_that("github_has_submodules with broken libcurl", {

  with_mock(
    `remotes::download` = function(path, ...) {
      cat('{ "message": "Not Found",',
          '  "documentation_url": "https://developer.github.com/v3"',
          '}', file = path)
    },
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
    "gaborcsardi/pkgconfig",
    ref = github_pull(7),
    lib = lib,
    quiet = TRUE
  )

  expect_silent(packageDescription("pkgconfig"))
  expect_equal(packageDescription("pkgconfig")$RemoteRepo, "pkgconfig")

})

test_that("pull request, release, alternative notation", {

  expect_equal(
    parse_git_repo("gaborcsardi/pkgconfig#7")$ref,
    github_pull("7")
  )

  expect_equal(
    parse_git_repo("gaborcsardi/pkgconfig@*release")$ref,
    github_release()
  )

})

test_that("type = 'both' works well", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  expect_equal(
    package_deps("falsy", type = "both"),
    package_deps("falsy", type = "binary")
  )

})

test_that("update_packages", {

  object <- package_deps("dotenv")
  object$diff[object$package == "falsy"] <- 2L
  object$diff[object$package == "magrittr"] <- 0L

  with_mock(
    `remotes::install_packages` = function(...) { },
    expect_message(
      update_packages(object, quiet = FALSE),
      "Skipping 1 packages? not available: falsy"
    )
  )

  object$diff[object$package == "falsy"] <- 1L

  with_mock(
    `remotes::install_packages` = function(...) { },
    expect_message(
      update_packages(object, quiet = FALSE),
      "Skipping 1 packages? ahead of CRAN: falsy"
    )
  )

  object$diff[object$package == "falsy"] <- 0L
  object$installed[object$package == "magrittr"] <- NA_integer_

  with_mock(
    `remotes::install_packages` = function(packages, ...) packages,
    expect_equal(
      update_packages(object, upgrade = FALSE),
      "magrittr"
    )
  )

})
