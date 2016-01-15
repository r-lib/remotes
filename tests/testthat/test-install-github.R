
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

## -2 = not installed, but available on CRAN
## -1 = installed, but out of date
##  0 = installed, most recent version
##  1 = installed, version ahead of CRAN
##  2 = package not on CRAN

test_that("update.package_deps", {

  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "magrittr"),
    installed = c("1.0", "1.0", "1.0"),
    available = c("1.0", NA, "1.0"),
    diff = c(0L, 2L, 0L)
  )
  class(object) <- c("package_deps", "data.frame")

  with_mock(
    `remotes::install_packages` = function(...) { },
    expect_message(
      update(object, quiet = FALSE),
      "Skipping 1 packages? not available: falsy"
    )
  )

  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "magrittr"),
    installed = c("1.0", "1.1", "1.0"),
    available = c("1.0", "1.0", "1.0"),
    diff = c(0L, 1L, 0L)
  )
  class(object) <- c("package_deps", "data.frame")

  with_mock(
    `remotes::install_packages` = function(...) { },
    expect_message(
      update(object, quiet = FALSE),
      "Skipping 1 packages? ahead of CRAN: falsy"
    )
  )

  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "magrittr"),
    installed = c("1.0", "1.0", NA),
    available = c("1.0", "1.1", "1.0"),
    diff = c(0L, -1L, -2L)
  )
  class(object) <- c("package_deps", "data.frame")

  with_mock(
    `remotes::install_packages` = function(packages, ...) packages,
    expect_equal(
      update(object, upgrade = FALSE),
      "magrittr"
    )
  )

})
