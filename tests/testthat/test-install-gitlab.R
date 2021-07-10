context("Install from GitLab")

test_that("install_gitlab", {
  skip_on_cran()
  skip_if_offline()
  withr::local_envvar(c(GITLAB_PAT=""))

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  install_gitlab("jimhester/falsy", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("falsy", lib.loc = lib))
  expect_equal(
    packageDescription("falsy", lib.loc = lib)$RemoteRepo,
    "falsy")

  remote <- package2remote("falsy", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "gitlab_remote")
  expect_equal(format(remote), "GitLab")
  expect_equal(remote$host, "gitlab.com")
  expect_equal(remote$username, "jimhester")
  expect_equal(remote$repo, "falsy")
  expect_equal(remote$ref, "HEAD")
  expect_equal(remote$subdir, NULL)
  expect_true(!is.na(remote$sha) && nzchar(remote$sha))
})

test_that("install_gitlab with subgroups and special characters", {
  skip_on_cran()
  skip_if_offline()
  withr::local_envvar(c(GITLAB_PAT=""))

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  install_gitlab("r-lib-grp/my-awesome-group/test.pkg", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("test123", lib.loc = lib))
  expect_equal(
    packageDescription("test123", lib.loc = lib)$RemoteRepo,
    "my-awesome-group/test.pkg")

  remote <- package2remote("test123", lib = lib)
  expect_equal(remote$username, "r-lib-grp")
  expect_equal(remote$repo, "my-awesome-group/test.pkg")
  expect_equal(remote$subdir, NULL)

  install_gitlab("r-lib-grp/test-pkg", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("test123", lib.loc = lib))
  expect_equal(
    packageDescription("test123", lib.loc = lib)$RemoteRepo,
    "test-pkg")

  remote <- package2remote("test123", lib = lib)
  expect_equal(remote$username, "r-lib-grp")
  expect_equal(remote$repo, "test-pkg")
  expect_equal(remote$subdir, NULL)
})

test_that("error if not username, warning if given as argument", {
  skip_on_cran()
  skip_if_offline()
  withr::local_envvar(c(GITLAB_PAT=""))

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  expect_error(
    install_gitlab("falsy", lib = lib, quiet = TRUE)
  )
})

test_that("remote_download.gitlab_remote messages", {
  skip_on_cran()
  skip_if_offline()
  withr::local_envvar(c(GITLAB_PAT=""))

  mockery::stub(remote_download.gitlab_remote, "download", TRUE)
  expect_message(
    remote_download.gitlab_remote(
      remote("gitlab",
        host = "https://gitlab.com",
        username = "jimhester",
        repo = "falsy",
        ref = "HEAD"
      )
    ),
    "Downloading GitLab repo"
  )
})

test_that("remote_sha.gitlab_remote", {
  skip_on_cran()
  skip_if_offline()
  withr::local_envvar(c(GITLAB_PAT=""))

  expect_equal(
    remote_sha(
      remote("gitlab",
        host = "https://gitlab.com",
        username = "jimhester",
        repo = "falsy",
        ref = "1.0"
      )
    ),
    "0f39d9eb735bf16909831c0bb129063dda388375"
  )

  expect_equal(
    remote_sha(
      remote("gitlab",
        host = "https://gitlab.com",
        username = "drgola",
        repo = "r_pkg_test",
        ref = "feature/foo_mult"
      )
    ),
    "58376e96e9b42baece2ab7c0414db6064740c6b6"
  )

})

test_that("gitlab_project_id", {
  skip_on_cran()
  skip_if_offline()
  withr::local_envvar(c(GITLAB_PAT=""))

  expect_equal(
    gitlab_project_id(
      username = "jimhester",
      repo = "covr",
      host = "https://gitlab.com",
      ref = "HEAD"
    ),
    1486846
  )

})

test_that("gitlab_remote reverts to git2r_remote when git_fallback with git2r", {
  skip_if_not_installed("git2r")
  withr::local_envvar(c(GITLAB_PAT="badcafe"))
  mockery::stub(gitlab_remote, "pkg_installed", TRUE, 2L)  # assume git2r available

  expect_message({
    r <- gitlab_remote(
      "fakenamespace/namespace/repo",
      git_fallback = TRUE
    )
  }, "auth_token does not")
  expect_s3_class(r, "git2r_remote")
  expect_equal(r$credentials$username, "gitlab-ci-token")
  expect_equal(r$credentials$password, "badcafe")

  withr::local_envvar(c(GITLAB_PAT=""))
  expect_message({
    r <- gitlab_remote(
      "fakenamespace/namespace/repo",
      git_fallback = TRUE
    )
  }, "Unable to establish api access")
  expect_s3_class(r, "git2r_remote")
  expect_equal(r$credentials, NULL)

  r <- gitlab_remote("fakenamespace/namespace/repo", git_fallback = FALSE)
  expect_s3_class(r, "gitlab_remote")
})

test_that("gitlab_remote reverts to xgit_remote when git_fallback and no git2r", {
  withr::local_envvar(c(GITLAB_PAT=""))
  mockery::stub(gitlab_remote, "pkg_installed", FALSE, 2L)  # assume git2r unavailable
  
  expect_message({
    r <- gitlab_remote(
      "fakenamespace/namespace/repo",
      git_fallback = TRUE
    )
  }, "Unable to establish api access")
  expect_s3_class(r, "xgit_remote")
  expect_equal(r$url, "https://gitlab.com/fakenamespace/namespace/repo.git")

  withr::local_envvar(c(GITLAB_PAT="badcafe"))

  expect_message({
    r <- gitlab_remote(
      "fakenamespace/namespace/repo",
      git_fallback = TRUE
    ) 
  }, "auth_token does not")
  expect_s3_class(r, "xgit_remote")
  expect_equal(r$url, "https://gitlab-ci-token:badcafe@gitlab.com/fakenamespace/namespace/repo.git")

  expect_message({
    r <- gitlab_remote(
      "fakenamespace/namespace/repo",
      auth_token = "goodcafe",
      host = "github.com",
      git_fallback = TRUE
    )
  }, "Attempting git_remote.*<auth_token>@")
  expect_s3_class(r, "xgit_remote")
  expect_equal(r$url, "https://gitlab-ci-token:goodcafe@github.com/fakenamespace/namespace/repo.git")
})

