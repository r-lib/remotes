test_that("git_extract_sha1_tar", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  sha <- "fbae60ced0afee0e7c0f8dc3b5b1bb48d303f3dd"
  url <- build_url(
    "api.github.com/repos/hadley/devtools/tarball",
    sha
  )

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  download(tmp, url, auth_token = github_pat())

  expect_equal(
    git_extract_sha1_tar(tmp),
    sha
  )
})


test_that("git not quiet", {

  local_mocked_bindings(
    check_git_path = function(...) "/foo/git",
    .package = "remotes"
  )

  local_mocked_bindings(
    system = function(...) "0",
    .package = "base"
  )

  expect_message(
    git(args = c("arg1", "arg2"), quiet = FALSE),
    "['\"]/foo/git['\"] arg1arg2"
  )
})


test_that("git error", {

  local_mocked_bindings(
    check_git_path = function(...) "/foo/git",
    .package = "remotes"
  )

  local_mocked_bindings(
    system = function(...) structure("foo", status = "1"),
    .package = "base"
  )
  expect_error(git(args = "arg"), "Command failed")
})

test_that("git_path", {

  tmp <- tempfile()
  expect_error(
    git_path(tmp),
    "does not exist"
  )

  cat("Hello", file = tmp)
  expect_equal(git_path(tmp), tmp)

  local_mocked_bindings(
    Sys.which = function(...) "",
    .package = "base"
  )
  local_mocked_bindings(
    os_type = function(...) "windows",
    .package = "remotes"
  )
  local_mocked_bindings(
    file.exists = function(...) FALSE,
    .package = "base"
  )

  expect_null(git_path())
})


test_that("check_git_path", {

  local_mocked_bindings(
    git_path = function(...) NULL,
    .package = "remotes"
  )
  expect_error(
    check_git_path(),
    "Git does not seem to be installed on your system"
  )
})
