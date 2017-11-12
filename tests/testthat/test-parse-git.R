context("Parse git repo")

test_that("pull request and latest release, via spec and URL", {

  expect_equal(
    parse_git_repo("r-lib/remotes#7")$ref,
    github_pull("7")
  )
  expect_equal(
    parse_git_repo("https://github.com/r-lib/remotes/pull/7")$ref,
    github_pull("7")
  )

  expect_equal(
    parse_git_repo("r-lib/remotes@*release")$ref,
    github_release()
  )
  expect_equal(
    parse_git_repo("https://github.com/r-lib/remotes/releases/latest")$ref,
    github_release()
  )
})

test_that("parse_github_repo_spec trailing slash, issue #54", {
  expect_equal(
    parse_github_repo_spec("foo/bar/baz/"),
    parse_github_repo_spec("foo/bar/baz")
  )
})

test_that("parse_github_url() accepts all forms of URL (github.com and GHE)", {
  ## HTTPS
  expect_identical(
    parse_github_url("https://github.com/r-lib/remotes.git"),
    list(username = "r-lib", repo = "remotes", ref = "", pull = "", release = "")
  )
  expect_identical(
    parse_github_url("https://github.ubc.ca/user/repo.git"),
    list(username = "user", repo = "repo", ref = "", pull = "", release = "")
  )

  ## SSH
  expect_identical(
    parse_github_url("git@github.com:r-lib/remotes.git"),
    list(username = "r-lib", repo = "remotes", ref = "", pull = "", release = "")
  )
  expect_identical(
    parse_github_url("git@github.ubc.ca:user/repo.git"),
    list(username = "user", repo = "repo", ref = "", pull = "", release = "")
  )

  ## browser URLs
  expect_identical(
    parse_github_url("https://github.com/r-lib/remotes"),
    list(username = "r-lib", repo = "remotes", ref = "", pull = "", release = "")
  )
  expect_identical(
    parse_github_url("https://github.ubc.ca/user/repo"),
    list(username = "user", repo = "repo", ref = "", pull = "", release = "")
  )

  expect_identical(
    parse_github_url("https://github.com/r-lib/remotes/tree/i-am-a-branch"),
    list(username = "r-lib", repo = "remotes", ref = "i-am-a-branch", pull = "", release = "")
  )
  expect_identical(
    parse_github_url("https://github.com/r-lib/remotes/commit/1234567"),
    list(username = "r-lib", repo = "remotes", ref = "1234567", pull = "", release = "")
  )
  expect_identical(
    parse_github_url("https://github.com/r-lib/remotes/pull/108"),
    list(username = "r-lib", repo = "remotes", ref = "", pull = "108", release = "")
  )
  expect_identical(
    parse_github_url("https://github.com/r-lib/remotes/releases/tag/1.0.0"),
    list(username = "r-lib", repo = "remotes", ref = "1.0.0", pull = "", release = "")
  )
  expect_identical(
    parse_github_url("https://github.com/r-lib/remotes/releases/latest"),
    list(username = "r-lib", repo = "remotes", ref = "", pull = "", release = "*release")
  )
})

test_that("parse_github_repo_spec catches invalid spec", {
  expect_error(
    parse_github_repo_spec("/$&@R64&3"),
    "Invalid git repo specification"
  )
})
