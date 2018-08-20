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

test_that("parse_repo_spec trailing slash, issue #54", {
  expect_equal(
    parse_repo_spec("foo/bar/baz/"),
    parse_repo_spec("foo/bar/baz")
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

test_that("parse_repo_spec catches invalid spec", {
  expect_error(
    parse_repo_spec("/$&@R64&3"),
    "Invalid git repo specification"
  )
})

test_that("parse_repo_spec, github", {

  cases <- list(
    list("user/repo"),
    list("pkg=user/repo", package = "pkg"),
    list("pkg=user/repo", package = "pkg"),
    list("user/repo/subdir", subdir = "subdir"),
    list("user/repo@badcafe", ref = "badcafe"),
    list("user/repo#123", ref = github_pull("123")),
    list("user/repo@*release", ref = github_release()),
    list("pkg=user/repo/subdir", package = "pkg", subdir = "subdir"),
    list("pkg=user/repo@badcafe", package = "pkg", ref = "badcafe"),
    list("pkg=user/repo#123", package = "pkg", ref = github_pull("123")),
    list("pkg=user/repo@*release", package = "pkg", ref = github_release()),

    # github url cases
    list("git@github.com:user/repo.git"),
    list("git@github.ubc.ca:user/repo.git"),
    list("https://github.com/user/repo"),
    list("https://github.ubc.ca/user/repo"),
    list("https://github.com/user/repo/tree/i-am-a-branch", ref = "i-am-a-branch"),
    list("https://github.com/user/repo/commit/1234567", ref = "1234567"),
    list("https://github.com/user/repo/pull/108", ref = github_pull("108")),
    list("https://github.com/user/repo/releases/tag/1.0.0", ref = "1.0.0"),
    list("https://github.com/user/repo/releases/latest", ref = github_release()),
    list("https://github.com/user/repo/releases/latest", ref = github_release()),
    list("https://github.com/foo/bar", username = "foo", repo = "bar"),
    list("git@github.com:foo/bar.git", username = "foo", repo = "bar"),

    # Username and repo can have hyphens in them
    list("git@github.com:foo-bar/baz-qux.git", username = "foo-bar", repo = "baz-qux")
  )

  for (case in cases) {
    expect_equal_named_lists(
      p <- parse_git_repo(case[[1]]),
      utils::modifyList(
        list(username = "user", repo = "repo"),
        case[-1]
      )
    )
  }
})

test_that("parse_git_repo errors on invalid GitHub input", {
  expect_error(parse_git_repo("https://github.com/r-lib"), "Invalid GitHub URL")
})
