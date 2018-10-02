
context("GitHub")

test_that("github_pat", {
  withr::local_envvar(GITHUB_PAT="badcafe")

  expect_equal(github_pat(), "badcafe")
  expect_message(github_pat(quiet = FALSE), "Using github PAT from envvar GITHUB_PAT")
})

test_that("github_commit", {
  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  # Without curl
  expect_equal(
    github_commit("r-lib", "remotes", "1.0.0", use_curl = FALSE),
    "92e5d5c061f181242cb877e4714bea76d94927da")

  # With curl
  expect_equal(
    github_commit("r-lib", "remotes", "1.0.0", use_curl = TRUE),
    "92e5d5c061f181242cb877e4714bea76d94927da")

  # With curl and different local sha
  expect_equal(
    github_commit("r-lib", "remotes", "1.0.0", use_curl = TRUE, current_sha = "xyz"),
    "92e5d5c061f181242cb877e4714bea76d94927da")

  # With curl and same local sha
  expect_equal(
    github_commit("r-lib", "remotes", "1.0.0", use_curl = TRUE, current_sha = "92e5d5c061f181242cb877e4714bea76d94927da"),
    "92e5d5c061f181242cb877e4714bea76d94927da")
})

test_that("github_DESCRIPTION", {
  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  tmp <- tempfile()
  on.exit(unlink(tmp))

  download(tmp, "https://raw.githubusercontent.com/r-lib/remotes/1.0.0/DESCRIPTION")
  desc <- readChar(tmp, file.info(tmp)$size)

  # Without curl
  expect_equal(
    github_DESCRIPTION("r-lib", "remotes", ref = "1.0.0", use_curl = FALSE),
    desc)

  # With curl
  expect_equal(
    github_DESCRIPTION("r-lib", "remotes", ref = "1.0.0", use_curl = TRUE),
    desc)
})
