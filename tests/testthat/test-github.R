
context("GitHub")

test_that("github_pat", {
  withr::local_envvar(c(GITHUB_PAT="badcafe"))

  expect_equal(github_pat(), "badcafe")
  expect_message(github_pat(quiet = FALSE), "Using github PAT from envvar GITHUB_PAT")

  withr::with_envvar(c(GITHUB_PAT=NA, CI=NA), {
     expect_equal(github_pat(), NULL)
  })

  withr::with_envvar(c(GITHUB_PAT=NA, CI="true"), {
    expect_true(nzchar(github_pat()))
  })
  expect_true(nzchar(github_pat()))
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

test_that("github_error", {
  mockery::stub(
    github_error,
    "curl::parse_headers_list",
    list(`x-ratelimit-remaining` = 0, `x-ratelimit-limit` = 5000, `x-ratelimit-reset` = "1539962178"))

  # Test without the TRAVIS envvar set
  withr::with_envvar(c(TRAVIS = NA), {
    err <- github_error(list(headers = "", status_code = "304", content = charToRaw('{"message": "foobar"}')))
    expect_known_output(conditionMessage(err), test_path("github-error-local.txt"), print = TRUE)
  })

  # Test with the TRAVIS envvar set
  withr::with_envvar(c(TRAVIS = "true"), {
    err <- github_error(list(headers = "", status_code = "304", content = charToRaw('{"message": "foobar"}')))
    expect_known_output(conditionMessage(err), test_path("github-error-travis.txt"), print = TRUE)
  })

})
