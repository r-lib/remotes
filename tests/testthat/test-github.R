
context("GitHub")

test_that("github_pat", {

  orig <- Sys.getenv("GITHUB_PAT")
  on.exit(Sys.setenv(GITHUB_PAT = orig), add = TRUE)

  Sys.setenv(GITHUB_PAT = "badcafe")
  expect_equal(github_pat(), "badcafe")

  expect_message(github_pat(quiet = FALSE), "Using github PAT from envvar GITHUB_PAT")
})
