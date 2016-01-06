
context("Git")

test_that("git_extract_sha1", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  sha <- "fbae60ced0afee0e7c0f8dc3b5b1bb48d303f3dd"
  url <- paste0(
    "https://api.github.com/repos/hadley/devtools/zipball/",
    sha
  )

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  download(tmp, url, auth_token = NULL)

  expect_equal(
    git_extract_sha1(tmp),
    sha
  )
})
