
context("Download")

test_that("download_method", {

  with_mock(
    `base::capabilities` = function(...) c(libcurl = TRUE),
    expect_equal(download_method(), "libcurl")
  )

  with_mock(
    `base::capabilities` = function(...) c(libcurl = FALSE),
    `remotes::os_type` = function() "windows",
    expect_equal(download_method(), "wininet")
  )

  with_mock(
    `base::capabilities` = function(...) c(libcurl = FALSE),
    `remotes::os_type` = function() "unix",
    expect_equal(download_method(), "auto")
  )

})

test_that("download", {

  skip_on_cran()
  skip_if_offline()

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  download(tmp, "http://httpbin.org/get", auth_token = NULL)
  res <- fromJSONFile(tmp)
  expect_true("args" %in% names(res))

  download(tmp, "http://httpbin.org/get", auth_token = "foobar")
  res <- fromJSONFile(tmp)
  expect_equal(res$args$access_token, "foobar")

})
