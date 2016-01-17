
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

test_that("os_type", {

  expect_equal(os_type(), .Platform$OS.type)
})

test_that("download basic auth", {

  with_mock(
    `utils::download.file` = function(url, ...) { print(url); 0 },
    expect_output(
      download(
        tempfile(),
        "http://foo.bar.com",
        basic_auth = list(user = "user", password = "password")
      ),
      "http://user:password@foo.bar.com"
    )
  )
})
