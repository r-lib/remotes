
context("Download")

test_that("download_method", {

  mockery::stub(download_method, "get_r_version", "3.3.0")
  expect_equal(download_method(), "auto")

  mockery::stub(download_method, "get_r_version", "3.2.5")
  mockery::stub(download_method, "os_type", "windows")
  expect_equal(download_method(), "wininet")

  mockery::stub(download_method, "get_r_version", "3.2.5")
  mockery::stub(download_method, "os_type", "unix")
  mockery::stub(download_method, "capabilities", c(libcurl = TRUE))
  expect_equal(download_method(), "libcurl")

  mockery::stub(download_method, "get_r_version", "3.2.5")
  mockery::stub(download_method, "os_type", "unix")
  mockery::stub(download_method, "capabilities", c(libcurl = FALSE))
  expect_equal(download_method(), "auto")
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

  mockery::stub(
    download,
    "base_download",
    function(url, ...) { print(url); 0 })

  expect_output(
    download(
      tempfile(),
      "http://foo.bar.com",
      basic_auth = list(user = "user", password = "password")
    ),
    "http://user:password@foo.bar.com"
  )
})

test_that("download fallback to curl, https", {

  skip_on_cran()
  skip_if_offline()

  mockery::stub(download, "get_r_version", "3.0.0")
  download(tmp <- tempfile(), "https://httpbin.org/ip")
  expect_match(paste(readLines(tmp), collapse = "\n"), "origin")
})

test_that("download with curl, basic auth", {

  skip_on_cran()
  skip_if_offline()

  mockery::stub(download, "get_r_version", "3.0.0")
  download(
    tmp <- tempfile(),
    "http://httpbin.org/basic-auth/user/passwd",
    basic_auth = list(user = "user", password = "passwd")
  )
  expect_match(
    paste(readLines(tmp), collapse = "\n"),
    '"authenticated": true'
  )
})
