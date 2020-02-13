
context("Download")

test_that("download_method", {

  mockery::stub(download_method, "get_r_version", "3.3.0")
  mockery::stub(download_method, "has_curl", FALSE)
  with_options(list(download.file.method = NULL),
               expect_equal(download_method(), "auto"))

  mockery::stub(download_method, "get_r_version", "3.2.5")
  mockery::stub(download_method, "os_type", "windows")
  with_options(list(download.file.method = NULL),
               expect_equal(download_method(), "wininet"))

  mockery::stub(download_method, "get_r_version", "3.2.5")
  mockery::stub(download_method, "os_type", "unix")
  mockery::stub(download_method, "has_curl", TRUE)
  with_options(list(download.file.method = NULL),
               expect_equal(download_method(), "libcurl"))

  mockery::stub(download_method, "get_r_version", "3.2.5")
  mockery::stub(download_method, "os_type", "unix")
  mockery::stub(download_method, "has_curl", FALSE)
  with_options(list(download.file.method = NULL),
               expect_equal(download_method(), "auto"))
})

test_that("download", {

  skip_on_cran()
  skip_if_offline()

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  download(tmp, "http://httpbin.org/get", auth_token = NULL)
  res <- json$parse_file(tmp)
  expect_null(res$headers$Authorization)

  download(tmp, "http://httpbin.org/get", auth_token = "foobar")
  res <- json$parse_file(tmp)
  expect_equal(res$headers$Authorization, "token foobar")

})

test_that("os_type", {

  expect_equal(os_type(), .Platform$OS.type)
})

test_that("download fallback to curl, https", {

  skip_on_cran()
  skip_if_offline()
  skip_if(is_standalone())

  mockery::stub(download, "get_r_version", "3.0.0")
  download(tmp <- tempfile(), "https://httpbin.org/ip")
  expect_match(paste(readLines(tmp), collapse = "\n"), "origin")
})

test_that("download with curl, basic auth", {

  skip_on_cran()
  skip_if_offline()
  skip_if(is_standalone())

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

test_that("base download with custom headers", {
  skip_on_cran()
  skip_if_offline()

  url <- "http://httpbin.org/anything"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  base_download(url, path = tmp, quiet = TRUE, headers = head)
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
})

test_that("wget method download with custom headers", {
  skip_on_cran()
  skip_if_offline()
  skip_without_program("wget")

  url <- "http://httpbin.org/anything"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  extra <- "--header=\"X-Another: extra-header\""
  with_options(
    list(download.file.method = "wget", download.file.extra = extra),
    base_download(url, path = tmp, quiet = TRUE, headers = head))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
  expect_equal(resp$headers$`X-Another`, "extra-header")
})

test_that("curl method download with custom headers", {
  skip_on_cran()
  skip_if_offline()
  skip_without_program("curl")

  url <- "http://httpbin.org/anything"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  extra <- "-H \"X-Another: extra-header\""
  with_options(
    list(download.file.method = "curl", download.file.extra = extra),
    base_download(url, path = tmp, quiet = TRUE, headers = head))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
  expect_equal(resp$headers$`X-Another`, "extra-header")
})

test_that("internal method download with custom headers", {
  skip_on_cran()
  skip_if_offline()

  url <- "http://httpbin.org/anything"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  with_options(
    list(download.file.method = "internal"),
    base_download(url, path = tmp, quiet = TRUE, headers = head))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
})

test_that("wininet method download with custom headers", {
  skip_on_cran()
  skip_if_offline()
  if (os_type() == "unix") return(expect_true(TRUE))
  if (getRversion() < "3.6.0") return(expect_true(TRUE))

  url <- "http://httpbin.org/anything"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  with_options(
    list(download.file.method = "wininet"),
    base_download(url, path = tmp, quiet = TRUE, headers = head))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
})

test_that("curl download with custom headers", {
  skip_on_cran()
  skip_if_offline()

  url <- "https://httpbin.org/anything"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  curl_download(url, path = tmp, quiet = TRUE, headers = head)
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
})

test_that("base download with basic auth", {
  skip_on_cran()
  skip_if_offline()

  url <- "http://httpbin.org/basic-auth/ruser/rpass"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  download(url, path = tmp, quiet = TRUE,
           basic_auth = list(user = "ruser", password = "rpass"))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("base wget download with basic auth", {
  skip_on_cran()
  skip_if_offline()
  skip_without_program("wget")

  url <- "http://httpbin.org/basic-auth/ruser/rpass"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  with_options(
    list(download.file.method = "wget"),
         download(url, path = tmp, quiet = TRUE,
                  basic_auth = list(user = "ruser", password = "rpass")))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("base curl download with basic auth", {
  skip_on_cran()
  skip_if_offline()
  skip_without_program("curl")

  url <- "http://httpbin.org/basic-auth/ruser/rpass"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  with_options(
    list(download.file.method = "curl"),
         download(url, path = tmp, quiet = TRUE,
                  basic_auth = list(user = "ruser", password = "rpass")))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("base internal download with basic auth", {
  skip_on_cran()
  skip_if_offline()

  url <- "http://httpbin.org/basic-auth/ruser/rpass"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  with_options(
    list(download.file.method = "internal"),
         download(url, path = tmp, quiet = TRUE,
                  basic_auth = list(user = "ruser", password = "rpass")))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("base wininet download with basic auth", {
  skip_on_cran()
  skip_if_offline()
  if (os_type() == "unix") return(expect_true(TRUE))
  if (getRversion() < "3.6.0") return(expect_true(TRUE))

  url <- "http://httpbin.org/basic-auth/ruser/rpass"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  with_options(
    list(download.file.method = "wininet"),
         download(url, path = tmp, quiet = TRUE,
                  basic_auth = list(user = "ruser", password = "rpass")))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("curl download with basic auth", {
  skip_on_cran()
  skip_if_offline()

  mockery::stub(download, "get_r_version", "3.0.0")

  url <- "https://httpbin.org/basic-auth/ruser/rpass"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  download(url, path = tmp, quiet = TRUE,
           basic_auth = list(user = "ruser", password = "rpass"))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("base curl download redirects", {
  skip_on_cran()
  skip_if_offline()
  skip_without_program("curl")

  url <- "http://httpbin.org/absolute-redirect/1"
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  with_options(
    list(download.file.method = "curl"),
         download(url, path = tmp, quiet = TRUE)
  )
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp))
  expect_equal(resp$url, "http://httpbin.org/get")
})
