
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

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  download(tmp, httpbin$url("/get"), auth_token = NULL)
  res <- json$parse_file(tmp)
  expect_null(res$headers$Authorization)

  download(tmp, httpbin$url("/get"), auth_token = "foobar")
  res <- json$parse_file(tmp)
  expect_equal(res$headers$Authorization, "token foobar")

})

test_that("os_type", {

  expect_equal(os_type(), .Platform$OS.type)
})

test_that("download fallback to curl, https", {
  skip_if(is_standalone())

  mockery::stub(download, "getRversion", package_version("3.0.0"))
  download(tmp <- tempfile(), httpbin$url("/ip"))
  expect_match(paste(readLines(tmp, warn = FALSE), collapse = "\n"), "origin")
})

test_that("download with curl, basic auth", {
  skip_if(is_standalone())

  mockery::stub(download, "getRversion", package_version("3.0.0"))
  download(
    tmp <- tempfile(),
    httpbin$url("/basic-auth/user/passwd"),
    basic_auth = list(user = "user", password = "passwd")
  )
  expect_match(
    paste(readLines(tmp, warn = FALSE), collapse = "\n"),
    '"authenticated":true'
  )
})

test_that("base download with custom headers", {
  url <- httpbin$url("/anything")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  base_download(url, path = tmp, quiet = TRUE, headers = head)
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
})

test_that("wget method download with custom headers", {
  skip_without_program("wget")

  url <- httpbin$url("/anything")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  extra <- "--header=\"X-Another: extra-header\""
  with_options(
    list(download.file.method = "wget", download.file.extra = extra),
    base_download(url, path = tmp, quiet = TRUE, headers = head))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
  expect_equal(resp$headers$`X-Another`, "extra-header")
})

test_that("curl method download with custom headers", {
  skip_without_program("curl")

  url <- httpbin$url("/anything")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  extra <- "-H \"X-Another: extra-header\""
  with_options(
    list(download.file.method = "curl", download.file.extra = extra),
    base_download(url, path = tmp, quiet = TRUE, headers = head))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
  expect_equal(resp$headers$`X-Another`, "extra-header")
})

test_that("internal method download with custom headers", {
  url <- httpbin$url("/anything")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  with_options(
    list(download.file.method = "internal"),
    base_download(url, path = tmp, quiet = TRUE, headers = head))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
})

test_that("wininet method download with custom headers", {
  if (os_type() == "unix") return(expect_true(TRUE))
  if (getRversion() < "3.6.0") return(expect_true(TRUE))

  url <- httpbin$url("/anything")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  with_options(
    list(download.file.method = "wininet"),
    base_download(url, path = tmp, quiet = TRUE, headers = head))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
})

test_that("curl download with custom headers", {
  url <- httpbin$url("/anything")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  head <- c("X-Custom" = "Foobar")
  curl_download(url, path = tmp, quiet = TRUE, headers = head)
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_equal(resp$headers$`X-Custom`, "Foobar")
})

test_that("base download with basic auth", {
  url <- httpbin$url("/basic-auth/ruser/rpass")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  download(url, path = tmp, quiet = TRUE,
           basic_auth = list(user = "ruser", password = "rpass"))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("base wget download with basic auth", {
  skip_without_program("wget")

  url <- httpbin$url("/basic-auth/ruser/rpass")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  with_options(
    list(download.file.method = "wget"),
         download(url, path = tmp, quiet = TRUE,
                  basic_auth = list(user = "ruser", password = "rpass")))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("base curl download with basic auth", {
  skip_without_program("curl")

  url <- httpbin$url("/basic-auth/ruser/rpass")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  with_options(
    list(download.file.method = "curl"),
         download(url, path = tmp, quiet = TRUE,
                  basic_auth = list(user = "ruser", password = "rpass")))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("base internal download with basic auth", {
  url <- httpbin$url("/basic-auth/ruser/rpass")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  with_options(
    list(download.file.method = "internal"),
         download(url, path = tmp, quiet = TRUE,
                  basic_auth = list(user = "ruser", password = "rpass")))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("base wininet download with basic auth", {
  if (os_type() == "unix") return(expect_true(TRUE))
  if (getRversion() < "3.6.0") return(expect_true(TRUE))

  url <- httpbin$url("/basic-auth/ruser/rpass")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  with_options(
    list(download.file.method = "wininet"),
         download(url, path = tmp, quiet = TRUE,
                  basic_auth = list(user = "ruser", password = "rpass")))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("curl download with basic auth", {
  mockery::stub(download, "getRversion", package_version("3.0.0"))

  url <- httpbin$url("/basic-auth/ruser/rpass")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  download(url, path = tmp, quiet = TRUE,
           basic_auth = list(user = "ruser", password = "rpass"))
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_true(resp$authenticated)
  expect_equal(resp$user, "ruser")
})

test_that("base curl download redirects", {
  skip_without_program("curl")

  url <- httpbin$url("/absolute-redirect/1")
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  with_options(
    list(download.file.method = "curl"),
         download(url, path = tmp, quiet = TRUE)
  )
  expect_true(file.exists(tmp))
  resp <- json$parse(readLines(tmp, warn = FALSE))
  expect_equal(resp$url, httpbin$url("/get"))
})
