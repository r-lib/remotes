
skip_if_offline <- function(host = "httpbin.org", port = 80) {

  res <- tryCatch(
    pingr::ping_port(host, count = 1L, port = port),
    error = function(e) NA
  )

  if (is.na(res)) skip("No internet connection")
}

skip_if_over_rate_limit <- function(by = 50) {

  tmp <- tempfile()
  download(
    tmp,
    "https://api.github.com/rate_limit",
    auth_token = github_pat()
  )

  res <- json$parse_file(tmp)$rate$remaining
  if (is.null(res) || res <= by) skip("Over the GitHub rate limit")
}

expect_equal_named_lists <- function(object, expected, ...) {
  expect_true(!is.null(names(object)) && !is.null(names(expected)))
  expect_true(is.list(object) && is.list(expected))
  object <- object[order(names(object))]
  expected <- expected[order(names(expected))]
  expect_equal(!!object, !!expected)
}

skip_without_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    skip(paste("Need the", pkg, "package"))
  }
}

skip_without_program <- function(program) {
  if (Sys.which(program) == "") {
    skip(paste("Need the", program, "program"))
  }
}

test_temp_file <- function(fileext = "", pattern = "test-file-",
                           envir = parent.frame()) {
  tmp <- tempfile(pattern = pattern, fileext = fileext)
  withr::defer(
    try(unlink(tmp, recursive = TRUE, force = TRUE), silent = TRUE),
    envir = envir)
  tmp
}

test_temp_dir <- function(pattern = "test-dir-", envir = parent.frame()) {
  tmp <- test_temp_file(pattern, envir = envir)
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  tmp
}
