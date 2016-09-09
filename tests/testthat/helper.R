
skip_if_offline <- function(host = "httpbin.org", port = 80) {

  res <- tryCatch(
    pingr::ping_port(host, count = 1L, port = port),
    error = function(e) NA
  )

  if (is.na(res)) skip("No internet connection")
}

skip_if_over_rate_limit <- function(by = 5) {

  tmp <- tempfile()
  download(
    tmp,
    "https://api.github.com/rate_limit",
    auth_token = github_pat()
  )

  res <- fromJSONFile(tmp)$rate$remaining
  if (is.null(res) || res <= by) skip("Over the GitHub rate limit")
}
