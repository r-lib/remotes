
github_GET <- function(path, ..., host = "api.github.com", pat = github_pat(), use_curl = !is_standalone() && pkg_installed("curl")) {

  url <- build_url(host, path)

  if (isTRUE(use_curl)) {
    h <- curl::new_handle()
    headers <- c(
      if (!is.null(pat)) {
        c("Authorization" = paste0("token ", pat))
      }
    )
    curl::handle_setheaders(h, .list = headers)
    res <- curl::curl_fetch_memory(url, handle = h)

    if (res$status_code >= 300) {
      stop(github_error(res))
    }
    fromJSON(rawToChar(res$content))
  } else {
    tmp <- tempfile()
    download(tmp, url, auth_token = pat)

    fromJSONFile(tmp)
  }
}

github_commit <- function(username, repo, ref = "master",
  host = "api.github.com", pat = github_pat(), use_curl = !is_standalone() && pkg_installed("curl"), current_sha = NULL) {

  url <- build_url(host, "repos", username, repo, "commits", utils::URLencode(ref, reserved = TRUE))

  if (isTRUE(use_curl)) {
    h <- curl::new_handle()
    headers <- c(
      "Accept" = "application/vnd.github.v3.sha",
      if (!is.null(pat)) {
        c("Authorization" = paste0("token ", pat))
      }
    )

    if (!is.null(current_sha)) {
      headers <- c(headers, "If-None-Match" = paste0('"', current_sha, '"'))
    }
    curl::handle_setheaders(h, .list = headers)
    res <- curl::curl_fetch_memory(url, handle = h)
    if (res$status_code == 304) {
      return(current_sha)
    }
    if (res$status_code >= 300) {
      stop(github_error(res))
    }

    rawToChar(res$content)
  } else {
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)

    download(tmp, url, auth_token = pat)
    get_json_field(readLines(tmp, warn = FALSE), "sha")
  }
}

#' Retrieve Github personal access token.
#'
#' A github personal access token
#' Looks in env var `GITHUB_PAT`
#'
#' @keywords internal
#' @noRd
github_pat <- function(quiet = TRUE) {
  pat <- Sys.getenv('GITHUB_PAT')
  if (identical(pat, "")) return(NULL)

  if (!quiet) {
    message("Using github PAT from envvar GITHUB_PAT")
  }
  pat
}

github_DESCRIPTION <- function(username, repo, subdir = NULL, ref = "master", host = "api.github.com", ...,
  use_curl = !is_standalone() && pkg_installed("curl"), pat = github_pat()) {

  if (!is.null(subdir)) {
    subdir <- utils::URLencode(subdir)
  }

  url <- build_url(host, "repos", username, repo, "contents", subdir, "DESCRIPTION")
  url <- paste0(url, "?ref=", utils::URLencode(ref))

  if (isTRUE(use_curl)) {
    h <- curl::new_handle()
    headers <- c(
      "Accept" = "application/vnd.github.v3.raw",
      if (!is.null(pat)) {
        c("Authorization" = paste0("token ", pat))
      }
    )

    curl::handle_setheaders(h, .list = headers)
    res <- curl::curl_fetch_memory(url, handle = h)
    if (res$status_code >= 300) {
      stop(github_error(res))
    }
    rawToChar(res$content)
  } else {
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)

    tmp <- tempfile()
    download(tmp, url, auth_token = pat)

    base64_decode(gsub("\\\\n", "", fromJSONFile(tmp)$content))
  }
}

github_error <- function(res) {
  res_headers <- curl::parse_headers_list(res$headers)
  ratelimit_remaining <- res_headers$`x-ratelimit-remaining`

  ratelimit_reset <- .POSIXct(res_headers$`x-ratelimit-reset`, tz = "UTC")

  error_details <- fromJSON(rawToChar(res$content))$message

  msg <- sprintf(
"HTTP error %s.
  %s

  Rate limit remaining: %s
  Rate limit reset at: %s",

    res$status_code,
    error_details,
    ratelimit_remaining,
    format(ratelimit_reset, usetz = TRUE)
  )

  structure(list(message = msg, call = NULL), class = c("simpleError", "error", "condition"))
}


#> Error: HTTP error 404.
#>   Not Found
#> 
#>   Rate limit remaining: 4999
#>   Rate limit reset at: 2018-10-10 19:43:52 UTC
