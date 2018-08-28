
github_GET <- function(path, ..., host = "api.github.com", pat = github_pat()) {

  url <- build_url(host, path)

  tmp <- tempfile()
  download(tmp, url, auth_token = pat)

  fromJSONFile(tmp)
}

github_commit <- function(username, repo, ref = "master",
  host = "api.github.com", pat = github_pat(), use_curl = is_installed("curl"), current_sha = NULL) {

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
      stop("HTTP error ", res$status_code, ".", call. = FALSE)
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
#' Looks in env var \code{GITHUB_PAT}
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
  use_curl = is_installed("curl"), pat = github_pat()) {

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
      stop("HTTP error ", res$status_code, ".", call. = FALSE)
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
