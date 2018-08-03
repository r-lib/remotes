
github_GET <- function(path, ..., host = "api.github.com", pat = github_pat()) {

  url <- build_url(host, path)

  tmp <- tempfile()
  download(tmp, url, auth_token = pat)

  fromJSONFile(tmp)
}

github_commit <- function(username, repo, ref = "master",
  host = "api.github.com", pat = github_pat()) {

  url <- build_url(host, "repos", username, repo, "commits", utils::URLencode(ref, reserved = TRUE))

  tmp <- tempfile()
  download(tmp, url, auth_token = pat)

  get_json_field(readLines(tmp, warn = FALSE), "sha")
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

github_DESCRIPTION <- function(username, repo, subdir = NULL, ref = "master", host = "api.github.com", ...) {

  url <- build_url(host, "repos", username, repo, "contents", paste0(subdir, "DESCRIPTION"))
  url <- paste0(url, "?ref=", utils::URLencode(ref))

  tmp <- tempfile()
  download(tmp, url, auth_token = github_pat())

  base64_decode(gsub("\\\\n", "", fromJSONFile(tmp)$content))
}
