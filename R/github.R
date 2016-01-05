
github_GET <- function(path, ..., pat = github_pat()) {

  url <- paste0("https://api.github.com/", path)

  tmp <- tempfile()
  download(tmp, url, auth_token = pat)

  fromJSONFile(tmp)
}

github_commit <- function(username, repo, ref = "master") {

  url <- file.path("https://api.github.com",
                   "repos", username, repo, "commits", ref)

  tmp <- tempfile()
  download(tmp, url, auth_token = github_pat())

  fromJSONFile(tmp)$sha
}

#' Retrieve Github personal access token.
#'
#' A github personal access token
#' Looks in env var \code{GITHUB_PAT}
#'
#' @keywords internal
#' @noRd
github_pat <- function() {
  pat <- Sys.getenv('GITHUB_PAT')
  if (identical(pat, "")) return(NULL)

  message("Using github PAT from envvar GITHUB_PAT")
  pat
}
