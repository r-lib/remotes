
github_commit <- function(username, repo, ref = "master") {

  url <- file.path("https://api.github.com",
                   "repos", username, repo, "commits", ref)

  tmp <- tempfile()
  download(tmp, url, auth_token = github_pat())

  json_dict_get(tmp, "sha")
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
