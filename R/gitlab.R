
gitlab_GET <- function(path, ..., pat = gitlab_pat()) {

  url <- paste0("https://www.gitlab.com/api/v3/", path)

  tmp <- tempfile()
  download(tmp, url, auth_token = pat)

  fromJSONFile(tmp)
}

gitlab_commit <- function(username, repo, ref = "master") {

  url <- file.path("https://www.gitlab.com/api/v3",
                   "projects", paste0(username, "%2F", repo), 
                   "repository/commits", ref)

  tmp <- tempfile()
  download(tmp, url, meta$auth_token %||% auth_token %||% gitlab_pat())

  fromJSONFile(tmp)
}

#' Retrieve GitLab personal access token.
#'
#' A GitLab personal access token
#' Looks in env var \code{GITLAB_PAT}
#'
#' @keywords internal
#' @noRd
gitlab_pat <- function() {
  pat <- Sys.getenv('GITLAB_PAT')
  if (identical(pat, "")) return(NULL)

  message("Using GitLab PAT from envvar GITLAB_PAT")
  pat
}
