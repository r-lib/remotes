bitbucket_GET <- function(path, ..., pat = bitbucket_pat(), api_version = "2.0") {
  url <- paste0("https://api.bitbucket.org/", api_version, "/", path)
  tmp <- tempfile()
  download(tmp, url, auth_token = pat)
  fromJSONFile(tmp)
}


bitbucket_pat <- function() {
  pat <- Sys.getenv("BITBUCKET_PAT")
  if (identical(pat, "")) return(NULL)
  message("Using bitbucket PAT from envvar BITBUCKET_PAT")
  pat
}

parse_repo <- function(path, rx, nms) {
  replace <- stats::setNames(sprintf("\\%d", seq_along(nms)), nms)
  params <- lapply(replace, function(r) gsub(rx, r, path, perl = TRUE))
  if (params$invalid != "")
    stop(sprintf("Invalid repo: %s", path))
  params <- params[sapply(params, nchar) > 0]
  params
}

parse_bitbucket_repo <- function(path) {
  username_rx <- "(?:([^/]+)/)?"
  repo_rx <- "([^/@#]+)"
  subdir_rx <- "(?:/([^@#]*[^@#/]))?"
  ref_rx <- "(?:@([^*].*))"
  pull_rx <- "(?:#([0-9]+))"
  ref_or_pull_rx <- sprintf("(?:%s|%s)?", ref_rx, pull_rx)
  bitbucket_rx <- sprintf("^(?:%s%s%s%s|(.*))$",
                          username_rx, repo_rx, subdir_rx, ref_or_pull_rx)
  param_names <- c("username", "repo", "subdir", "ref", "pull", "invalid")
  params <- parse_repo(path, bitbucket_rx, param_names)
  if (!is.null(params$pull)) {
    params$ref <- bitbucket_pull(params$pull)
    params$pull <- NULL
  }
  params
}

#' Bitbucket references
#'
#' Use as \code{ref} parameter to \code{\link{install_bitbucket}}.
#' Allows installing a specific pull request.
#'
#' @param pull The pull request to install
#' @seealso \code{\link{install_bitbucket}}
#' @export
bitbucket_pull <- function(pull) structure(pull, class = "bitbucket_pull")

#' @export
resolve_ref.bitbucket_pull <- function(x, params, ...) {
  # GET /repositories/{owner}/{repo_slug}/pullrequests/{id}
  # https://confluence.atlassian.com/bitbucket/pullrequests-resource-423626332.html#pullrequestsResource-GETaspecificpullrequest
  path <- file.path("repositories", params$username, params$repo,
                    "pullrequests", x)
  response <- tryCatch(
    bitbucket_GET(path, ..., api_version = "2.0"),
    error = function(e) e
  )
  ## Just because libcurl might download the error page...
  if (methods::is(response, "error") || is.null(response$head)) {
    stop("Cannot find Bitbucket pull request ", params$username, "/",
         params$repo, "#", x)
  }
  params$username <- response$author$username
  params$ref <- response$source$branch$name
  params
}
