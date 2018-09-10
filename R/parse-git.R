#' Parse a remote git repo specification
#'
#' A remote repo can be specified in two ways:
#' \describe{
#' \item{as a URL}{`parse_github_url()` handles HTTPS and SSH remote URLs
#' and various GitHub browser URLs}
#' \item{via a shorthand}{`parse_repo_spec()` handles this concise form:
#' `[username/]repo[/subdir][#pull|@ref|@*release]`}
#' }
#'
#' @param repo Character scalar, the repo specification.
#' @return List with members: `username`, `repo`, `subdir`
#'   `ref`, `pull`, `release`, some which will be empty.
#'
#' @name parse-git-repo
#' @examples
#' parse_repo_spec("metacran/crandb")
#' parse_repo_spec("jimhester/covr#47")        ## pull request
#' parse_repo_spec("jeroen/curl@v0.9.3")       ## specific tag
#' parse_repo_spec("tidyverse/dplyr@*release") ## shorthand for latest release
#' parse_repo_spec("r-lib/remotes@550a3c7d3f9e1493a2ba") ## commit SHA
#' parse_repo_spec("r-lib/remotes@550a3c7d3f9e1493a2ba") ## commit SHA
#' parse_repo_spec("igraph=igraph/rigraph") ## Different package name from repo name
#'
#' parse_github_url("https://github.com/jeroen/curl.git")
#' parse_github_url("git@github.com:metacran/crandb.git")
#' parse_github_url("https://github.com/jimhester/covr")
#' parse_github_url("https://github.example.com/user/repo.git")
#' parse_github_url("git@github.example.com:user/repo.git")
#'
#' parse_github_url("https://github.com/r-lib/remotes/pull/108")
#' parse_github_url("https://github.com/r-lib/remotes/tree/name-of-branch")
#' parse_github_url("https://github.com/r-lib/remotes/commit/1234567")
#' parse_github_url("https://github.com/r-lib/remotes/releases/latest")
#' parse_github_url("https://github.com/r-lib/remotes/releases/tag/1.0.0")
NULL

#' @export
#' @rdname parse-git-repo
parse_repo_spec <- function(repo) {
  package_name_rx <- "(?:(?<package>[[:alpha:]][[:alnum:].]*[[:alnum:]])=)?"
  username_rx <- "(?:(?<username>[^/]+)/)"
  repo_rx     <- "(?<repo>[^/@#]+)"
  subdir_rx   <- "(?:/(?<subdir>[^@#]*[^@#/])/?)?"
  ref_rx      <- "(?:@(?<ref>[^*].*))"
  pull_rx     <- "(?:#(?<pull>[0-9]+))"
  release_rx  <- "(?:@(?<release>[*]release))"
  ref_or_pull_or_release_rx <- sprintf(
    "(?:%s|%s|%s)?", ref_rx, pull_rx, release_rx
  )
  spec_rx  <- sprintf(
    "^%s%s%s%s%s$", package_name_rx, username_rx, repo_rx, subdir_rx, ref_or_pull_or_release_rx
  )
  params <- as.list(re_match(text = repo, pattern = spec_rx))

  if (is.na(params$.match)) {
    stop(sprintf("Invalid git repo specification: '%s'", repo))
  }

  params[grepl("^[^\\.]", names(params))]
}

#' @export
#' @rdname parse-git-repo
parse_github_repo_spec <- parse_repo_spec

#' @export
#' @rdname parse-git-repo
parse_github_url <- function(repo) {
  prefix_rx <- "(?:github[^/:]+[/:])"
  username_rx <- "(?:(?<username>[^/]+)/)"
  repo_rx     <- "(?<repo>[^/@#]+)"
  ref_rx <- "(?:(?:tree|commit|releases/tag)/(?<ref>.+$))"
  pull_rx <- "(?:pull/(?<pull>.+$))"
  release_rx <- "(?:releases/)(?<release>.+$)"
  ref_or_pull_or_release_rx <- sprintf(
    "(?:/(%s|%s|%s))?", ref_rx, pull_rx, release_rx
  )
  url_rx  <- sprintf(
    "%s%s%s%s",
    prefix_rx, username_rx, repo_rx, ref_or_pull_or_release_rx
  )
  params <- as.list(re_match(text = repo, pattern = url_rx))

  if (is.na(params$.match)) {
    stop(sprintf("Invalid GitHub URL: '%s'", repo))
  }
  if (params$ref == "" && params$pull == "" && params$release == "") {
    params$repo <- gsub("\\.git$", "", params$repo)
  }
  if (params$release == "latest") {
    params$release <- "*release"
  }

  params[grepl("^[^\\.]", names(params))]
}

parse_git_repo <- function(repo) {

  if (grepl("^https://github|^git@github", repo)) {
    params <- parse_github_url(repo)
  } else {
    params <- parse_repo_spec(repo)
  }
  params <- params[viapply(params, nchar) > 0]

  if (!is.null(params$pull)) {
    params$ref <- github_pull(params$pull)
    params$pull <- NULL
  }

  if (!is.null(params$release)) {
    params$ref <- github_release()
    params$release <- NULL
  }

  params
}

