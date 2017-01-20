#' Attempts to install a package directly from GitLab.
#'
#' This function is vectorised on \code{repo} so you can install multiple
#' packages in a single command.
#'
#' @param repo Repository address in the format
#'   \code{username/repo[/subdir][@@ref|#merge]}. Alternatively, you can
#'   specify \code{subdir} and/or \code{ref} using the respective parameters
#'   (see below); if both is specified, the values in \code{repo} take
#'   precedence.
#' @param ref Desired git reference. Could be a commit, tag, or branch
#'   name, or a call to \code{\link{gitlab_merge}}. Defaults to \code{"master"}.
#' @param subdir subdirectory within repo that contains the R package.
#' @param auth_token To install from a private repo, generate a personal
#'   access token (PAT) in \url{https://gitlab.com/profile/personal_access_tokens} and
#'   supply to this argument. This is safer than using a password because
#'   you can easily delete a PAT without affecting any others. Defaults to
#'   the \code{GITLAB_PAT} environment variable.
#' @param host GitLab API host to use. Override with your GitLab hostname, for 
#'   example, \code{"gitlab.hostname.com/api/v3"}. #check this
#' @param ... Other arguments passed on to \code{install.packages}.
#' @details
#' Attempting to install from a source repository that uses submodules
#' raises a warning. Because the zipped sources provided by GitLab do not
#' include submodules, this may lead to unexpected behaviour or compilation
#' failure in source packages. In this case, cloning the repository manually
#' may yield better results.
#' @export
#' @seealso \code{\link{gitlab_merge}}
#' @examples
#' \dontrun{
#' install_gitlab("ConorIA/roxygen")
#' install_gitlab("wch/ggplot2")
#' install_gitlab(c("rstudio/httpuv", "rstudio/shiny"))
#' install_gitlab(c("hadley/httr@@v0.4", "klutometis/roxygen#142",
#'   "mfrasca/r-logging/pkg"))
#'
#' # To install from a private repo, use auth_token with a token
#' # from https://gitlab.com/settings/applications. You only need the
#' # repo scope. Best practice is to save your PAT in env var called
#' # GITHUB_PAT.
#' install_gitlab("hadley/private", auth_token = "abc")
#'
#' }
install_gitlab <- function(repo, ref = "master", subdir = NULL,
                           auth_token = gitlab_pat(),
                           host = "www.gitlab.com", ...) {

  remotes <- lapply(repo, gitlab_remote, ref = ref,
    subdir = subdir, auth_token = auth_token, host = host)

  install_remotes(remotes, ...)
}

gitlab_remote <- function(repo, ref = NULL, subdir = NULL,
                       auth_token = gitlab_pat(), sha = NULL,
                       host = "www.gitlab.com") {

  meta <- parse_git_repo(repo)
  meta <- gitlab_resolve_ref(meta$ref %||% ref, meta)

  remote("gitlab",
    host = host,
    repo = meta$repo,
    subdir = meta$subdir %||% subdir,
    username = meta$username,
    ref = meta$ref,
    sha = sha,
    auth_token = auth_token
  )
}

#' @export
remote_download.gitlab_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading GitLab repo ", x$username, "/", x$repo, "@", x$ref)
  }

  dest <- tempfile(fileext = paste0(".zip"))
  src_root <- paste0("https://", x$host, "/api/v3/projects/", x$username, "%2F", x$repo)
  src <- paste0(src_root, "/repository/archive.zip?", utils::URLencode(x$ref, reserved = TRUE))

  if (gitlab_has_submodules(x)) {
    warning("GitLab repo contains submodules, may not function as expected!",
            call. = FALSE)
  }

  download(dest, src, auth_token = x$auth_token)
}

gitlab_has_submodules <- function(x) {
  src_root <- paste0("https://", x$host, "/repos/", x$username, "/", x$repo)
  src_submodules <- paste0(src_root, "/contents/.gitmodules?ref=", x$ref)

  tmp <- tempfile()
  res <- tryCatch(
    download(tmp, src_submodules, auth_token = x$auth_token),
    error = function(e) e
  )
  if (methods::is(res, "error")) return(FALSE)

  ## download() sometimes just downloads the error page, because
  ## the libcurl backend in download.file() is broken
  ## If the request was successful (=submodules exist), then it has an
  ## 'sha' field.
  sha <- tryCatch(
    fromJSONFile(tmp)$sha,
    error = function(e) e
  )
  ! methods::is(sha, "error") && ! is.null(sha)
}

#' @export
remote_metadata.gitlab_remote <- function(x, bundle = NULL, source = NULL) {
  # Determine sha as efficiently as possible
  if (!is.null(x$sha)) {
    # Might be cached already (because re-installing)
    sha <- x$sha
  } else if (!is.null(bundle)) {
    # Might be able to get from zip archive
    sha <- git_extract_sha1(bundle)
  } else {
    # Otherwise can use gitlab api
    sha <- gitlab_commit(x$username, x$repo, x$ref)$sha
  }

  list(
    RemoteType = "gitlab",
    RemoteHost = x$host,
    RemoteRepo = x$repo,
    RemoteUsername = x$username,
    RemoteRef = x$ref,
    RemoteSha = sha,
    RemoteSubdir = x$subdir,
    # Backward compatibility for packrat etc.
    GithubRepo = x$repo,
    GithubUsername = x$username,
    GithubRef = x$ref,
    GithubSHA1 = sha,
    GithubSubdir = x$subdir
  )
}

#' GitLab references
#'
#' Use as \code{ref} parameter to \code{\link{install_gitlab}}.
#' Allows installing a specific merge request or the latest release.
#'
#' @param merge The merge request to install
#' @seealso \code{\link{install_gitlab}}
#' @rdname gitlab_refs
#' @export
gitlab_merge <- function(merge) structure(merge, class = "gitlab_merge")

#' @rdname gitlab_refs
#' @export
gitlab_release <- function() structure(NA_integer_, class = "gitlab_release")

gitlab_resolve_ref <- function(x, params) UseMethod("gitlab_resolve_ref")

#' @export
gitlab_resolve_ref.default <- function(x, params) {
  params$ref <- x
  params
}

#' @export
gitlab_resolve_ref.NULL <- function(x, params) {
  params$ref <- "master"
  params
}

#' @export
gitlab_resolve_ref.gitlab_merge <- function(x, params) {
  # GET /projects/:user/:repo/merge_request/:number
  path <- file.path("projects", paste0(params$username, "%2F", params$repo), "merge_requests", x)
  response <- tryCatch(
    gitlab_GET(path),
    error = function(e) e
  )

  ## Just because libcurl might download the error page...
  if (methods::is(response, "error") || is.null(response$head)) {
    stop("Cannot find GitLab merge request ", params$username, "/",
         params$repo, "#", x)
  }

  params$username <- response$head$user$login
  params$ref <- response$head$ref
  params
}

# Retrieve the ref for the latest release
#' @export
gitlab_resolve_ref.gitlab_release <- function(x, params) {
  # GET /repos/:user/:repo/releases
  path <- paste("repos", params$username, params$repo, "releases", sep = "/")
  response <- tryCatch(
    gitlab_GET(path),
    error = function(e) e
  )

  if (methods::is(response, "error") || !is.null(response$message)) {
    stop("Cannot find repo ", params$username, "/", params$repo, ".")
  }

  if (length(response) == 0L)
    stop("No releases found for repo ", params$username, "/", params$repo, ".")

  params$ref <- response[[1L]]$tag_name
  params
}

#' Parse a concise GitLab repo specification
#'
#' The current format is:
#' \code{[username/]repo[/subdir][#merge|@ref|@*release]}
#' The \code{*release} suffix represents the latest release.
#'
#' @param repo Character scalar, the repo specification.
#' @return List with members: \code{username}, \code{repo}, \code{subdir}
#'   \code{ref}, \code{merge}, \code{release}. Members that do not
#'   appear in the input repo specification are omitted.
#'
#' @export
#' @examples
#' parse_gitlab_repo_spec("metacran/crandb")
#' parse_gitlab_repo_spec("jeroenooms/curl@v0.9.3")
#' parse_gitlab_repo_spec("jimhester/covr#47")
#' parse_gitlab_repo_spec("hadley/dplyr@*release")
#' parse_gitlab_repo_spec("mangothecat/remotes@550a3c7d3f9e1493a2ba")

parse_gitlab_repo_spec <- function(repo) {
  username_rx <- "(?:([^/]+)/)?"
  repo_rx <- "([^/@#]+)"
  subdir_rx <- "(?:/([^@#]*[^@#/]))?"
  ref_rx <- "(?:@([^*].*))"
  merge_rx <- "(?:#([0-9]+))"
  release_rx <- "(?:@([*]release))"
  ref_or_merge_or_release_rx <- sprintf("(?:%s|%s|%s)?", ref_rx, merge_rx, release_rx)
  gitlab_rx <- sprintf("^(?:%s%s%s%s|(.*))$",
    username_rx, repo_rx, subdir_rx, ref_or_merge_or_release_rx)

  param_names <- c("username", "repo", "subdir", "ref", "merge", "release", "invalid")
  replace <- stats::setNames(sprintf("\\%d", seq_along(param_names)), param_names)
  params <- lapply(replace, function(r) gsub(gitlab_rx, r, repo, perl = TRUE))
  if (params$invalid != "")
    stop(sprintf("Invalid git repo: %s", repo))
  params <- params[viapply(params, nchar) > 0]

  params
}

parse_git_repo <- function(repo) {
  params <- parse_gitlab_repo_spec(repo)

  if (!is.null(params$merge)) {
    params$ref <- gitlab_merge(params$merge)
    params$merge <- NULL
  }

  if (!is.null(params$release)) {
    params$ref <- gitlab_release()
    params$release <- NULL
  }

  params
}
