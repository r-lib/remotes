#' Attempts to install a package directly from GitHub.
#'
#' This function is vectorised on \code{repo} so you can install multiple
#' packages in a single command.
#'
#' @param repo Repository address in the format
#'   \code{username/repo[/subdir][@@ref|#pull]}. Alternatively, you can
#'   specify \code{subdir} and/or \code{ref} using the respective parameters
#'   (see below); if both is specified, the values in \code{repo} take
#'   precedence.
#' @param ref Desired git reference. Could be a commit, tag, or branch
#'   name, or a call to \code{\link{github_pull}}. Defaults to \code{"master"}.
#' @param subdir subdirectory within repo that contains the R package.
#' @param auth_token To install from a private repo, generate a personal
#'   access token (PAT) in \url{https://github.com/settings/applications} and
#'   supply to this argument. This is safer than using a password because
#'   you can easily delete a PAT without affecting any others. Defaults to
#'   the \code{GITHUB_PAT} environment variable.
#' @param host GitHub API host to use. Override with your GitHub enterprise
#'   hostname, for example, \code{"github.hostname.com/api/v3"}.
#' @param ... Other arguments passed on to \code{\link[utils]{install.packages}}.
#' @details
#' If the repository uses submodules a command-line git client is required to
#' clone the submodules.
#' @export
#' @seealso \code{\link{github_pull}}
#' @examples
#' \dontrun{
#' install_github("klutometis/roxygen")
#' install_github("wch/ggplot2")
#' install_github(c("rstudio/httpuv", "rstudio/shiny"))
#' install_github(c("hadley/httr@@v0.4", "klutometis/roxygen#142",
#'   "mfrasca/r-logging/pkg"))
#'
#' # To install from a private repo, use auth_token with a token
#' # from https://github.com/settings/applications. You only need the
#' # repo scope. Best practice is to save your PAT in env var called
#' # GITHUB_PAT.
#' install_github("hadley/private", auth_token = "abc")
#'
#' }
install_github <- function(repo,
                           ref = "master", subdir = NULL,
                           auth_token = github_pat(),
                           host = "api.github.com", ...) {

  remotes <- lapply(repo, github_remote, ref = ref,
    subdir = subdir, auth_token = auth_token, host = host)

  install_remotes(remotes, auth_token = auth_token, host = host, ...)
}

github_remote <- function(repo, ref = "master", subdir = NULL,
                       auth_token = github_pat(), sha = NULL,
                       host = "api.github.com") {

  meta <- parse_git_repo(repo)
  meta <- github_resolve_ref(meta$ref %||% ref, meta, auth_token)

  remote("github",
    host = host,
    package = meta$package,
    repo = meta$repo,
    subdir = meta$subdir %||% subdir,
    username = meta$username,
    ref = meta$ref,
    sha = sha,
    auth_token = auth_token
  )
}

#' @export
remote_download.github_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading GitHub repo ", x$username, "/", x$repo, "@", x$ref)
  }

  dest <- tempfile(fileext = paste0(".tar.gz"))
  src_root <- build_url(x$host, "repos", x$username, x$repo)
  src <- paste0(src_root, "/tarball/", utils::URLencode(x$ref, reserved = TRUE))

  download(dest, src, auth_token = x$auth_token)
}

#' @export
remote_metadata.github_remote <- function(x, bundle = NULL, source = NULL, sha = NULL) {

  if (!is.null(bundle)) {
    # Might be able to get from archive
    sha <- git_extract_sha1_tar(bundle)
  } else if (is_na(sha)) {
    sha <- NULL
  }

  list(
    RemoteType = "github",
    RemoteHost = x$host,
    RemotePackage = x$package,
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

#' GitHub references
#'
#' Use as \code{ref} parameter to \code{\link{install_github}}.
#' Allows installing a specific pull request or the latest release.
#'
#' @param pull The pull request to install
#' @seealso \code{\link{install_github}}
#' @rdname github_refs
#' @export
github_pull <- function(pull) structure(pull, class = "github_pull")

#' @rdname github_refs
#' @export
github_release <- function() structure(NA_integer_, class = "github_release")

github_resolve_ref <- function(x, params, ...) UseMethod("github_resolve_ref")

#' @export
github_resolve_ref.default <- function(x, params, ...) {
  params$ref <- x
  params
}

#' @export
github_resolve_ref.NULL <- function(x, params, ...) {
  params$ref <- "master"
  params
}

#' @export
github_resolve_ref.github_pull <- function(x, params, ..., auth_token = NULL) {
  # GET /repos/:user/:repo/pulls/:number
  path <- file.path("repos", params$username, params$repo, "pulls", x)
  response <- tryCatch(
    github_GET(path, pat = auth_token),
    error = function(e) e
  )

  ## Just because libcurl might download the error page...
  if (methods::is(response, "error") || is.null(response$head)) {
    stop("Cannot find GitHub pull request ", params$username, "/",
         params$repo, "#", x)
  }

  params$username <- response$head$user$login
  params$ref <- response$head$ref
  params
}

# Retrieve the ref for the latest release
#' @export
github_resolve_ref.github_release <- function(x, params, ..., auth_token = NULL) {
  # GET /repos/:user/:repo/releases
  path <- paste("repos", params$username, params$repo, "releases", sep = "/")
  response <- tryCatch(
    github_GET(path, pat = auth_token),
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

#' @export
remote_package_name.github_remote <- function(remote, ..., use_local = TRUE,
  use_curl = is_installed("curl")) {

  # If the package name was explicitly specified, use that
  if (!is.null(remote$package)) {
    return(remote$package)
  }

  # Otherwise if the repo is an already installed package assume that.
  if (isTRUE(use_local)) {
    local_name <- suppressWarnings(utils::packageDescription(remote$repo, fields = "Package"))
    if (!is.na(local_name)) {
      return(local_name)
    }
  }

  # Otherwise lookup the package name from the remote DESCRIPTION file
  desc <- github_DESCRIPTION(username = remote$username, repo = remote$repo,
    subdir = remote$subdir, host = remote$host, ref = remote$ref,
    pat = remote$auth_token, use_curl = use_curl)

  if (is.null(desc)) {
    return(NA_character_)
  }

  tmp <- tempfile()
  writeLines(desc, tmp)
  on.exit(unlink(tmp))

  read_dcf(tmp)$Package
}

#' @export
remote_sha.github_remote <- function(remote, ..., use_curl = is_installed("curl")) {
  github_commit(username = remote$username, repo = remote$repo,
    host = remote$host, ref = remote$ref, pat = remote$auth_token, use_curl = use_curl)
}

#' @export
format.github_remote <- function(x, ...) {
  "GitHub"
}
