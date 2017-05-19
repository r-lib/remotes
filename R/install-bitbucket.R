
#' Install a package directly from bitbucket
#'
#' This function is vectorised so you can install multiple packages in
#' a single command.
#'
#' @inheritParams install_github
#' @param auth_user your account username if you're attempting to install
#'   a package hosted in a private repository (and your username is different
#'   to \code{username})
#' @param password your password
#' @param ref Desired git reference; could be a commit, tag, or branch name.
#'   This argument is only used if \code{repo} does not specify a reference.
#'   Defaults to master.
#' @seealso Bitbucket API docs:
#'   \url{https://confluence.atlassian.com/bitbucket/use-the-bitbucket-cloud-rest-apis-222724129.html}
#'
#' @export
#' @examples
#' \dontrun{
#' install_bitbucket("sulab/mygene.r@@default")
#' install_bitbucket("dannavarro/lsr-package")
#' }
install_bitbucket <- function(repo, ref = "master", subdir = NULL,
                              auth_user = NULL, password = NULL,
                              force = FALSE, ...) {

  remotes <- lapply(repo, bitbucket_remote, ref = ref,
    subdir = subdir, auth_user = auth_user, password = password)

  if (!isTRUE(force)) {
    remotes <- Filter(different_sha, remotes)
  }

  install_remotes(remotes, ...)
}

bitbucket_remote <- function(repo, ref = NULL, subdir = NULL,
                              auth_user = NULL, password = NULL, sha = NULL) {

  meta <- parse_git_repo(repo)

  meta$ref <- meta$ref %||% ref %||% "master"

  remote("bitbucket",
    repo = meta$repo,
    subdir = meta$subdir %||% subdir,
    username = meta$username,
    ref = meta$ref %||% ref,
    sha = sha,
    auth_user = auth_user,
    password = password
  )
}

#' @export
remote_download.bitbucket_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading bitbucket repo ", x$username, "/", x$repo, "@", x$ref)
  }

  dest <- tempfile(fileext = paste0(".zip"))
  src <- paste("https://bitbucket.org/", x$username, "/", tolower(x$repo), "/get/",
    x$ref, ".zip", sep = "")

  if (!is.null(x$password)) {
    auth <- list(
      user = x$auth_user %||% x$username,
      password = x$password
    )
  } else {
    auth <- NULL
  }

  download(dest, src, basic_auth = auth)
}

#' @export
remote_metadata.bitbucket_remote <- function(x, bundle = NULL, source = NULL) {
  # Determine sha as efficiently as possible
  if (!is.null(x$sha)) {
    # Might be cached already (because re-installing)
    sha <- x$sha
  } else if (!is.null(bundle)) {
    # Might be able to get from zip archive
    sha <- git_extract_sha1(bundle)
  } else {
    # Don't know
    sha <- NULL
  }

  list(
    RemoteType = "bitbucket",
    RemoteRepo = x$repo,
    RemoteUsername = x$username,
    RemoteRef = x$ref,
    RemoteSha = sha,
    RemoteSubdir = x$subdir
  )
}

#' @export
remote_package_name.bitbucket_remote <- function(remote, ...) {
  remote_package_name.github_remote(
    remote,
    url = "https://bitbucket.org",
    ...
  )
}

#' @export
remote_sha.bitbucket_remote <-function(remote, ...) {
  remote_sha.github_remote(remote, url = "https://bitbucket.org", ...)
}
