
#' Install a package directly from bitbucket
#'
#' This function is vectorised so you can install multiple packages in
#' a single command.
#'
#' @inheritParams install_github
#' @param auth_user your account username if you're attempting to install
#'   a package hosted in a private repository (and your username is different
#'   to \code{username}). Defaults to the \code{BITBUCKET_USER} environment
#'   variable.
#' @param password your password. Defaults to the \code{BITBUCKET_PASSWORD}
#'   environment variable. See details for further information on setting
#'   up a password.
#' @param ref Desired git reference; could be a commit, tag, or branch name.
#'   Defaults to master.
#' @seealso Bitbucket API docs:
#'   \url{https://confluence.atlassian.com/bitbucket/use-the-bitbucket-cloud-rest-apis-222724129.html}
#'
#' @details To install from a private repo, or more generally, access the
#' Bitbucket API with your own credentials, you will need to get an access
#' token. You can create an access token following the instructions found in
#' the
#' \href{https://confluence.atlassian.com/bitbucket/app-passwords-828781300.html}{Bitbucket
#' App Passwords documentation}. The App Password requires read-only access to
#' your repositories and pull requests. Then store your password in the
#' environment variable \code{BITBUCKET_PASSWORD} (e.g. \code{evelynwaugh:swordofhonour})
#' @export
#' @examples
#' \dontrun{
#' install_bitbucket("sulab/mygene.r@@default")
#' install_bitbucket("dannavarro/lsr-package")
#' }
install_bitbucket <- function(repo, ref = "master", subdir = NULL,
                              auth_user = bitbucket_user(), password = bitbucket_password(),
                              host = "api.bitbucket.org/2.0", ...) {

  remotes <- lapply(repo, bitbucket_remote, ref = ref,
    subdir = subdir, auth_user = auth_user, password = password, host = host)

  install_remotes(remotes, auth_user = auth_user, password = password, host = host, ...)
}

bitbucket_remote <- function(repo, ref = "master", subdir = NULL,
                              auth_user = NULL, password = NULL, sha = NULL,
                              host = NULL, ...) {

  meta <- parse_git_repo(repo)

  remote("bitbucket",
    repo = meta$repo,
    subdir = meta$subdir %||% subdir,
    username = meta$username,
    ref = meta$ref %||% ref,
    sha = sha,
    auth_user = auth_user,
    password = password,
    host = host
  )
}

#' @export
remote_download.bitbucket_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading bitbucket repo ", x$username, "/", x$repo, "@", x$ref)
  }

  dest <- tempfile(fileext = paste0(".tar.gz"))

  url <- bitbucket_download_url(x$username, x$repo, x$ref, host = x$host, auth = basic_auth(x))

  download(dest, url, basic_auth = basic_auth(x))
}

#' @export
remote_metadata.bitbucket_remote <- function(x, bundle = NULL, source = NULL, sha = NULL) {
  if (!is.null(bundle)) {
    # Might be able to get from archive
    sha <- git_extract_sha1_tar(bundle)
  } else if (is.na(sha)) {
    sha <- NULL
  }

  list(
    RemoteType = "bitbucket",
    RemoteHost = x$host,
    RemoteRepo = x$repo,
    RemoteUsername = x$username,
    RemoteRef = x$ref,
    RemoteSha = sha,
    RemoteSubdir = x$subdir
  )
}

#' @export
remote_package_name.bitbucket_remote <- function(remote, ...) {

  bitbucket_DESCRIPTION(
    username = remote$username, repo = remote$repo,
    host = remote$host, auth = basic_auth(remote))$Package
}

#' @export
remote_sha.bitbucket_remote <- function(remote, ...) {
  bitbucket_commit(username = remote$username, repo = remote$repo,
    host = remote$host, ref = remote$ref, auth = basic_auth(remote))$sha %||% NA_character_
}

#' @export
format.bitbucket_remote <- function(x, ...) {
  "Bitbucket"
}

bitbucket_commit <- function(username, repo, ref = "master",
  host = "api.bitbucket.org/2.0", auth = NULL) {

  url <- build_url(host, "repositories", username, repo, "commit", ref)

  tmp <- tempfile()
  download(tmp, url, basic_auth = auth)

  fromJSONFile(tmp)
}

bitbucket_DESCRIPTION <- function(username, repo, subdir = NULL, ref = "master", host = "https://api.bitbucket.org/2.0", auth = NULL,...) {

  url <- build_url(host, "repositories", username, repo, "src", ref, paste0(subdir, "DESCRIPTION"))

  tmp <- tempfile()
  download(tmp, url, basic_auth = auth)

  read_dcf(tmp)
}

basic_auth <- function(x) {
  if (!is.null(x$password)) {
    list(
      user = x$auth_user %||% x$username,
      password = x$password
    )
  } else {
    NULL
  }
}


bitbucket_download_url <- function(username, repo, ref = "master",
  host = "api.bitbucket.org/2.0", auth = NULL) {

  url <- build_url(host, "repositories", username, repo)

  tmp <- tempfile()
  download(tmp, url, basic_auth = auth)

  paste0(build_url(fromJSONFile(tmp)$links$html$href, "get", ref), ".tar.gz")
}

bitbucket_password <- function(quiet = TRUE) {
  pass <- Sys.getenv("BITBUCKET_PASSWORD")
  if (identical(pass, "")) return(NULL)
  if (!quiet) message("Using bitbucket password from envvar BITBUCKET_PAT")
  pass
}

bitbucket_user <- function(quiet = TRUE) {
  user <- Sys.getenv("BITBUCKET_USER")
  if (identical(user, "")) return(NULL)
  if (!quiet) message("Using bitbucket user from envvar BITBUCKET_PAT")
  user
}
