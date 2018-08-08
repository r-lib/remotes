#' Install a package from GitLab
#'
#' This function is vectorised on \code{repo} so you can install multiple
#' packages in a single command. Like other remotes the repository will skip
#' installation if `force == FALSE` (the default) and the remote state has
#' not changed since the previous installation.
#'
#' @inheritParams install_github
#' @param repo Repository address in the format
#'   \code{username/repo[/subdir][@@ref]}.
#' @param host GitLab API host to use. Override with your GitLab enterprise
#'   hostname, for example, \code{"gitlab.hostname.com"}.
#' @export
#' @family package installation
#' @examples
#' \dontrun{
#' install_gitlab("jimhester/covr")
#' }
install_gitlab <- function(repo,
                           auth_token = gitlab_pat(),
                           host = "gitlab.com", ...)
{

  remotes <- lapply(repo, gitlab_remote, auth_token = auth_token, host = host)

  install_remotes(remotes, ...)
}

gitlab_remote <- function(repo,
                       auth_token = gitlab_pat(), sha = NULL,
                       host = "gitlab.com") {

  meta <- parse_git_repo(repo)
  meta$ref <- meta$ref %||% "master"

  remote("gitlab",
    host = host,
    repo = meta$repo,
    subdir = meta$subdir,
    username = meta$username,
    ref = meta$ref,
    sha = sha,
    auth_token = auth_token
  )
}

#' @export
remote_download.gitlab_remote <- function(x, quiet = FALSE) {
  dest <- tempfile(fileext = paste0(".zip"))

  src_root <- build_url(x$host, x$username, x$repo)
  src <- paste0(src_root, "/repository/archive.zip?ref=", utils::URLencode(x$ref, reserved = TRUE))

  if (!quiet) {
    message("Downloading GitLab repo ", x$username, "/", x$repo, "@", x$ref,
            "\nfrom URL ", src)
  }

  download(dest, src, auth_token = x$auth_token)
}

#' @export
remote_metadata.gitlab_remote <- function(x, bundle = NULL, source = NULL) {
  # Determine sha as efficiently as possible
  if (!is.null(bundle)) {
    # Might be able to get from zip archive
    sha <- git_extract_sha1(bundle)
  } else {
    # Otherwise can lookup with remote_ls
    sha <- remote_sha(x)
  }

  list(
    RemoteType = "gitlab",
    RemoteHost = x$host,
    RemoteRepo = x$repo,
    RemoteUsername = x$username,
    RemoteRef = x$ref,
    RemoteSha = sha,
    RemoteSubdir = x$subdir
  )
}

#' @export
remote_package_name.gitlab_remote <- function(remote, ...) {

  tmp <- tempfile()
  src <- build_url(
    remote$host, remote$username, remote$repo, "raw",
    remote$ref, remote$subdir, "DESCRIPTION")

  dest <- tempfile()
  res <- download(dest, src, auth_token = remote$auth_token)

  tryCatch(
    read_dcf(dest)$Package,
    error = function(e) NA_character_)
}

#' @export
remote_sha.gitlab_remote <- function(remote, url = "https://gitlab.com", ...) {
  gitlab_commit(username = remote$username, repo = remote$repo,
    host = remote$host, ref = remote$ref, pat = remote$auth_token)
}

#' @export
format.gitlab_remote <- function(x, ...) {
  "GitLab"
}

gitlab_commit <- function(username, repo, ref = "master",
  host = "gitlab.com", pat = gitlab_pat()) {

  url <- build_url(host, "api", "v4", "projects", utils::URLencode(paste0(username, "/", repo), reserved = TRUE), "repository", "commits", ref)

  tmp <- tempfile()
  download(tmp, url, auth_token = pat)

  fromJSONFile(tmp)$id
}

#' Retrieve GitLab personal access token.
#'
#' A GitLab personal access token
#' Looks in env var \code{GITLAB_PAT}
#'
#' @keywords internal
#' @export
gitlab_pat <- function(quiet = TRUE) {
  pat <- Sys.getenv("GITLAB_PAT")
  if (nzchar(pat)) {
    if (!quiet) {
      message("Using GitLab PAT from envvar GITLAB_PAT")
    }
    return(pat)
  }
  return(NULL)
}
