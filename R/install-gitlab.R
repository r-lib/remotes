#' Install a package from GitLab
#'
#' This function is vectorised on `repo` so you can install multiple
#' packages in a single command. Like other remotes the repository will skip
#' installation if `force == FALSE` (the default) and the remote state has
#' not changed since the previous installation.
#'
#' @inheritParams install_github
#' @param repo Repository address in the format
#'   `username/repo[/subdir][@@ref]`.
#' @param host GitLab API host to use. Override with your GitLab enterprise
#'   hostname, for example, `"gitlab.hostname.com"`.
#' @param auth_token To install from a private repo, generate a personal access
#'   token (PAT) in \url{https://gitlab.com/profile/personal_access_tokens} and
#'   supply to this argument. This is safer than using a password because you
#'   can easily delete a PAT without affecting any others. Defaults to the
#'   GITLAB_PAT environment variable.
#' @inheritParams install_github
#' @export
#' @family package installation
#' @examples
#' \dontrun{
#' install_gitlab("jimhester/covr")
#' }
install_gitlab <- function(repo,
                           auth_token = gitlab_pat(),
                           host = "gitlab.com",
                           dependencies = NA,
                           upgrade = c("default", "ask", "always", "never"),
                           force = FALSE,
                           quiet = FALSE,
                           build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                           repos = getOption("repos"),
                           type = getOption("pkgType"),
                           ...) {

  remotes <- lapply(repo, gitlab_remote, auth_token = auth_token, host = host)

  install_remotes(remotes, auth_token = auth_token, host = host,
                  dependencies = dependencies,
                  upgrade = upgrade,
                  force = force,
                  quiet = quiet,
                  build = build,
                  build_opts = build_opts,
                  repos = repos,
                  type = type,
                  ...)
}

gitlab_remote <- function(repo,
                       auth_token = gitlab_pat(), sha = NULL,
                       host = "gitlab.com", ...) {

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
  dest <- tempfile(fileext = paste0(".tar.gz"))

  src_root <- build_url(x$host, "api", "v4", "projects", utils::URLencode(paste0(x$username, "/", x$repo), reserved = TRUE))
  src <- paste0(src_root, "/repository/archive.tar.gz?sha=", utils::URLencode(x$ref, reserved = TRUE))

  if (!quiet) {
    message("Downloading GitLab repo ", x$username, "/", x$repo, "@", x$ref,
            "\nfrom URL ", src)
  }

  download(dest, src, auth_token = x$auth_token, auth_phrase = "private_token=")
}

#' @export
remote_metadata.gitlab_remote <- function(x, bundle = NULL, source = NULL, sha = NULL) {

  if (!is.null(bundle)) {
    # Might be able to get from archive
    sha <- git_extract_sha1_tar(bundle)
  } else if (is_na(sha)) {
    sha <- NULL
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

  src_root <- build_url(
    remote$host, "api", "v4", "projects",
    utils::URLencode(paste0(remote$username, "/", remote$repo),
                     reserved = TRUE),
    "repository")

  src <- paste0(
    src_root, "/files/",
    ifelse(
      is.null(remote$subdir),
      "DESCRIPTION",
      utils::URLencode(paste0(remote$subdir, "/DESCRIPTION"), reserved = TRUE)),
    "/raw?ref=", remote$ref, "&private_token=", remote$auth_token)

  dest <- tempfile()
  res <- download(dest, src, auth_token = remote$auth_token, auth_phrase = "private_token=")

  tryCatch(
    read_dcf(dest)$Package,
    error = function(e) remote$repo)
}

#' @export
remote_sha.gitlab_remote <- function(remote, ...) {
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
  download(tmp, url, auth_token = pat, auth_phrase = "private_token=")

  json$parse_file(tmp)$id
}

#' Retrieve GitLab personal access token.
#'
#' A GitLab personal access token
#' Looks in env var `GITLAB_PAT`
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
