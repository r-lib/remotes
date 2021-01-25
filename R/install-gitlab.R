#' Install a package from GitLab
#'
#' This function is vectorised on `repo` so you can install multiple
#' packages in a single command. Like other remotes the repository will skip
#' installation if `force == FALSE` (the default) and the remote state has
#' not changed since the previous installation.
#'
#' @inheritParams install_github
#' @param repo Repository address in the format
#'   `username/repo[@@ref]`.
#' @param host GitLab API host to use. Override with your GitLab enterprise
#'   hostname, for example, `"gitlab.hostname.com"`.
#' @param auth_token To install from a private repo, generate a personal access
#'   token (PAT) in \url{https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html} and
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
                           subdir = NULL,
                           auth_token = gitlab_pat(quiet),
                           host = "gitlab.com",
                           dependencies = NA,
                           upgrade = c("default", "ask", "always", "never"),
                           force = FALSE,
                           quiet = FALSE,
                           build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                           build_manual = FALSE, build_vignettes = FALSE,
                           repos = getOption("repos"),
                           type = getOption("pkgType"),
                           ...) {

  remotes <- lapply(repo, gitlab_remote, subdir = subdir, auth_token = auth_token, host = host)

  install_remotes(remotes, auth_token = auth_token, host = host,
                  dependencies = dependencies,
                  upgrade = upgrade,
                  force = force,
                  quiet = quiet,
                  build = build,
                  build_opts = build_opts,
                  build_manual = build_manual,
                  build_vignettes = build_vignettes,
                  repos = repos,
                  type = type,
                  ...)
}

gitlab_remote <- function(repo, ref = "HEAD", subdir = NULL,
                       auth_token = gitlab_pat(), sha = NULL,
                       host = "gitlab.com", ...) {

  meta <- parse_git_repo(repo)
  meta <- gitlab_resolve_ref(meta$ref %||% ref, meta, host = host, auth_token = auth_token)

  remote("gitlab",
    host = host,
    repo = paste(c(meta$repo, meta$subdir), collapse = "/"),
    subdir = subdir,
    username = meta$username,
    ref = meta$ref,
    sha = sha,
    auth_token = auth_token
  )
}

#' @export
remote_download.gitlab_remote <- function(x, quiet = FALSE) {
  dest <- tempfile(fileext = paste0(".tar.gz"))

  project_id <- gitlab_project_id(x$username, x$repo, x$ref, x$host, x$auth_token)

  src_root <- build_url(x$host, "api", "v4", "projects", project_id)
  src <- paste0(src_root, "/repository/archive.tar.gz?sha=", utils::URLencode(x$ref, reserved = TRUE))

  if (!quiet) {
    message("Downloading GitLab repo ", x$username, "/", x$repo, "@", x$ref,
            "\nfrom URL ", src)
  }

  download(dest, src, headers = c("Private-Token" = x$auth_token))
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

#' GitLab references
#'
#' Use as `ref` parameter to [install_gitlab()].
#' Allows installing a specific pull request or the latest release.
#'
#' @param pull The pull request to install
#' @seealso [install_gitlab()]
#' @rdname gitlab_refs
#' @export
gitlab_pull <- function(pull) structure(pull, class = "gitlab_pull")

#' @rdname gitlab_refs
#' @export
gitlab_release <- function() structure(NA_integer_, class = "gitlab_release")

gitlab_resolve_ref <- function(x, params, ...) UseMethod("gitlab_resolve_ref")

#' @export
gitlab_resolve_ref.default <- function(x, params, ...) {
  params$ref <- x
  params
}

#' @export
gitlab_resolve_ref.NULL <- function(x, params, ...) {
  params$ref <- "HEAD"
  params
}

#' @export
gitlab_resolve_ref.github_pull <- function(x, params, ..., host, auth_token = gitlab_pat()) {

}

# Retrieve the ref for the latest release
#' @export
gitlab_resolve_ref.github_release <- function(x, params, ..., host, auth_token = gitlab_pat()) {
  # GET /projects/:user%2F:repo
  path <- paste("projects",
    utils::URLencode(paste0(params$username, "/", params$repo), reserved = TRUE),
    sep = "/")
  response <- tryCatch(
    gitab_GET(path, host = host, pat = auth_token),
    error = function(e) e
  )

  # GET /projects/:id/:releases
  path <- paste("projects", response$id, "releases", sep = "/")
  response <- tryCatch(
    gitab_GET(path, host = host, pat = auth_token),
    error = function(e) e
  )

  if (methods::is(response, "error") || !is.null(response$message)) {
    stop("Cannot find repo ", params$username, "/", params$repo, ".", "\n",
      response$message)
  }

  if (length(response) == 0L)
    stop("No releases found for repo ", params$username, "/", params$repo, ".")

  params$ref <- response[[1L]]$tag_name
  params

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
    "/raw?ref=", utils::URLencode(remote$ref, reserved = TRUE))

  dest <- tempfile()
  res <- download(dest, src, headers = c("Private-Token" = remote$auth_token))

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

gitlab_commit <- function(username, repo, ref = "HEAD",
  host = "gitlab.com", pat = gitlab_pat()) {

  url <- build_url(host, "api", "v4", "projects", utils::URLencode(paste0(username, "/", repo), reserved = TRUE), "repository", "commits", utils::URLencode(ref, reserved = TRUE))

  tmp <- tempfile()
  download(tmp, url, headers = c("Private-Token" = pat))

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

gitlab_project_id <- function(username, repo, ref = "HEAD",
  host = "gitlab.com", pat = gitlab_pat()) {

  url <- build_url(host, "api", "v4", "projects", utils::URLencode(paste0(username, "/", repo), reserved = TRUE), "repository", "commits", utils::URLencode(ref, reserved = TRUE))

  tmp <- tempfile()
  download(tmp, url, headers = c("Private-Token" = pat))

  json$parse_file(tmp)$project_id
}
