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
#'   hostname, for example, `"<PROTOCOL://>gitlab.hostname.com"`.
#'   The PROTOCOL is required by packrat during RStudio Connect deployment. While
#'   \link{install_gitlab} may work without, omitting it generally
#'   leads to package restoration errors.
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

  remotes <- lapply(repo, gitlab_remote, subdir = subdir, auth_token = auth_token, host = host, ...)

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

gitlab_remote <- function(repo, subdir = NULL,
                          auth_token = gitlab_pat(), sha = NULL,
                          host = "gitlab.com", ..., 
                          git_fallback = getOption("remotes.gitlab_git_fallback", TRUE),
                          quiet = FALSE) {

  meta <- parse_git_repo(repo)
  meta$ref <- meta$ref %||% "HEAD"

  # use project id api request as a canary for api access using auth_token.
  project_id <- try(silent = TRUE, {
    gitlab_project_id(meta$username, repo, meta$ref, host, auth_token)
  })

  has_access_token <- !is.null(auth_token) && nchar(auth_token) > 0L
  if (inherits(project_id, "try-error") && isTRUE(git_fallback)) {
    if (has_access_token && !quiet) {
      message(wrap(exdent = 2L, paste0("auth_token does not have scopes ", 
        "'read-repository' and 'api' for host '", host, "' required to ",
        "install using gitlab_remote.")))
    } else if (!quiet) {
      message(wrap(exdent = 2L, paste0("Unable to establish api access for ",
        "host '", host, "' required to install using gitlab_remote.")))
    }

    gitlab_to_git_remote(
      repo = repo,
      subdir = subdir,      
      auth_token = auth_token,
      sha = sha, 
      host = host,
      quiet = quiet,
      ...
    )
  } else {
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
}

gitlab_to_git_remote <- function(repo, subdir = NULL,
                                 auth_token = gitlab_pat(), sha = NULL,
                                 host = "gitlab.com", ..., 
                                 git_fallback = getOption("remotes.gitlab_git_fallback", TRUE),
                                 credentials = NULL,
                                 quiet = FALSE) {

  # for basic http auth, required names are largely undocumented:
  #   - in GitLab CI using job account, username must be "gitlab-ci-token"
  #   - for Project Access Tokens, username must be "<project-name>"
  #   - for Personal Access Tokens, username is ignored
  #
  # choose to use "gitlab-ci-token" for most general default behavior
  # https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html
  
  url <- paste0(build_url(host, repo), ".git")
  url_has_embedded_token <- grepl("^(.*://)?[^@/]+@", url)
  has_access_token <- !is.null(auth_token) && nchar(auth_token) > 0L
  has_credentials <- !is.null(credentials)
  use_git2r <- !is_standalone() && pkg_installed("git2r")

  if (url_has_embedded_token || has_credentials) {
    if (!quiet)
      message(wrap(exdent = 2L, paste0("Attempting git_remote")))
  } else if (has_access_token && !has_credentials && use_git2r) {
    if (!quiet)
      message(wrap(exdent = 2L, paste0("Attempting git_remote using ",
        "credentials: username='gitlab-ci-token', password=<auth_token>")))

    credentials <- getExportedValue("git2r", "cred_user_pass")(
      username = "gitlab-ci-token",
      password = auth_token
    )    
  } else if (has_access_token && !has_credentials && !use_git2r) {
    url_protocol <- gsub("((.*)://)?.*", "\\1", url)
    url_path     <- gsub("((.*)://)?", "", url)
    url <- paste0(url_protocol, "gitlab-ci-token:", auth_token, "@", url_path)

    if (!quiet)
      message(wrap(exdent = 2L, paste0("Attempting git_remote using ",
        sprintf("url=%sgitlab-ci-token:<auth_token>@%s", url_protocol, url_path))))
  }

  git_remote(
    url = url,
    subdir = subdir,
    ref = sha %||% meta$ref,
    credentials = credentials,
    ...
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

auth_token_has_gitlab_api_access <- function(host = "gitlab.com", pat) {
  # use the /version endpoint - general access endpoint with small payload, but 
  # inaccessible to CI tokens
  url <- build_url(host, "api", "v4", "version")
  has_access <- tryCatch({
    download(tempfile(), url, headers = c("Private-Token" = pat))
    TRUE
  }, error = function(e) {
    FALSE
  })
  has_access
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
