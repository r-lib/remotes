#' Attempts to install a package directly from GitHub.
#'
#' This function is vectorised on `repo` so you can install multiple
#' packages in a single command.
#'
#' @param repo Repository address in the format
#'   `username/repo[/subdir][@@ref|#pull]`. Alternatively, you can
#'   specify `subdir` and/or `ref` using the respective parameters
#'   (see below); if both is specified, the values in `repo` take
#'   precedence.
#' @param ref Desired git reference. Could be a commit, tag, or branch
#'   name, or a call to [github_pull()]. Defaults to `"master"`.
#' @param subdir subdirectory within repo that contains the R package.
#' @param auth_token To install from a private repo, generate a personal
#'   access token (PAT) in "https://github.com/settings/tokens" and
#'   supply to this argument. This is safer than using a password because
#'   you can easily delete a PAT without affecting any others. Defaults to
#'   the `GITHUB_PAT` environment variable.
#' @param host GitHub API host to use. Override with your GitHub enterprise
#'   hostname, for example, `"github.hostname.com/api/v3"`.
#' @param force Force installation, even if the remote state has not changed
#'   since the previous install.
#' @inheritParams install_deps
#' @param ... Other arguments passed on to [utils::install.packages()].
#' @details
#' If the repository uses submodules a command-line git client is required to
#' clone the submodules.
#' @family package installation
#' @export
#' @seealso [github_pull()]
#' @examples
#' \dontrun{
#' install_github("klutometis/roxygen")
#' install_github("wch/ggplot2")
#' install_github(c("rstudio/httpuv", "rstudio/shiny"))
#' install_github(c("hadley/httr@@v0.4", "klutometis/roxygen#142",
#'   "mfrasca/r-logging/pkg"))
#'
#' # To install from a private repo, use auth_token with a token
#' # from https://github.com/settings/tokens. You only need the
#' # repo scope. Best practice is to save your PAT in env var called
#' # GITHUB_PAT.
#' install_github("hadley/private", auth_token = "abc")
#'
#' }
install_github <- function(repo,
                           ref = "master",
                           subdir = NULL,
                           auth_token = github_pat(quiet),
                           host = "api.github.com",
                           dependencies = NA,
                           upgrade = c("default", "ask", "always", "never"),
                           force = FALSE,
                           quiet = FALSE,
                           build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                           build_manual = FALSE, build_vignettes = FALSE,
                           repos = getOption("repos"),
                           type = getOption("pkgType"),
                           ...) {

  remotes <- lapply(repo, github_remote, ref = ref,
    subdir = subdir, auth_token = auth_token, host = host)

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

github_remote <- function(repo, ref = "master", subdir = NULL,
                       auth_token = github_pat(), sha = NULL,
                       host = "api.github.com", ...) {

  meta <- parse_git_repo(repo)
  meta <- github_resolve_ref(meta$ref %||% ref, meta, host = host, auth_token = auth_token)

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
#' Use as `ref` parameter to [install_github()].
#' Allows installing a specific pull request or the latest release.
#'
#' @param pull The pull request to install
#' @seealso [install_github()]
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
github_resolve_ref.github_pull <- function(x, params, ..., host, auth_token = github_pat()) {
  # GET /repos/:user/:repo/pulls/:number
  path <- file.path("repos", params$username, params$repo, "pulls", x)
  response <- tryCatch(
    github_GET(path, host = host, pat = auth_token),
    error = function(e) e
  )

  ## Just because libcurl might download the error page...
  if (methods::is(response, "error") || is.null(response$head)) {
    stop("Cannot find GitHub pull request ", params$username, "/",
         params$repo, "#", x, "\n",
         response$message)
  }

  params$username <- response$head$user$login
  params$ref <- response$head$ref
  params
}

# Retrieve the ref for the latest release
#' @export
github_resolve_ref.github_release <- function(x, params, ..., host, auth_token = github_pat()) {
  # GET /repos/:user/:repo/releases
  path <- paste("repos", params$username, params$repo, "releases", sep = "/")
  response <- tryCatch(
    github_GET(path, host = host, pat = auth_token),
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
remote_package_name.github_remote <- function(remote, ..., use_local = TRUE,
  use_curl = !is_standalone() && pkg_installed("curl")) {

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
    pat = remote$auth_token %||% github_pat(), use_curl = use_curl)

  if (is.null(desc)) {
    return(NA_character_)
  }

  tmp <- tempfile()
  writeChar(desc, tmp)
  on.exit(unlink(tmp))

  read_dcf(tmp)$Package
}

#' @export
remote_sha.github_remote <- function(remote, ..., use_curl = !is_standalone() && pkg_installed("curl")) {
  tryCatch(
    github_commit(username = remote$username, repo = remote$repo,
      host = remote$host, ref = remote$ref, pat = remote$auth_token %||% github_pat(), use_curl = use_curl),

    # 422 errors most often occur when a branch or PR has been deleted, so we
    # ignore the error in this case
    http_422 = function(e) NA_character_
  )
}

#' @export
format.github_remote <- function(x, ...) {
  "GitHub"
}
