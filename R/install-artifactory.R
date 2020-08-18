
# What do we need for artifactory?
# user name
# API key
# URL
# repo

# Only support source installs for V1

#' Attempts to install a package directly from GitHub.
#'
#' This function is vectorised on `repo` so you can install multiple
#' packages in a single command.
#'
#' @param pkgs Repository address in the format
#'   `username/repo[/subdir][@@ref|#pull|@@*release]`. Alternatively, you can
#'   specify `subdir` and/or `ref` using the respective parameters
#'   (see below); if both are specified, the values in `repo` take
#'   precedence.
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
install_artifactory <- function(pkgs,
                                repo = artifactory_repo(quiet),
                                auth_user = artifactory_user(quiet),
                                auth_key = artifactory_key(quiet),
                                force = FALSE,
                                dependencies = NA,
                                upgrade = c("default", "ask", "always", "never"),
                                quiet = FALSE,
                                build = TRUE,
                                build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                                build_manual = FALSE,
                                build_vignettes = FALSE,
                                repos = getOption("repos"),
                                type = getOption("pkgType"),
                                ...) {

  message("install_artifactory: ", paste0(pkgs, collapse = ", "))

  remotes <- lapply(pkgs, artifactory_remote, repo = repo, user = auth_user, key = auth_key)

  install_remotes(
    remotes,
    host = host,
    basic_auth = list(user = auth_user, key = auth_key),
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

#' Create a new github_remote
#'
#' This is an internal function to create a new github_remote, users should
#' generally have no need for it.
#' @inheritParams install_github
#' @export
#' @keywords internal
artifactory_remote <- function(package,
                               repo = artifactory_repo(),
                               user = artifactory_user(),
                               key = artifactory_key()) {

  message("artifactory_remote: ", package, sprintf(" (%s / %s : %s)", repo, user, key))

  basic_auth <- list(user = user, password = key)

  info <- artifactory_package_info(
    pkg = package,
    repo_url = build_url(repo, "src", "contrib"),
    basic_auth = basic_auth
  )

  message("info: ", paste0(info, collapse = " // "))

  remote(
    "artifactory",
    package = package,
    version = info$version,
    host = info$url,
    basic_auth = list(user = user, password = key)
  )
}

#' @export
remote_download.artifactory_remote <- function(x, quiet = FALSE) {
  dest <- tempfile(fileext = ".tar.gz")
  if (!quiet) {
    message(sprintf(
      "Downloading package \"%s\" from Artifactory.\nURL: %s",
      x$package,
      x$host
    ))
  }
  download(dest, x$host, basic_auth = x$basic_auth)
}

#' @export
remote_package_name.artifactory_remote <- function(remote, ...) {
  remote$package
}

#'  @export
remote_sha.artifactory_remote <- function(remote, ...) {
  NULL
}

#' @export
remote_metadata.artifactory_remote <- function(x, bundle = NULL, source = NULL, sha = NULL) {
  list(
    RemoteType = "artifactory",
    RemoteHost = x$host,
    RemoteSha = x$version
  )
}


#' Set Artifactory environmental variables.
#'
#' DFGHJKL
#'
#' @param quiet XXSD
#' @describeIn artifactory_repo AAA
#' @export
artifactory_repo <- function(quiet = TRUE) {
  artifactory_envvar("ARTIFACTORY_REPO", "repo", quiet)
}

#' @describeIn artifactory_repo XXX
#' @export
artifactory_user <- function(quiet = TRUE) {
  artifactory_envvar("ARTIFACTORY_USER", "user name", quiet)
}

#' @describeIn artifactory_repo YYY
#' @export
artifactory_key <- function(quiet = TRUE) {
  artifactory_envvar("ARTIFACTORY_KEY", "API key", quiet)
}

#' Set Artifactory environmental variables.
#'
#' Adapted from [gitlab_pat()].
#'
#' @param key Environment variable name.
#' @param desc Short description, e.g., "user name."
#' @param quiet Print status message.
#' @keywords internal
artifactory_envvar <- function(key, desc, quiet = TRUE) {
  envvar <- Sys.getenv(key)
  if (nzchar(envvar)) {
    if (!quiet) message("Using Artifactory ", desc, " from env var ", key)
    return(envvar)
  }
  return(NULL)
}


artifactory_package_info <- function(pkg, repo_url, basic_auth = NULL) {
  repolist <- download(tempfile(), repo_url, basic_auth = basic_auth)
  repolist <- readLines(repolist, warn = FALSE)

  # Lines with the package name
  matches <- grep(pkg, repolist, fixed = TRUE, value = TRUE)
  matches <- gsub(paste0("^.*(", pkg, "_.*?\\.tar\\.gz).*$"), "\\1", matches, perl = TRUE)

  # Check number of matches

  # Get version
  chomp <- sub(".tar.gz", "", matches, fixed = T)
  chomp <- sub(paste0(pkg, "_"), "", chomp, fixed = T)

  list(package = pkg, version = chomp, url = build_url(repo_url, matches))
}







#
# https://elderresearch.jfrog.io/artifactory/cran-local/bin/macosx/el-capitan/contrib/3.6/erilang_0.0.0.9000.tgz
#
#


