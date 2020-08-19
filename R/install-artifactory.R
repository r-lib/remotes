#' Attempt to install package(s) from an Artifactory repo.
#'
#' This function is vectorised on `pkgs` to allow installing
#' multiple packages with a single command.
#'
#' @param pkgs One or more package names to install.
#' @param repo Artifactory repo URL, e.g., `"company.jfrog.io/artifactory/cran-local"`.
#' @param auth_key Artifactory API key.
#' @param force Force installation, even if the remote state has not changed
#'   since the previous install.
#' @inheritParams install_deps
#' @param ... Other arguments passed on to [utils::install.packages()].
install_artifactory <- function(pkgs,
                                repo = artifactory_repo(quiet),
                                auth_token = artifactory_key(quiet),
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

  remotes <- lapply(pkgs, artifactory_remote, repo = repo, auth_token = auth_token)

  install_remotes(
    remotes,
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

#' Create a new Artifactory remote
#'
#' This is an internal function to create a new `artifactory_remote`.
#' @inheritParams install_artifactory
#' @export
#' @keywords internal
artifactory_remote <- function(package,
                               repo = artifactory_repo(),
                               auth_token = artifactory_key()) {

  # Source-only for now
  details <- artifactory_package_metadata(
    pkgname = package,
    repo = repo,
    key = auth_token,
    type = "source"
  )

  if (is.null(details)) {
    stop(sprintf('Package "%s" not found in Artifactory.', package))
  }

  remote(
    "artifactory",
    package = details$properties$cran.name,
    repo = repo,
    url = details$downloadUri,
    sha = details$checksums$sha1,
    api_key = auth_token
  )
}

#' @export
format.artifactory_remote <- function(x, ...) {
  "Artifactory"
}

#' @export
remote_download.artifactory_remote <- function(x, quiet = FALSE) {
  dest <- tempfile(fileext = ".tar.gz")
  if (!quiet) {
    message(sprintf('Downloading package "%s" from Artifactory.', x$package))
    message(sprintf("URL: %s", x$url))
  }
  download(dest, x$url, headers = list("X-JFrog-Art-Api" = x$api_key))
}

#' @export
remote_package_name.artifactory_remote <- function(remote, ...) {
  remote$package
}

#'  @export
remote_sha.artifactory_remote <- function(remote, ...) {
  remote$sha
}

#' @export
remote_metadata.artifactory_remote <- function(x, bundle = NULL, source = NULL, sha = NULL) {
  list(
    RemoteType = "artifactory",
    RemoteRepo = x$repo,
    RemoteSha = x$sha
  )
}


#' Fetch package metadata using Artifactory API.
#'
#' @param pkgname The remote package name.
#' @param repo Artifactory repository URL (assumed `"<host>/<repo>"`).
#' @param key Artifactory API key.
#' @param type Package type (only `"source"` allowed for now).
#'
#' @return A list of package metadata.
#' @keywords internal
artifactory_package_metadata <- function(pkgname,
                                         repo  = artifactory_repo(),
                                         key = artifactory_key(),
                                         type = "source") {

  type <- match.arg(type)

  if (!has_package("jsonlite")) {
    stop('Package "jsonlite" is required for Artifactory remotes')
  }

  # Separate the host from the repo
  url_parts <- unlist(strsplit(sub("^.*?://", "", repo), "/", fixed = TRUE))
  url_host <- build_url(parts[1:(length(parts) - 1)])
  url_repo <- tail(parts, 1)

  api_url <- build_url(
    url_host, "api", "search",
    sprintf("artifact?name=%s&repo=%s", pkgname, url_repo)
  )

  jsonfile <- tempfile(fileext = ".json")

  download(jsonfile, api_url,
    headers = list(
      "X-JFrog-Art-Api" = key,
      "X-Result-Detail" = "info,properties"
    )
  )

  payload <- jsonlite::read_json(jsonfile)[["results"]]

  result <- Filter(function(x) grepl("src/contrib", x$path, fixed = TRUE), payload)
  result <- Filter(function(x) x$properties$cran.name == pkgname, result)
  result <- unlist(result, recursive = FALSE)

  result
}


#' Set Artifactory environmental variables.
#'
#' Attempt to set Artifactory URL and authentication parameters
#' from the environment.
#'
#' @param quiet Do not show any status messages.
#' @describeIn artifactory_repo Artifactory repository URL (`ARTIFACTORY_REPO`).
#' @export
artifactory_repo <- function(quiet = TRUE) {
  artifactory_envvar("ARTIFACTORY_REPO", "repo", quiet)
}

#' @describeIn artifactory_repo Artifactory API key (`ARTIFACTORY_KEY`).
#' @export
artifactory_key <- function(quiet = TRUE) {
  artifactory_envvar("ARTIFACTORY_KEY", "API key", quiet)
}

#' Set Artifactory environmental variables.
#'
#' Adapted from [gitlab_pat()].
#'
#' @param key Environment variable name.
#' @param desc Short description, e.g., "API key."
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
