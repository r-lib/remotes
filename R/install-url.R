
#' Install a package from a url
#'
#' This function is vectorised so you can install multiple packages in
#' a single command.
#'
#' @param url location of package on internet. The url should point to a
#'   zip file, a tar file or a bzipped/gzipped tar file.
#' @param subdir subdirectory within url bundle that contains the R package.
#' @param ... Other arguments passed on to [utils::install.packages()].
#' @inheritParams install_github
#' @export
#'
#' @family package installation
#' @examples
#' \dontrun{
#' install_url("https://github.com/hadley/stringr/archive/master.zip")
#' }

install_url <- function(url, subdir = NULL,
                        dependencies = NA,
                        upgrade = c("default", "ask", "always", "never"),
                        force = FALSE,
                        quiet = FALSE,
                        build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                        repos = getOption("repos"),
                        type = getOption("pkgType"),
                        ...) {
  remotes <- lapply(url, url_remote, subdir = subdir, quiet = quiet)
  install_remotes(remotes,
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

url_remote <- function(url, subdir = NULL, quiet = FALSE, ...) {

  # mimic local_remote

  # download and keep in "private" directory
  if (!quiet) {
    message("Downloading package from url: ", url) # nocov
  }

  ext <- if (grepl("\\.tar\\.gz$", url)) "tar.gz" else file_ext(url)

  bundle <- tempfile(fileext = paste0(".", ext))
  download(bundle, url)

  # decompress, returning path to "decompressed" directory
  path <- decompress(bundle, tempdir())

  remote("url",
    url = url,
    subdir = subdir,
    path = path
  )
}

#' @importFrom tools file_ext
#' @export
remote_download.url_remote <- function(x, quiet = FALSE) {
  remote_download.local_remote(x, quiet = quiet)
}

#' @export
remote_metadata.url_remote <- function(x, bundle = NULL, source = NULL, sha = NULL) {
  list(
    RemoteType = "url",
    RemoteUrl = x$url,
    RemoteSubdir = x$subdir
  )
}

#' @export
remote_package_name.url_remote <- function(remote, ...) {
  remote_package_name.local_remote(remote, ...)
}

#' @export
remote_sha.url_remote <- function(remote, ...) {
  if (is.null(remote$path)) {
    return(NA_character_)
  }
  remote_sha.local_remote(remote, ...)
}

#' @export
format.url_remote <- function(x, ...) {
  "URL"
}
