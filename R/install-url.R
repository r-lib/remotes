
#' Install a package from a url
#'
#' This function is vectorised so you can install multiple packages in
#' a single command.
#'
#' @param url location of package on internet. The url should point to a
#'   zip file, a tar file or a bzipped/gzipped tar file.
#' @param subdir subdirectory within url bundle that contains the R package.
#' @param ... Other arguments passed on to \code{\link[utils]{install.packages}}.
#' @export
#'
#' @examples
#' \dontrun{
#' install_url("https://github.com/hadley/stringr/archive/master.zip")
#' }

install_url <- function(url, subdir = NULL, ...) {
  remotes <- lapply(url, url_remote, subdir = subdir)
  install_remotes(remotes, ...)
}

url_remote <- function(url, subdir = NULL) {
  remote("url",
    url = url,
    subdir = subdir
  )
}

#' @importFrom tools file_ext
#' @export
remote_download.url_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading package from url: ", x$url)
  }

  ext <- if (grepl("\\.tar\\.gz$", x$url)) "tar.gz" else file_ext(x$url)

  bundle <- tempfile(fileext = paste0(".", ext))
  download(bundle, x$url)
}

#' @export
remote_metadata.url_remote <- function(x, bundle = NULL, source = NULL) {
  list(
    RemoteType = "url",
    RemoteUrl = x$url,
    RemoteSubdir = x$subdir
  )
}

#' @export
remote_package_name.url_remote <- function(remote, ...) {
  NA_character_
}

#' @export
remote_sha.url_remote <- function(remote, ...) {
  NA_character_
}

#' @export
format.url_remote <- function(x, ...) {
  "URL"
}
