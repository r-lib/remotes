cache <- new.env(parent = emptyenv())

#' @rdname available_packages
#' @export
available_packages_set <- function(repos, type, db) {
  signature <- rawToChar(serialize(list(repos, type), NULL, ascii = TRUE))
  if (is.null(cache[[signature]])) {
    cache[[signature]] <- db
  }
  cache[[signature]]
}

#' @rdname available_packages
#' @export
available_packages_reset <- function() {
  rm(list = ls(envir = cache), envir = cache)
}

#' Simpler available.packages
#'
#' This is mostly equivalent to [utils::available.packages()] however it also
#' caches the full result. Additionally the cache can be assigned explicitly with
#' [available_packages_set()] and reset (cleared) with [available_packages_reset()].
#'
#' @inheritParams utils::available.packages
#' @keywords internal
#' @seealso [utils::available.packages()] for full documentation on the output format.
#' @export
available_packages <- function(repos = getOption("repos"), type = getOption("pkgType")) {
  if (getRversion() < '3.5.0') {
    # R 3.5.0 added ellipsis to utils::available.packages()
    available_packages_set(
      repos, type,
      suppressWarnings(utils::available.packages(utils::contrib.url(repos, type), type = type))
    )
  } else {
    headers <- getOption("remotes.download.headers")
    available_packages_set(
      repos, type,
      suppressWarnings(utils::available.packages(utils::contrib.url(repos, type), type = type, headers = headers))
    )
  }
}
