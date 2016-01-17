
install <- function(pkgdir, dependencies = NA, quiet = TRUE, ...) {

  install_deps(pkgdir, dependencies = dependencies, quiet = quiet, ...)

  safe_install_packages(
      pkgdir,
      repos = NULL,
      quiet = quiet,
      type = "source",
    ...
  )

  invisible(TRUE)
}

safe_install_packages <- function(...) {

  lib <- paste(.libPaths(), collapse = ":")

  with_envvar(
    c(R_LIBS = lib,
      R_LIBS_USER = lib,
      R_LIBS_SITE = lib,
      R_PROFILE_USER = tempfile()),
    utils::install.packages(...)
  )
}

#' Install package dependencies if needed.
#'
#' @inheritParams package_deps
#' @param threads Number of threads to start, passed to
#'   \code{install.packages} as \code{Ncpus}.
#' @param ... additional arguments passed to \code{\link{install.packages}}.
#' @export
#' @examples
#' \dontrun{install_deps(".")}

install_deps <- function(pkgdir, dependencies = NA,
                         threads = getOption("Ncpus", 1),
                         repos = getOption("repos"),
                         type = getOption("pkgType"),
                         ...,
                         upgrade = TRUE,
                         quiet = FALSE) {

  packages <- dev_package_deps(
    pkgdir,
    repos = repos,
    dependencies = dependencies,
    type = type
  )

  update(
    packages,
    dependencies = dependencies,
    ...,
    Ncpus = threads,
    quiet = quiet,
    upgrade = upgrade
  )
}
