
install <- function(pkgdir = ".", dependencies = NA, quiet = TRUE, ...) {

  if (file.exists(file.path(pkgdir, "src")) && ! has_devel()) {
    missing_devel_warning(pkgdir)
  }

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

  if (has_package("crancache")) {
    i.p <- crancache::install_packages
  } else {
    i.p <- utils::install.packages
  }

  with_envvar(
    c(R_LIBS = lib,
      R_LIBS_USER = lib,
      R_LIBS_SITE = lib,
      R_PROFILE_USER = tempfile()),
    i.p(...)
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

install_deps <- function(pkgdir = ".", dependencies = NA,
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

  dep_deps <- if (isTRUE(dependencies)) NA else dependencies

  update(
    packages,
    dependencies = dep_deps,
    ...,
    Ncpus = threads,
    quiet = quiet,
    upgrade = upgrade
  )
}
