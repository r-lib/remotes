
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
