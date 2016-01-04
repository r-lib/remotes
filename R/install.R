
install <- function(pkgdir, dependencies = NA, quiet = TRUE, ...) {

  install_deps(pkgdir, dependencies = dependencies, quiet = quiet, ...)

  install.packages(
    pkgdir,
    repos = NULL,
    quiet = quiet,
    type = "source",
    ...
  )

  invisible(TRUE)
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

  update(packages, ..., Ncpus = threads, quiet = quiet, upgrade = upgrade)
}
