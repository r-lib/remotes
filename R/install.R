
install <- function(pkg, dependencies = NA, quiet = TRUE, ...) {

  install_deps(pkg, dependencies = dependencies, quiet = quiet, ...)

  install.packages(
    pkg,
    repos = NULL,
    quiet = quiet,
    type = "source",
    ...
  )

  invisible(TRUE)
}

#' Install package dependencies if needed.
#'
#' @inheritParams install
#' @inheritParams package_deps
#' @param ... additional arguments passed to \code{\link{install.packages}}.
#' @export
#' @examples
#' \dontrun{install_deps(".")}
install_deps <- function(pkg = ".", dependencies = NA,
                         threads = getOption("Ncpus", 1),
                         repos = getOption("repos"),
                         type = getOption("pkgType"),
                         ...,
                         upgrade = TRUE,
                         quiet = FALSE) {

  pkg <- dev_package_deps(pkg, repos = repos, dependencies = dependencies,
    type = type)
  update(pkg, ..., Ncpus = threads, quiet = quiet, upgrade = upgrade)
}
