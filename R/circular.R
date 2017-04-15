
## A environment to hold which packages are being installed so packages
## with circular dependencies can be skipped the second time.

installing <- new.env(parent = emptyenv())

is_root_install <- function() is.null(installing$packages)

exit_from_root_install <- function() installing$packages <- NULL

check_for_circular_dependencies <- function(pkgdir, quiet) {
  pkgdir <- normalizePath(pkgdir)
  pkg <- get_desc_field(file.path(pkgdir, "DESCRIPTION"), "Package")

  if (pkg %in% installing$packages) {
    if (!quiet) message("Skipping ", pkg, ", it is already being installed")
    TRUE

  } else {
    installing$packages <- c(installing$packages, pkg)
    FALSE
  }
}
