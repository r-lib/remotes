
install <- function(pkg, dependencies = NA, quiet = TRUE, ...) {
  install.packages(
    pkg,
    repos = NULL,
    dependencies = dependencies,
    quiet = quiet,
    type = "source",
    ...
  )

  invisible(TRUE)
}
