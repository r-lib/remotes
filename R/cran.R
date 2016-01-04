
available_packages <- function(repos, type) {
  suppressWarnings(available.packages(contrib.url(repos, type), type = type))
}
