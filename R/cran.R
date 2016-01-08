
available_packages <- function(repos, type) {
  suppressWarnings(utils::available.packages(utils::contrib.url(repos, type), type = type))
}
