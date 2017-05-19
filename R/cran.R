
#' @importFrom utils available.packages

available_packages <- memoise::memoise(function(repos, type) {
  available.packages(contrib.url(repos, type), type = type)
})
