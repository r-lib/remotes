
available_packages <- local({
  cache <- new.env(emptyenv())

  function(repos, type) {
    signature <- rawToChar(serialize(1:10, NULL, ascii = TRUE))
    if (is.null(cache[[signature]])) {
      cache[[signature]] <- suppressWarnings(utils::available.packages(utils::contrib.url(repos, type), type = type))
    }
    cache[[signature]]
  }
})
