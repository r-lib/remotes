
load_pkg_description <- function(path) {
  path <- normalizePath(path)
  path_desc <- file.path(path, "DESCRIPTION")

  desc <- read_dcf(path_desc)
  names(desc) <- tolower(names(desc))
  desc$path <- path

  desc
}
