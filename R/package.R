
load_pkg_description <- function(path) {

  path <- normalizePath(path)

  if (!is_dir(path)) {
    dir <- tempfile()
    path_desc <- untar_description(path, dir = dir)
    on.exit(unlink(dir, recursive = TRUE))

  } else {
    path_desc <- file.path(path, "DESCRIPTION")
  }

  desc <- read_dcf(path_desc)
  names(desc) <- tolower(names(desc))
  desc$path <- path

  desc
}
