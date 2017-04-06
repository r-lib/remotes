
`%||%` <- function (a, b) if (!is.null(a)) a else b

`%:::%` <- function (p, f) get(f, envir = asNamespace(p))

viapply <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, integer(1L), ..., USE.NAMES = USE.NAMES)
}

is_bioconductor <- function(x) {
  !is.null(x$biocviews)
}

trim_ws <- function(x) {
  gsub("^[[:space:]]+|[[:space:]]+$", "", x)
}

set_envvar <- function(envs) {
  if (length(envs) == 0) return()

  stopifnot(is.named(envs))

  old <- Sys.getenv(names(envs), names = TRUE, unset = NA)
  set <- !is.na(envs)

  both_set <- set & !is.na(old)

  if (any(set))  do.call("Sys.setenv", as.list(envs[set]))
  if (any(!set)) Sys.unsetenv(names(envs)[!set])

  invisible(old)
}

with_envvar <- function(new, code) {
  old <- set_envvar(new)
  on.exit(set_envvar(old))
  force(code)
}

is.named <- function(x) {
  !is.null(names(x)) && all(names(x) != "")
}

pkg_installed <- function(pkg) {

  if (pkg %in% loadedNamespaces()) {
    TRUE
  } else if (requireNamespace(pkg, quietly = TRUE)) {
    try(unloadNamespace(pkg))
    TRUE
  } else {
    FALSE
  }
}

has_package <- function(pkg) {
  if (pkg %in% loadedNamespaces()) {
    TRUE
  } else {
    requireNamespace(pkg, quietly = TRUE)
  }
}

with_something <- function(set, reset = set) {
  function(new, code) {
    old <- set(new)
    on.exit(reset(old))
    force(code)
  }
}

in_dir <- with_something(setwd)

get_r_version <- function() {
  paste(R.version$major, sep = ".", R.version$minor)
}

set_libpaths <- function(paths) {
  old <- .libPaths()
  .libPaths(paths)
  invisible(old)
}

with_libpaths <- with_something(set_libpaths, .libPaths)

## There are two kinds of tar on windows, one needs --force-local
## not to interpret : characters, the other does not. We try both ways.

untar <- function(tarfile, ...) {
  if (os_type() == "windows") {
    tryCatch(
      utils::untar(tarfile, extras = "--force-local", ...),
      error = function(e) utils::untar(tarfile, ...)
    )

  } else {
    utils::untar(tarfile, ...)
  }
}

os_type <- function() {
  .Platform$OS.type
}

sys_type <- function() {
  if (.Platform$OS.type == "windows") {
    "windows"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "macos"
  } else if (Sys.info()["sysname"] == "Linux") {
    "linux"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    stop("Unknown OS")
  }
}

is_dir <- function(path) {
  file.info(path)$isdir
}

untar_description <- function(tarball, dir = tempfile()) {
  files <- untar(tarball, list = TRUE)
  desc <- grep("^[^/]+/DESCRIPTION$", files, value = TRUE)
  if (length(desc) < 1) stop("No 'DESCRIPTION' file in package")
  untar(tarball, desc, exdir = dir)
  file.path(dir, desc)
}
