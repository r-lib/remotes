
`%||%` <- function (a, b) if (!is.null(a)) a else b

`%:::%` <- function (p, f) get(f, envir = asNamespace(p))

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

untar <- function(tarfile, ...) {
  extras <- if (os_type() == "windows") "--force-local"
  utils::untar(tarfile, extras = extras, ...)
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
