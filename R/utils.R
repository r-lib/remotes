
`%||%` <- function (a, b) if (!is.null(a)) a else b

is_bioconductor <- function(x) {
  !is.null(x$biocviews)
}

trim_ws <- function(x) {
  gsub("^[[:space:]]+|[[:space:]]+$", "", x)
}

with_envvar <- function (new, code) {
  old <- set_envvar(envs = new)
  on.exit(set_envvar(old))
  force(code)
}

set_envvar <- function (envs) {
  if (length(envs) == 0) return()

  stopifnot(is.named(envs))

  envs <- envs[!duplicated(names(envs), fromLast = TRUE)]
  old <- Sys.getenv(names(envs), names = TRUE, unset = NA)
  set <- !is.na(envs)
  if (any(set))
    do.call("Sys.setenv", as.list(envs[set]))
  if (any(!set))
    Sys.unsetenv(names(envs)[!set])
  invisible(old)
}

is.named <- function (x) {
  !is.null(names(x)) && all(names(x) != "")
}
