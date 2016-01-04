
`%||%` <- function (a, b) if (!is.null(a)) a else b

is_bioconductor <- function(x) {
  !is.null(x$biocviews)
}

trim_ws <- function(x) {
  gsub("^[[:space:]]+|[[:space:]]+$", "", x)
}
