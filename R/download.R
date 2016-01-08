
download <- function(path, url, auth_token, quiet = TRUE) {

  real_url <- url
  if (!is.null(auth_token)) {
    sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
    real_url <- paste0(url, sep, "access_token=", auth_token)
  }

  status <- utils::download.file(
    real_url,
    path,
    method = download_method(),
    quiet = quiet,
    mode = "wb"
  )

  if (status != 0)  stop("Cannot download file from ", url, call. = FALSE)

  path
}

download_method <- function() {

  if (isTRUE(unname(capabilities("libcurl")))) {
    "libcurl"

  } else if (os_type() == "windows") {
    "wininet"

  } else {
    "auto"
  }
}

os_type <- function() {
  .Platform$OS.type
}
