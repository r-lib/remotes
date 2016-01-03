
download <- function(path, url, auth) {
  ## TODO: auth
  download.file(url, path, method = download_method())
}

download_method <- function() {

  if (.Platform$OS.type == "windows") {
    "wininet"

  } else if (isTRUE(unname(capabilities("libcurl")))) {
    "libcurl"

  } else if (Sys.which("wget") != "") {
    "wget"

  } else {
    "auto"
  }
}
