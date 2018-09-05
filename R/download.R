
#' @importFrom utils compareVersion

download <- function(path, url, auth_token = NULL, basic_auth = NULL,
                     quiet = TRUE) {

  real_url <- url

  if (!is.null(basic_auth)) {
    str <- paste0("://", basic_auth$user, ":", basic_auth$password, "@")
    real_url <- sub("://", str, url)
  }

  if (!is.null(auth_token)) {
    sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
    real_url <- paste0(url, sep, "access_token=", auth_token)
  }

  if (compareVersion(get_r_version(), "3.2.0") == -1) {
    curl_download(real_url, path, quiet)

  } else {

    base_download(real_url, path, quiet)
  }

  path
 }

base_download <- function(url, path, quiet) {

  suppressWarnings(
    status <- utils::download.file(
      url,
      path,
      method = download_method(),
      quiet = quiet,
      mode = "wb"
    )
  )

  if (status != 0)  stop("Cannot download file from ", url, call. = FALSE)

  path
}

download_method <- function() {
  
  # R versions newer than 3.3.0 have correct default methods
  if (compareVersion(get_r_version(), "3.3") == -1) {
    
    if (os_type() == "windows") {
      "wininet"
      
    } else if (isTRUE(unname(capabilities("libcurl")))) {
      "libcurl"
      
    } else {
      "auto"
    }
    
  } else {
    "auto"
  }
}

curl_download <- function(url, path, quiet) {

  if (pkg_installed("curl")) {
    stop("The 'curl' package is required if R is older than 3.2.0")
  }

  curl::curl_download(url, path, quiet = quiet, mode = "wb")
}

true_download_method <- function(x) {
  if (identical(x, "auto")) {
    auto_download_method()
  } else {
    x
  }
}

auto_download_method <- function() {
  if (isTRUE(capabilities("libcurl"))) {
    "libcurl"
  } else if (isTRUE(capabilities("http/ftp"))) {
    "internal"
  } else if (nzchar(Sys.which("wget"))) {
    "wget"
  } else if (nzchar(Sys.which("curl"))) {
    "curl"
  } else {
    ""
  }
}

download_method_secure <- function() {
  method <- true_download_method(download_method())

  if (method %in% c("wininet", "libcurl", "wget", "curl")) {
    # known good methods
    TRUE
  } else if (identical(method, "internal")) {
    # if internal then see if were using windows internal with inet2
    identical(Sys.info()[["sysname"]], "Windows") && utils::setInternet2(NA)
  } else {
    # method with unknown properties (e.g. "lynx") or unresolved auto
    FALSE
  }
}

download_packages <- function (pkgs, destdir, repos = getOption("repos"),
    contriburl = utils::contrib.url(repos, type), type = getOption("pkgType"), versions, ...) {

  pool <- curl::new_pool()
  on.exit(lapply(curl::multi_list(pool), curl::multi_cancel), add = TRUE)

  save_package <- function(res) {
    pkg <- basename(res$url)
    if (res$status_code > 300) {
      stop("Request failed: ", res$url, ": ", rawToChar(res$content), call. = FALSE)
    }
    cat("downloaded ", pkg, ":", res$status_code, "\n", sep = "")
    writeBin(res$content, file.path(destdir, pkg))
  }

  pkg_url <- file.path(contriburl, paste0(pkgs, "_", versions, ".tgz"))
  for (p in pkg_url) {
    curl::curl_fetch_multi(p, save_package, pool = pool)
  }
  curl::multi_run(pool = pool)

  matrix(ncol = 2, c(pkgs, file.path(destdir, basename(pkg_url))))
}
