
#' Download a file
#'
#' Uses either the curl package for R versions older than 3.2.0,
#' otherwise a wrapper around [download.file()].
#'
#' We respect the `download.file.method` setting of the user. If it is
#' not set, then see `download_method()` for choosing a method.
#'
#' Authentication can be supplied three ways:
#' * By setting `auth_token`. This will append an HTTP `Authorization`
#'   header: `Authorization: token {auth_token}`.
#' * By setting `basic_auth` to a list with elements `user` and `password`.
#'   This will append a proper `Authorization: Basic {encoded_password}`
#'   HTTP header.
#' * By specifying the proper `headers` directly.
#'
#' If both `auth_token` and `basic_auth` are specified, that's an error.
#' `auth_token` and `basic_auth` are _appended_ to `headers`, so they
#' take precedence over an `Authorization` header that is specified
#' directly in `headers`.
#'
#' @param path Path to download to. `dirname(path)` must exist.
#' @param url URL.
#' @param auth_token Token for token-based authentication or `NULL`.
#' @param basic_auth List with `user` and `password` for basic HTTP
#'   authentication, or `NULL`.
#' @param quiet Passed to [download.file()] or [curl::curl_download()].
#' @param headers Named character vector of HTTP headers to use.
#' @return `path`, if the download was successful.
#'
#' @keywords internal
#' @importFrom utils compareVersion

download <- function(path, url, auth_token = NULL, basic_auth = NULL,
                     quiet = TRUE, headers = NULL) {

  if (!is.null(basic_auth) && !is.null(auth_token)) {
    stop("Cannot use both Basic and Token authentication at the same time")
  }

  if (!is.null(basic_auth)) {
    userpass <- paste0(basic_auth$user, ":", basic_auth$password)
    auth <- paste("Basic", base64_encode(charToRaw(userpass)))
    headers <- c(headers, Authorization = auth)
  }

  if (!is.null(auth_token)) {
    headers <- c(headers, Authorization = paste("token", auth_token))
  }

  if (getRversion() < "3.2.0") {
    curl_download(url, path, quiet, headers)

  } else {

    base_download(url, path, quiet, headers)
  }

  path
 }

base_download <- function(url, path, quiet, headers) {

  method <- download_method()

  status <- if (method == "wget") {
    base_download_wget(url, path, quiet, headers)
  } else if (method =="curl") {
    base_download_curl(url, path, quiet, headers)
  } else if (getRversion() < "3.6.0") {
    base_download_noheaders(url, path, quiet, headers, method)
  } else {
    base_download_headers(url, path, quiet, headers, method)
  }

  if (status != 0) stop("Cannot download file from ", url, call. = FALSE)

  path
}

base_download_wget <- function(url, path, quiet, headers) {

  extra <- getOption("download.file.extra")

  if (length(headers)) {
    qh <- shQuote(paste0(names(headers), ": ", headers))
    extra <- c(extra, paste0("--header=", qh))
  }

  with_options(
    list(download.file.extra = extra),
    suppressWarnings(
      utils::download.file(
        url,
        path,
        method = "wget",
        quiet = quiet,
        mode = "wb",
        extra = extra
      )
    )
  )
}

base_download_curl <- function(url, path, quiet, headers) {

  extra <- getOption("download.file.extra")

  if (length(headers)) {
    qh <- shQuote(paste0(names(headers), ": ", headers))
    extra <- c(extra, paste("-H", qh))
  }

  with_options(
    list(download.file.extra = extra),
    suppressWarnings(
      utils::download.file(
        url,
        path,
        method = "curl",
        quiet = quiet,
        mode = "wb",
        extra = extra
      )
    )
  )
}

base_download_noheaders <- function(url, path, quiet, headers, method) {

  if (length(headers)) {

    if (method == "wininet" && getRversion() < "3.6.0") {
      warning(paste(
        "R (< 3.6.0) cannot send HTTP headers with the `wininet` download method.",
        "This download will likely fail. Please choose a different download",
        "method, via the `download.file.method` option. The `libcurl` method is",
        "best, if available, and the `wget` and `curl` methods work as well,",
        "if the corresponding external tool is available. See `?download.file`"))
    }

    get("unlockBinding", baseenv())("makeUserAgent", asNamespace("utils"))
    orig <- get("makeUserAgent", envir = asNamespace("utils"))
    on.exit({
      assign("makeUserAgent", orig, envir = asNamespace("utils"))
      lockBinding("makeUserAgent", asNamespace("utils"))
    }, add = TRUE)
    ua <- orig(FALSE)

    flathead <- paste0(names(headers), ": ", headers, collapse = "\r\n")
    agent <- paste0(ua, "\r\n", flathead)
    assign(
      "makeUserAgent",
      envir = asNamespace("utils"),
      function(format = TRUE) {
        if (format) {
            paste0("User-Agent: ", agent, "\r\n")
        } else {
          agent
        }
      })
  }

  suppressWarnings(
    utils::download.file(
      url,
      path,
      method = method,
      quiet = quiet,
      mode = "wb"
    )
  )
}

base_download_headers <- function(url, path, quiet, headers, method) {
  suppressWarnings(
    utils::download.file(
      url,
      path,
      method = method,
      quiet = quiet,
      mode = "wb",
      headers = headers
    )
  )
}

has_curl <- function() isTRUE(unname(capabilities("libcurl")))

download_method <- function() {

  user_option <- getOption("download.file.method")

  if (!is.null(user_option)) {
    ## The user wants what the user wants
    user_option

  } else if (has_curl()) {
    ## If we have libcurl, it is usually the best option
    "libcurl"

  } else if (compareVersion(get_r_version(), "3.3") == -1 &&
             os_type() == "windows") {
    ## Before 3.3 we select wininet on Windows
    "wininet"

  } else {
    ## Otherwise this is probably hopeless, but let R select, and
    ##  try something
    "auto"
  }
}

curl_download <- function(url, path, quiet, headers) {

  if (!pkg_installed("curl")) {
    stop("The 'curl' package is required if R is older than 3.2.0")
  }

  handle <- curl::new_handle()
  if (!is.null(headers)) curl::handle_setheaders(handle, .list = headers)
  curl::curl_download(url, path, quiet = quiet, mode = "wb", handle = handle)
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
    # only done before R 3.3
    if (utils::compareVersion(get_r_version(), "3.3") == -1) {
      # if internal then see if were using windows internal with inet2
      identical(Sys.info()[["sysname"]], "Windows") && utils::setInternet2(NA)
    } else {
      FALSE
    }
  } else {
    # method with unknown properties (e.g. "lynx") or unresolved auto
    FALSE
  }
}
