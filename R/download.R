
#' @importFrom utils compareVersion

download <- function(path, url, auth_token = NULL, basic_auth = NULL,
                     quiet = TRUE, auth_phrase = "access_token=",
                     headers = NULL) {

  real_url <- url

  if (!is.null(basic_auth)) {
    userpass <- paste0(basic_auth$user, ":", basic_auth$password)
    auth <- paste("Basic", base64_encode(charToRaw(userpass)))
    headers <- c(headers, Authorization = auth)
  }

  if (!is.null(auth_token)) {
    sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
    tkn <- if (grepl("=$", auth_phrase)) auth_phrase else paste0(auth_phrase, "=")
    real_url <- paste0(url, sep, tkn, auth_token)
  }

  if (compareVersion(get_r_version(), "3.2.0") == -1) {
    curl_download(real_url, path, quiet, headers)

  } else {

    base_download(real_url, path, quiet, headers)
  }

  path
 }

base_download <- function(url, path, quiet, headers) {

  if (!is.null(headers)) {
    unlockBinding("makeUserAgent", asNamespace("utils"))
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
