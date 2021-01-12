gitab_GET <- function(path, ..., host = "gitlab.com", pat = gitlab_pat(), use_curl = !is_standalone() && pkg_installed("curl")) {

  url <- build_url(host, "api", "v4", path)

  if (isTRUE(use_curl)) {
    h <- curl::new_handle()
    headers <- c(
      if (!is.null(pat)) {
        c("Authorization" = paste0("token ", pat))
      }
    )
    curl::handle_setheaders(h, .list = headers)
    res <- curl::curl_fetch_memory(url, handle = h)

    if (res$status_code >= 300) {
      stop("Error downloading from gitlab")
    }
    json$parse(raw_to_char_utf8(res$content))
  } else {
    tmp <- tempfile()
    download(tmp, url, auth_token = pat)

    json$parse_file(tmp)
  }
}