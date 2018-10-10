install_dev <- function(pkg, ...) {
  url <- build_url("crandb.r-pkg.org", pkg)

  f <- tempfile()
  on.exit(unlink(f))

  download(f, url)
  url_field <- fromJSONFile(f)$URL

  pkg_urls <- strsplit(url_field, "[[:space:]]*,[[:space:]]*", perl = TRUE)[[1]]

  parts <- na.omit(re_match(pkg_urls, "^https?://(?<domain>[^/]+)/(?<username>[^/]+)/(?<repo>[^/]+)"))

  if (nrow(parts) != 1) {
    stop("Could not determine development repository", call. = FALSE)
  }

  switch(parts$domain,
    github.com = install_github(paste0(parts$username, "/", parts$repo), ...),
    gitlab.com = install_gitlab(paste0(parts$username, "/", parts$repo), ...),
    bitbucket.com = install_bitbucket(paste0(parts$username, "/", parts$repo), ...)
  )
}
