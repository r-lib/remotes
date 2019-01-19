#' Install the development version of a package
#'
#' `install_dev()` retrieves the package DESCRIPTION from the CRAN mirror and
#' looks in the 'URL' and 'BugReports' fields for GitHub, GitLab or Bitbucket
#' URLs. It then calls the appropriate `install_()` function to install the
#' development package.
#'
#' @param package The package name to install.
#' @param cran_url The URL of the CRAN mirror to use, by default based on the
#'   'repos' option. If unset uses 'https://cloud.r-project.org'.
#' @param ... Additional arguments passed to [install_github()],
#'   [install_gitlab()], or [install_bitbucket()] functions.
#' @family package installation
#' @export
#' @examples
#' \dontrun{
#' # From GitHub
#' install_dev("dplyr")
#'
#' # From GitLab
#' install_dev("iemiscdata")
#'
#' # From Bitbucket
#' install_dev("argparser")
#' }
install_dev <- function(package, cran_url = getOption("repos")[["CRAN"]], ...) {
  if (is.null(cran_url) || identical(cran_url, "@CRAN@")) {
    cran_url <- "https://cloud.r-project.org"
  }

  # if @[ref] is included, save for repo install
  if (contains_ref(package)) {
    ref <- sub(".*(?=(@|#))", "", package, perl = TRUE)
    package <- sub("(#|@).*", "", package)
  } else {
    ref <- ""
  }

  url <- build_url(cran_url, "web", "packages", package, "DESCRIPTION")

  f <- tempfile()
  on.exit(unlink(f))

  download(f, url)
  desc <- read_dcf(f)

  url_fields <- c(desc$URL, desc$BugReports)

  if (length(url_fields) == 0) {
    stop("Could not determine development repository", call. = FALSE)
  }

  pkg_urls <- unlist(strsplit(url_fields, "[[:space:]]*,[[:space:]]*"))

  # Remove trailing "/issues" from the BugReports URL
  pkg_urls <- sub("/issues$", "", pkg_urls)

  valid_domains <- c("github[.]com", "gitlab[.]com", "bitbucket[.]org")

  parts <-
    re_match(pkg_urls,
      sprintf("^https?://(?<domain>%s)/(?<username>%s)/(?<repo>%s)(?:/(?<subdir>%s))?",
        domain = paste0(valid_domains, collapse = "|"),
        username = "[^/]+",
        repo = "[^/@#]+",
        subdir = "[^/@$ ]+"
      )
    )[c("domain", "username", "repo", "subdir")]

  # Remove cases which don't match and duplicates

  parts <- unique(stats::na.omit(parts))

  if (nrow(parts) != 1) {
    stop("Could not determine development repository", call. = FALSE)
  }

  ref <- paste0(
    paste0(c(parts$username, parts$repo, if (nzchar(parts$subdir)) parts$subdir), collapse = "/"),
    ref)

  switch(parts$domain,
    github.com = install_github(ref, ...),
    gitlab.com = install_gitlab(ref, ...),
    bitbucket.org = install_bitbucket(ref, ...)
  )
}
