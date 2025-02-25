#' Install the development version of a package
#'
#' @description
#' `install_dev()` retrieves the package DESCRIPTION from the CRAN mirror and
#' looks in the 'URL' and 'BugReports' fields for GitHub, GitLab or Bitbucket
#' URLs. It then calls the appropriate `install_()` function to install the
#' development package.
#'
#' `can_install_dev()` checks if the development version can be installed.
#'
#' @param package The package name to install.
#' @param cran_url The URL of the CRAN mirror to use, by default based on the
#'   'repos' option. If unset uses 'https://cloud.r-project.org'.
#' @param ... Additional arguments passed to [install_github()],
#'   [install_gitlab()], or [install_bitbucket()] functions.
#' @family package installation
#' @export
#' @name install_dev
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
install_dev <- function(package, cran_url = getOption("repos")[["CRAN"]]) {
  if (is.null(cran_url) || identical(cran_url, "@CRAN@")) {
    cran_url <- "https://cloud.r-project.org"
  }

  refs <- dev_split_ref(package)
  url <- build_url(cran_url, "web", "packages", refs[["pkg"]], "DESCRIPTION")

  f <- tempfile()
  on.exit(unlink(f))

  download(f, url)
  desc <- read_dcf(f)

  url_fields <- c(desc$URL, desc$BugReports)

  if (length(url_fields) == 0) {
    return(FALSE)
  }

  pkg_urls <- unlist(strsplit(url_fields, "[[:space:]]*,[[:space:]]*"))

  # Remove trailing "/issues" from the BugReports URL
  pkg_urls <- sub("/issues/?$", "", pkg_urls)

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

  # Can install if we have exactly one valid part
  nrow(parts) == 1
}

#' @export
install_dev <- function(package, cran_url = getOption("repos")[["CRAN"]], ...) {
  if (!can_install_dev(package, cran_url)) {
    stop("Could not determine development repository", call. = FALSE)
  }

  full_ref <- paste0(
    paste0(c(parts$username, parts$repo, if (nzchar(parts$subdir)) parts$subdir), collapse = "/"),
    refs[["ref"]]
  )

  switch(parts$domain,
    github.com = install_github(full_ref, ...),
    gitlab.com = install_gitlab(full_ref, ...),
    bitbucket.org = install_bitbucket(full_ref, ...)
  )
}
