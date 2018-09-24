
#' Install specified version of a CRAN package.
#'
#' If you are installing an package that contains compiled code, you will
#' need to have an R development environment installed.  You can check
#' if you do by running `devtools::has_devel` (you need the
#' `devtools` package for this).
#'
#' @export
#' @family package installation
#' @param package package name
#' @param version If the specified version is NULL or the same as the most
#'   recent version of the package, this function simply calls
#'   [utils::install.packages()]. Otherwise, it looks at the list of
#'   archived source tarballs and tries to install an older version instead.
#' @param ... Other arguments passed on to [utils::install.packages()].
#' @inheritParams utils::install.packages
#' @inheritParams install_github
#' @author Jeremy Stephens
#' @importFrom utils available.packages contrib.url install.packages

install_version <- function(package, version = NULL,
                            dependencies = NA,
                            upgrade = FALSE,
                            force = FALSE,
                            quiet = FALSE,
                            build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                            repos = getOption("repos"),
                            type = getOption("pkgType"),
                            ...) {

  url <- download_version_url(package, version, repos, type)
  install_url(url,
              dependencies = dependencies,
              upgrade = upgrade,
              force = force,
              quiet = quiet,
              build = build,
              build_opts = build_opts,
              repos = repos,
              type = type,
              ...)
}

package_find_repo <- function(package, repos) {
  for (repo in repos) {
    if (length(repos) > 1)
      message("Trying ", repo)

    archive <-
      tryCatch({
        con <- gzcon(url(sprintf("%s/src/contrib/Meta/archive.rds", repo), "rb"))
        on.exit(close(con))
        readRDS(con)
      },
      warning = function(e) list(),
      error = function(e) list())

    info <- archive[[package]]
    if (!is.null(info)) {
      info$repo <- repo
      return(info)
    }
  }

  stop(sprintf("couldn't find package '%s'", package))
}


#' Download a specified version of a CRAN package
#'
#' It downloads the package to a temporary file, and
#' returns the name of the file.
#'
#' @inheritParams install_version
#' @return Name of the downloaded file.
#'
#' @export

download_version <- function(package, version = NULL,
                             repos = getOption("repos"),
                             type = getOption("pkgType"), ...) {

  url <- download_version_url(package, version, repos, type)
  download(path = tempfile(), url = url)
}

download_version_url <- function(package, version, repos, type) {

  contriburl <- contrib.url(repos, type)
  available <- available.packages(contriburl)

  if (package %in% row.names(available)) {
    current.version <- available[package, 'Version']
    if (is.null(version) || version == current.version) {
      row <- available[which(rownames(available) == package)[1], ]
      return(paste0(
        row[["Repository"]],
        "/",
        row[["Package"]],
        "_",
        row[["Version"]],
        ".tar.gz"
      ))
    }
  }

  info <- package_find_repo(package, repos)

  if (is.null(version)) {
    # Grab the latest one: only happens if pulled from CRAN
    package.path <- row.names(info)[nrow(info)]
  } else {
    package.path <- paste(package, "/", package, "_", version, ".tar.gz",
      sep = "")
    if (!(package.path %in% row.names(info))) {
      stop(sprintf("version '%s' is invalid for package '%s'", version,
        package))
    }
  }

  paste(info$repo[1L], "/src/contrib/Archive/", package.path, sep = "")
}
