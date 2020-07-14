
#' Install specific version of a package.
#'
#' This function knows how to look in multiple CRAN-like package repositories, and in their
#' \code{archive} directories, in order to find specific versions of the requested package.
#'
#' The repositories are searched in the order specified by the \code{repos} argument.  This enables
#' teams to maintain multiple in-house repositories with different policies - for instance, one repo
#' for development snapshots and one for official releases.  A common setup would be to first search
#' the official release repo, then the dev snapshot repo, then a public CRAN mirror.
#'
#' Older versions of packages on CRAN are usually only available in source form.  If your requested
#' package contains compiled code, you will need to have an R development environment installed. You
#' can check if you do by running `devtools::has_devel` (you need the `devtools` package for this).
#'
#' @export
#' @family package installation
#' @param package Name of the package to install.
#' @param version Version of the package to install.  Can either be a string giving the exact
#'   version required, or a specification in the same format as the parenthesized expressions used
#'   in package dependencies. One of the following formats:
#'   - An exact version required, as a string, e.g. `"0.1.13"`
#'   - A comparison operator and a version, e.g. `">= 0.1.12"`
#'   - Several criteria to satisfy, as a comma-separated string, e.g. `">= 1.12.0, < 1.14"`
#'   - Several criteria to satisfy, as elements of a character vector, e.g. `c(">= 1.12.0", "< 1.14")`
#' @param ... Other arguments passed on to [utils::install.packages()].
#' @inheritParams utils::install.packages
#' @inheritParams install_github
#' @examples
#' \dontrun{
#' install_version("devtools", "1.11.0")
#' install_version("devtools", ">= 1.12.0, < 1.14")
#'
#' ## Specify search order (e.g. in ~/.Rprofile)
#' options(repos = c(
#'   prod = "http://mycompany.example.com/r-repo",
#'   dev = "http://mycompany.example.com/r-repo-dev",
#'   CRAN = "https://cran.revolutionanalytics.com"
#' ))
#' install_version("mypackage", "1.15") # finds in 'prod'
#' install_version("mypackage", "1.16-39487") # finds in 'dev'
#' }
#' @importFrom utils available.packages contrib.url install.packages

install_version <- function(package, version = NULL,
                            dependencies = NA,
                            upgrade = c("default", "ask", "always", "never"),
                            force = FALSE,
                            quiet = FALSE,
                            build = FALSE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                            build_manual = FALSE, build_vignettes = FALSE,
                            repos = getOption("repos"),
                            type = "source",
                            ...) {

  # TODO would it make sense to vectorize this, e.g. `install_version(c("foo", "bar"), c("1.1", "2.2"))`?
  if (length(package) < 1) {
    return()
  }
  if (length(package) > 1) {
    stop("install_version() must be called with a single 'package' argument - multiple packages given")
  }

  if (!identical(type, "source")) {
    stop("`type` must be 'source' for `install_version()`", call. = FALSE)
  }

  url <- download_version_url(package, version, repos, type)
  res <- install_url(url,
    dependencies = dependencies,
    upgrade = upgrade,
    force = force,
    quiet = quiet,
    build = build,
    build_opts = build_opts,
    build_manual = build_manual,
    build_vignettes = build_vignettes,
    repos = repos,
    type = type,
    ...
  )

  lib <- list(...)$lib %||% .libPaths()

  # Remove Metadata from installed package
  add_metadata(
    system.file(package = package, lib.loc = lib),
    list(RemoteType = NULL, RemoteUrl = NULL, RemoteSubdir = NULL)
  )

  invisible(res)
}

version_from_tarball <- function(tarball_name) {
  package_ver_regex <- paste0(".+_(", .standard_regexps()$valid_package_version, ")\\.tar\\.gz$")
  ifelse(grepl(package_ver_regex, tarball_name), sub(package_ver_regex, "\\1", tarball_name), NULL)
}

version_satisfies_criteria <- function(to_check, criteria) {
  to_check <- package_version(to_check)
  result <- apply(version_criteria(criteria), 1, function(r) {
    if (is.na(r["compare"])) {
      TRUE
    } else {
      get(r["compare"], mode = "function")(to_check, r["version"])
    }
  })
  all(result)
}

package_installed <- function(pkg, criteria) {
  v <- suppressWarnings(utils::packageDescription(pkg, fields = "Version"))
  !is.na(v) && version_satisfies_criteria(v, criteria)
}

version_criteria <- function(criteria) {
  if (is.character(criteria) && length(criteria) == 1) {
    criteria <- strsplit(criteria, ",")[[1]]
  }

  numeric_ver <- .standard_regexps()$valid_numeric_version

  package <- "p" # dummy package name, required by parse_deps()

  spec <- if (is.null(criteria) || (length(criteria) == 1 && is.na(criteria[[1L]]))) {
    package
  } else {
    ifelse(grepl(paste0("^", numeric_ver, "$"), criteria),
      paste0(package, "(== ", criteria, ")"),
      paste0(package, "(", criteria, ")")
    )
  }

  parse_deps(paste(spec, collapse = ", "))[c("compare", "version")]
}

# Find a given package record in the `archive.rds` file of a repository
package_find_archives <- function(package, repo, verbose = FALSE) {
  if (verbose) {
    message("Trying ", repo)
  }

  # TODO it would be nice to cache these downloaded files like `available.packages` does
  archive <-
    tryCatch(
      {
        tf <- tempfile(fileext = ".gz")
        on.exit(unlink(tf), add = TRUE)
        download(tf, sprintf("%s/src/contrib/Meta/archive.rds", repo))
        con <- gzfile(tf, "rb")
        on.exit(close(con), add = TRUE)
        readRDS(con)
      },
      warning = function(e) list(),
      error = function(e) list()
    )

  info <- archive[[package]]
  if (!is.null(info)) {
    info$repo <- repo
    return(info)
  }

  NULL
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

download_version_url <- function(package, version, repos, type, available, verbose = length(repos) > 1) {

  ## TODO should we do for(r in repos) { for (t in c('published','archive')) {...}}, or
  ## for (t in c('published','archive')) { for(r in repos) {...}} ? Right now it's the latter.  It
  ## only matters if required version is satisfied by both an early repo in archive/ and a late repo

  if (missing(available)) {
    contriburl <- contrib.url(repos, type)
    available <- available.packages(contriburl, filters = c("R_version", "OS_type", "subarch"))
  }

  package_exists <- FALSE

  # available.packages() returns a matrix with entries in the same order as the repositories in
  # `repos`, so the first packages we encounter should be preferred.
  for (ix in which(available[, "Package"] == package)) {
    package_exists <- TRUE
    row <- available[ix, ]
    if (version_satisfies_criteria(row["Version"], version)) {
      return(paste0(
        row["Repository"],
        "/",
        row["Package"],
        "_",
        row["Version"],
        ".tar.gz"
      ))
    }
  }

  for (repo in repos) {
    info <- package_find_archives(package, repo, verbose = verbose)
    if (is.null(info)) {
      next
    }

    package_exists <- TRUE

    for (i in rev(seq_len(nrow(info)))) {
      package_path <- row.names(info)[i]
      if (version_satisfies_criteria(version_from_tarball(package_path), version)) {
        return(file.path(repo, "src", "contrib", "Archive", package_path))
      }
    }
  }

  if (!package_exists) {
    stop(sprintf("couldn't find package '%s'", package))
  }

  stop(sprintf("version '%s' is invalid for package '%s'", version, package))
}
