#' Install a package from R-Universe
#'
#' `install_runiverse()` retrieves the canonical universe for a CRAN package
#' using the R-Universe API,
#' then downloads and installs the package from that universe.
#' If the package has a `Remotes` field,
#' dependencies will be installed first from the appropriate remote repositories
#' using the same function.
#'
#' @param package The package name to install.
#' @param universe The R-Universe to use, infer from the package if `NULL`.
#' @return A character vector of the names of installed packages, invisibly.
#' @family package installation
#' @export
#' @examples
#' \dontrun{
#' # From GitHub
#' install_runiverse("dplyr")
#'
#' # From GitLab
#' install_runiverse("iemiscdata")
#'
#' # From Bitbucket
#' install_runiverse("argparser")
#' }
install_runiverse <- function(package, universe = NULL, ...) {
  if (is.null(universe)) {
    universe <- get_runiverse_for_package(package)
  } else if (length(universe) != 1 || !is.character(universe)) {
    stop("'universe' must be a single string", call. = FALSE)
  }

  # https://github.com/r-lib/remotes/issues/618#issuecomment-3333533114
  repo <- paste0("https://", universe, ".r-universe.dev/", package)

  tempdir <- tempfile("remotes")
  dir.create(tempdir)
  on.exit(unlink(tempdir, recursive = TRUE), add = TRUE)

  # available.packages() does not work for the repo
  download <- utils::download.packages(package, destdir = tempdir, repos = repo)[, 2]

  untar_success <- utils::untar(download, file.path(package, "DESCRIPTION"), exdir = tempdir)
  if (untar_success != 0) {
    stop("Failed to extract package DESCRIPTION from downloaded tarball", call. = FALSE)
  }

  desc_path <- file.path(tempdir, package, "DESCRIPTION")
  desc <- read_dcf(desc_path)

  installed <- character()

  if (!is.null(desc$Remotes)) {
    message("Installing dependencies from Remotes field: ", desc$Remotes)

    remotes <- strsplit(desc$Remotes, "[ \n]*,[ \n]*")[[1]]
    org_pkg <- re_match(remotes, "^(?:github::)?(?<org>[^/:]+)/(?<pkg>[^/@#]+)$")

    for (i in seq_len(nrow(org_pkg))) {
      if (is.na(org_pkg$.match[[i]])) {
        install_remote(org_pkg$.text[[i]])
      } else {
        install_runiverse(org_pkg$pkg[[i]], universe = org_pkg$org[[i]], ...)
      }
    }
  }

  # We already downloaded but can't provide a correct `type` argument
  install_cran(package, repos = repo)
}

get_runiverse_for_package <- function(package) {
  # Can't use httr2, only curl
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Package 'curl' is required to install from R-Universe", call. = FALSE)
  }

  handle <- curl::new_handle()
  curl::handle_setheaders(handle, `User-Agent` = "r-lib/remotes")

  packages_raw <- curl::curl_fetch_memory(
    paste0("https://r-universe.dev/api/search?q=package:", package)
  )

  packages <- json$parse(rawToChar(packages_raw$content))

  results <- packages$results
  if (length(results) == 0) {
    stop(sprintf("Package '%s' not found on R-Universe", package), call. = FALSE)
  }

  packages$results[[1]]$"_user"
}
