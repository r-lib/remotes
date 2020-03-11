

#' @export
#' @rdname bioc_install_repos
#' @keywords internal
#' @examples
#' bioc_version()
#' bioc_version("3.4")

bioc_version <- function(r_ver = getRversion()) {
  bioconductor$get_bioc_version(r_ver)
}

#' Tools for Bioconductor repositories
#'
#' `bioc_version()` returns the Bioconductor version for the current or the
#' specified R version.
#'
#' `bioc_install_repos()` deduces the URLs of the Bioconductor repositories.
#'
#' @details
#' Both functions observe the `R_BIOC_VERSION` environment variable, which
#' can be set to force a Bioconductor version. If this is set, then the
#' `r_ver` and `bioc_ver` arguments are ignored.
#'
#' `bioc_install_repos()` observes the `R_BIOC_MIRROR` environment variable
#' and also the `BioC_mirror` option, which can be set to the desired
#' Bioconductor mirror. The option takes precedence if both are set. Its
#' default value is `https://bioconductor.org`.
#'
#' @return
#' `bioc_version()` returns a Bioconductor version, a `package_version`
#' object.
#'
#' `bioc_install_repos()` returns a named character vector of the URLs of
#' the Bioconductor repositories, appropriate for the current or the
#' specified R version.
#'
#' @param r_ver R version to use. For `bioc_install_repos()` it is
#'   ignored if `bioc_ver` is specified.
#' @param bioc_ver Bioconductor version to use. Defaults to the default one
#'   corresponding to `r_ver`.
#'
#' @export
#' @keywords internal
#' @examples
#' bioc_install_repos()

bioc_install_repos <- function(r_ver = getRversion(),
                               bioc_ver = bioc_version(r_ver)) {
  bioconductor$get_repos(bioc_ver)
}
