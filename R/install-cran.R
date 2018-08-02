
#' Attempts to install a package from CRAN.
#'
#' This function is vectorised on \code{pkgs} so you can install multiple
#' packages in a single command.
#'
#' @param pkgs Character vector of packages to install.
#' @inheritParams package_deps
#' @export
#' @family package installation
#' @examples
#' \dontrun{
#' install_cran("ggplot2")
#' install_cran(c("httpuv", "shiny"))
#' }
install_cran <- function(pkgs, repos = getOption("repos"), type = getOption("pkgType"), ..., quiet = FALSE) {

  remotes <- lapply(pkgs, cran_remote, repos = repos, type = type)

  install_remotes(remotes, quiet = quiet, ...)
}

cran_remote <- function(pkg, repos, type) {

  remote("cran",
    name = pkg,
    repos = repos,
    pkg_type = type)
}

#' @export
remote_package_name.cran_remote <- function(remote, ...) {
  remote$name
}

#' @export
remote_sha.cran_remote <- function(remote, url = "https://github.com", ...) {
  cran <- available_packages(remote$repos, remote$pkg_type)

  trim_ws(unname(cran[, "Version"][match(remote$name, rownames(cran))]))
}

#' @export
format.cran_remote <- function(x, ...) {
  "CRAN"
}
