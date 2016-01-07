#' Install a package from a SVN repository
#'
#' This function requires \code{svn} to be installed on your system in order to
#' be used.
#'
#' It is vectorised so you can install multiple packages with
#' a single command.
#'
#' @inheritParams install_git
#' @param subdir A sub-directory withing a svn repository that may contain the
#'   package we are interested in installing. By default, this
#'   points to the 'trunk' directory.
#' @param args A character vector providing extra arguments to pass on to
#    svn.
#' @param revision svn revision, if omitted updates to latest
#' @param branch Name of branch or tag to use, if not trunk.
#' @param ... Other arguments passed on to \code{\link{install}}
#' @noRd
#' @family package installation
#' @examples
#' \dontrun{
#' install_svn("https://github.com/hadley/stringr")
#' install_svn("https://github.com/hadley/httr", branch = "oauth")
#'}
install_svn <- function(url, subdir = NULL, branch = NULL, args = character(0),
                        ..., revision = NULL) {
  ## TODO
}
