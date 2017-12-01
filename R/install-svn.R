
#' Install a package from a SVN repository
#'
#' This function requires \command{svn} to be installed on your system in order to
#' be used.
#'
#' It is vectorised so you can install multiple packages with
#' a single command.
#'
#' @inheritParams install_git
#' @param subdir A sub-directory withing a svn repository that contains the
#'   package we are interested in installing.
#' @param args A character vector providing extra options to pass on to
#'   \command{svn}.
#' @param revision svn revision, if omitted updates to latest
#' @param ... Other arguments passed on to \code{\link[utils]{install.packages}}.
#' @export
#'
#' @examples
#' \dontrun{
#' install_svn("svn://github.com/hadley/stringr/trunk")
#' install_svn("svn://github.com/hadley/httr/branches/oauth")
#'}
install_svn <- function(url, subdir = NULL, args = character(0),
  ..., revision = NULL) {

  remotes <- lapply(url, svn_remote, svn_subdir = subdir,
    revision = revision, args = args)

  install_remotes(remotes, ...)
}

svn_remote <- function(url, svn_subdir = NULL, revision = revision,
  args = character(0)) {
  remote("svn",
    url = url,
    svn_subdir = svn_subdir,
    revision = revision,
    args = args
  )
}

#' @export
remote_download.svn_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading svn repo ", x$url)
  }

  bundle <- tempfile()
  svn_binary_path <- svn_path()
  url <- x$url
  args <- "export"
  if (!is.null(x$revision))
    args <- paste("-r", x$revision, args)
  if (!is.null(x$svn_subdir)) {
    url <- file.path(url, x$svn_subdir);
  }
  args <- c(x$args, args, url, bundle)

  message(shQuote(svn_binary_path), " ", paste0(args, collapse = " "))
  request <- system2(svn_binary_path, args, stdout = FALSE, stderr = FALSE)

  # This is only looking for an error code above 0-success
  if (request > 0) {
    stop("There seems to be a problem retrieving this SVN-URL.", call. = FALSE)
  }

  bundle
}

#' @export
remote_metadata.svn_remote <- function(x, bundle = NULL, source = NULL) {
  list(
    RemoteType = "svn",
    RemoteUrl = x$url,
    RemoteSubdir = x$subdir,
    RemoteArgs = if (length(x$args) > 0) paste0(deparse(x$args), collapse = " ")
  )
}

svn_path <- function(svn_binary_name = NULL) {
  # Use user supplied path
  if (!is.null(svn_binary_name)) {
    if (!file.exists(svn_binary_name)) {
      stop("Path ", svn_binary_name, " does not exist", .call = FALSE)
    }
    return(svn_binary_name)
  }

  # Look on path
  svn_path <- Sys.which("svn")[[1]]
  if (svn_path != "") return(svn_path)

  # On Windows, look in common locations
  if (os_type() == "windows") {
    look_in <- c(
      "C:/Program Files/Svn/bin/svn.exe",
      "C:/Program Files (x86)/Svn/bin/svn.exe"
    )
    found <- file.exists(look_in)
    if (any(found)) return(look_in[found][1])
  }

  stop("SVN does not seem to be installed on your system.", call. = FALSE)
}
