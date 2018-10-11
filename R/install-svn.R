
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
#' @param ... Other arguments passed on to [utils::install.packages()].
#' @inheritParams install_github
#' @family package installation
#' @export
#'
#' @examples
#' \dontrun{
#' install_svn("https://github.com/hadley/stringr/trunk")
#' install_svn("https://github.com/hadley/httr/branches/oauth")
#'}
install_svn <- function(url, subdir = NULL, args = character(0),
                        revision = NULL,
                        dependencies = NA,
                        upgrade = c("ask", "always", "never"),
                        force = FALSE,
                        quiet = FALSE,
                        build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                        repos = getOption("repos"),
                        type = getOption("pkgType"),
                        ...) {

  remotes <- lapply(url, svn_remote, svn_subdir = subdir,
    revision = revision, args = args)

  install_remotes(remotes, args = args,
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

svn_remote <- function(url, svn_subdir = NULL, revision = NULL,
  args = character(0), ...) {
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

  args <- "co"
  if (!is.null(x$revision)) {
    args <- c(args, "-r", x$revision)
  }
  args <- c(args, x$args, full_svn_url(x), bundle)

  if (!quiet) { message(shQuote(svn_binary_path), " ", paste0(args, collapse = " ")) }
  request <- system2(svn_binary_path, args, stdout = FALSE, stderr = FALSE)

  # This is only looking for an error code above 0-success
  if (request > 0) {
    stop("There seems to be a problem retrieving this SVN-URL.", call. = FALSE)
  }

  in_dir(bundle, {
    if (!is.null(x$revision)) {
      request <- system2(svn_binary_path, paste("update -r", x$revision), stdout = FALSE, stderr = FALSE)
      if (request > 0) {
        stop("There was a problem switching to the requested SVN revision", call. = FALSE)
      }
    }
  })
  bundle
}

#' @export
remote_metadata.svn_remote <- function(x, bundle = NULL, source = NULL, sha = NULL) {

  if (!is.null(bundle)) {
    in_dir(bundle, {
      revision <- svn_revision()
    })
  } else {
    revision <- sha
  }

  list(
    RemoteType = "svn",
    RemoteUrl = x$url,
    RemoteSubdir = x$svn_subdir,
    RemoteArgs = if (length(x$args) > 0) paste0(deparse(x$args), collapse = " "),
    RemoteSha = revision # for compatibility with other remotes
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

#' @export
remote_package_name.svn_remote <- function(remote, ...) {
  description_url <- file.path(full_svn_url(remote), "DESCRIPTION")
  tmp_file <- tempfile()
  on.exit(rm(tmp_file))
  response <- system2(svn_path(), paste("cat", description_url), stdout = tmp_file)
  if (!identical(response, 0L)) {
    return(NA_character_)
  }
  read_dcf(tmp_file)$Package
}

#' @export
remote_sha.svn_remote <- function(remote, ...) {
  svn_revision(full_svn_url(remote))
}

svn_revision <- function(url = NULL, svn_binary_path = svn_path()) {
  request <- system2(svn_binary_path, paste("info --xml", url), stdout = TRUE)
  if (!is.null(attr(request, "status")) && !identical(attr(request, "status"), 0L)) {
    stop("There was a problem retrieving the current SVN revision", call. = FALSE)
  }
  gsub(".*<commit[[:space:]]+revision=\"([[:digit:]]+)\">.*", "\\1", paste(collapse = "\n", request))
}

full_svn_url <- function(x) {
  url <- x$url
  if (!is.null(x$svn_subdir)) {
    url <- file.path(url, x$svn_subdir)
  }

  url
}

format.svn_remote <- function(x, ...) {
  "SVN"
}
