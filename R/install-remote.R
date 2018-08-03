#' Install a remote package.
#'
#' This:
#' \enumerate{
#'   \item downloads source bundle
#'   \item decompresses & checks that it's a package
#'   \item adds metadata to DESCRIPTION
#'   \item calls install
#' }
#' @noRd
install_remote <- function(remote, ..., force = FALSE, quiet = FALSE) {
  stopifnot(is.remote(remote))

  remote_sha <- remote_sha(remote)
  package_name <- remote_package_name(remote)
  local_sha <- local_sha(package_name)

  if (!isTRUE(force) &&
    !different_sha(remote_sha = remote_sha, local_sha = local_sha)) {

    if (!quiet) {
      message(
        "Skipping install of '", package_name, "' from a ", sub("_remote", "", class(remote)[1L]), " remote,",
        " the SHA1 (", substr(remote_sha, 1L, 8L), ") has not changed since last install.\n",
        "  Use `force = TRUE` to force installation")
    }
    return(invisible(FALSE))
  }

  if (inherits(remote, "cran_remote")) {
    install_packages(
      package_name, repos = remote$repos, type = remote$pkg_type,
      ..., quiet = quiet)
    return(invisible(TRUE))
  }

  bundle <- remote_download(remote, quiet = quiet)
  on.exit(unlink(bundle), add = TRUE)

  source <- source_pkg(bundle, subdir = remote$subdir)
  on.exit(unlink(source, recursive = TRUE), add = TRUE)

  add_metadata(source, remote_metadata(remote, bundle, source))

  # Because we've modified DESCRIPTION, its original MD5 value is wrong
  clear_description_md5(source)

  install(source, ..., quiet = quiet)
}

install_remotes <- function(remotes, ...) {
  invisible(vapply(remotes, install_remote, ..., FUN.VALUE = logical(1)))
}

# Add metadata
add_metadata <- function(pkg_path, meta) {
  path <- file.path(pkg_path, "DESCRIPTION")
  desc <- read_dcf(path)

  desc <- utils::modifyList(desc, meta)

  write_dcf(path, desc)
}

# Modify the MD5 file - remove the line for DESCRIPTION
clear_description_md5 <- function(pkg_path) {
  path <- file.path(pkg_path, "MD5")

  if (file.exists(path)) {
    text <- readLines(path)
    text <- text[!grepl(".*\\*DESCRIPTION$", text)]

    writeLines(text, path)
  }
}

remote <- function(type, ...) {
  structure(list(...), class = c(paste0(type, "_remote"), "remote"))
}
is.remote <- function(x) inherits(x, "remote")

remote_download <- function(x, quiet = FALSE) UseMethod("remote_download")
remote_metadata <- function(x, bundle = NULL, source = NULL) UseMethod("remote_metadata")
remote_package_name <- function(remote, ...) UseMethod("remote_package_name")
remote_sha <- function(remote, ...) UseMethod("remote_sha")

remote_package_name.default <- function(remote, ...) remote$repo
remote_sha.default <- function(remote, ...) NA_character_

different_sha <- function(remote_sha = NULL,
                          local_sha = NULL) {
  if (is.null(remote_sha)) {
    remote_sha <- remote_sha(remote)
  }

  if (is.null(local_sha)) {
    local_sha <- local_sha(remote_package_name(remote))
  }

  same <- remote_sha == local_sha
  same <- isTRUE(same) && !is.na(same)
  !same
}

local_sha <- function(name) {
  if (!is_installed(name)) {
    return(NA_character_)
  }
  package2remote(name)$sha %||% NA_character_
}

package2remote <- function(name, lib = .libPaths(), repos = getOption("repos"), type = getOption("pkgType")) {

  x <- tryCatch(utils::packageDescription(name, lib.loc = lib), error = function(e) NA, warning = function(e) NA)

  # will be NA if not installed
  if (identical(x, NA)) {
    return(remote("cran",
        name = name,
        repos = repos,
        pkg_type = type,
        sha = NA_character_))
  }

  if (is.null(x$RemoteType)) {

    # Packages installed with install.packages() or locally without devtools
    return(remote("cran",
        name = x$Package,
        repos = repos,
        pkg_type = type,
        sha = x$Version))
  }

  switch(x$RemoteType,
    github = remote("github",
      host = x$RemoteHost,
      repo = x$RemoteRepo,
      subdir = x$RemoteSubdir,
      username = x$RemoteUsername,
      ref = x$RemoteRef,
      sha = x$RemoteSha),
    gitlab = remote("gitlab",
      host = x$RemoteHost,
      repo = x$RemoteRepo,
      subdir = x$RemoteSubdir,
      username = x$RemoteUsername,
      ref = x$RemoteRef,
      sha = x$RemoteSha),
    git = remote("git",
      url = x$RemoteUrl,
      ref = x$RemoteRef,
      sha = x$RemoteSha,
      subdir = x$RemoteSubdir),
    bitbucket = remote("bitbucket",
      host = x$RemoteHost,
      repo = x$RemoteRepo,
      username = x$RemoteUsername,
      ref = x$RemoteRef,
      sha = x$RemoteSha,
      subdir = x$RemoteSubdir),
    svn = remote("svn",
      url = x$RemoteUrl,
      svn_subdir = x$RemoteSubdir,
      revision = x$RemoteSha,
      args = x$RemoteArgs),
    local = remote("local",
      path = x$RemoteUrl,
      subdir = x$RemoteSubdir,
      sha = {
        # Packages installed locally might have RemoteSha == NA_character_
        x$RemoteSha %||% x$Version
      }),
    url = remote("url",
      url = x$RemoteUrl,
      subdir = x$RemoteSubdir,
      config = x$RemoteConfig,
      pkg_type = x$RemotePkgType),
    bioc = remote("bioc",
      repo = x$RemoteRepo,
      mirror = x$RemoteMirror,
      release = x$RemoteRelease,
      username = x$RemoteUsername,
      password = x$RemotePassword,
      revision = x$RemoteRevision,
      sha = x$RemoteSha),

    # packages installed with install_cran
    cran = remote("cran",
      name = x$Package,
      repos = eval(parse(text = x$RemoteRepos)),
      pkg_type = x$RemotePkgType,
      sha = x$RemoteSha))
}

#' @export
format.remotes <- function(x, ...) {
  vapply(x, format, character(1))
}

#' @export
`[.remotes` <- function(x,i,...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}
