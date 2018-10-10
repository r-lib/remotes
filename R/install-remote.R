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
install_remote <- function(remote,
                           dependencies = dependencies,
                           upgrade = upgrade,
                           force = force,
                           quiet = quiet,
                           build = build,
                           build_opts = build_opts,
                           repos = repos,
                           type = type,
                           ...) {

  stopifnot(is.remote(remote))

  package_name <- remote_package_name(remote)
  local_sha <- local_sha(package_name)
  remote_sha <- remote_sha(remote, local_sha)

  if (!isTRUE(force) &&
    !different_sha(remote_sha = remote_sha, local_sha = local_sha)) {

    if (!quiet) {
      message(
        "Skipping install of '", package_name, "' from a ", sub("_remote", "", class(remote)[1L]), " remote,",
        " the SHA1 (", substr(remote_sha, 1L, 8L), ") has not changed since last install.\n",
        "  Use `force = TRUE` to force installation")
    }
    return(invisible(package_name))
  }

  if (inherits(remote, "cran_remote")) {
    install_packages(
      package_name, repos = remote$repos, type = remote$pkg_type,
      ..., quiet = quiet)
    return(invisible(package_name))
  }

  bundle <- remote_download(remote, quiet = quiet)
  on.exit(unlink(bundle), add = TRUE)

  source <- source_pkg(bundle, subdir = remote$subdir)
  on.exit(unlink(source, recursive = TRUE), add = TRUE)

  update_submodules(source, quiet)

  add_metadata(source, remote_metadata(remote, bundle, source, remote_sha))

  # Because we've modified DESCRIPTION, its original MD5 value is wrong
  clear_description_md5(source)

  install(source,
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

install_remotes <- function(remotes, ...) {
  invisible(vapply(remotes, install_remote, ..., FUN.VALUE = character(1)))
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
remote_metadata <- function(x, bundle = NULL, source = NULL, sha = NULL) UseMethod("remote_metadata")
remote_package_name <- function(remote, ...) UseMethod("remote_package_name")
remote_sha <- function(remote, ...) UseMethod("remote_sha")

remote_package_name.default <- function(remote, ...) remote$repo
remote_sha.default <- function(remote, ...) NA_character_

different_sha <- function(remote_sha, local_sha) {

  same <- remote_sha == local_sha
  same <- isTRUE(same) && !is.na(same)
  !same
}

local_sha <- function(name) {
  package2remote(name)$sha %||% NA_character_
}

# Convert an installed package to its equivalent remote. This constructs the
# remote from metadata stored in the package's DESCRIPTION file; the metadata
# is added to the package when it is installed by remotes. If the package is
# installed some other way, such as by `install.packages()` there will be no
# meta-data, so there we construct a generic CRAN remote.
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

  if (is.null(x$RemoteType) || x$RemoteType == "cran") {

    # Packages installed with install.packages() or locally without remotes
    return(remote("cran",
        name = x$Package,
        repos = repos,
        pkg_type = type,
        sha = x$Version))
  }

  switch(x$RemoteType,
    github = remote("github",
      host = x$RemoteHost,
      package = x$RemotePackage,
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
    xgit = remote("xgit",
      url = trim_ws(x$RemoteUrl),
      ref = x$RemoteRef %||% x$RemoteBranch,
      sha = x$RemoteSha,
      subdir = x$RemoteSubdir,
      args = x$RemoteArgs),
    git2r = remote("git2r",
      url = trim_ws(x$RemoteUrl),
      ref = x$RemoteRef %||% x$RemoteBranch,
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
      url = trim_ws(x$RemoteUrl),
      svn_subdir = x$RemoteSubdir,
      revision = x$RemoteSha,
      args = x$RemoteArgs),
    local = remote("local",
      path = trim_ws(x$RemoteUrl),
      subdir = x$RemoteSubdir,
      sha = {
        # Packages installed locally might have RemoteSha == NA_character_
        x$RemoteSha %||% x$Version
      }),
    url = remote("url",
      url = trim_ws(x$RemoteUrl),
      subdir = x$RemoteSubdir,
      config = x$RemoteConfig,
      pkg_type = x$RemotePkgType),
    bioc_git2r = remote("bioc_git2r",
      mirror = x$RemoteMirror,
      repo = x$RemoteRepo,
      release = x$RemoteRelease,
      sha = x$RemoteSha,
      branch = x$RemoteBranch),
    bioc_xgit = remote("bioc_xgit",
      mirror = x$RemoteMirror,
      repo = x$RemoteRepo,
      release = x$RemoteRelease,
      sha = x$RemoteSha,
      branch = x$RemoteBranch)
  )
}

#' @export
format.remotes <- function(x, ...) {
  vapply(x, format, character(1))
}
