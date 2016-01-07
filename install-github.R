
function(...) {

  ## This is the code of the package, put in here by brew

  
available_packages <- function(repos, type) {
  suppressWarnings(available.packages(contrib.url(repos, type), type = type))
}
read_dcf <- function(path) {
  fields <- colnames(read.dcf(path))
  as.list(read.dcf(path, keep.white = fields)[1, ])
}

write_dcf <- function(path, desc) {
  write.dcf(
    rbind(unlist(desc)),
    file = path,
    keep.white = names(desc),
    indent = 0
  )
}
# Decompress pkg, if needed
source_pkg <- function(path, subdir = NULL, before_install = NULL) {
  if (!file.info(path)$isdir) {
    bundle <- path
    outdir <- tempfile(pattern = "remotes")
    dir.create(outdir)

    path <- decompress(path, outdir)
  } else {
    bundle <- NULL
  }

  pkg_path <- if (is.null(subdir)) path else file.path(path, subdir)

  # Check it's an R package
  if (!file.exists(file.path(pkg_path, "DESCRIPTION"))) {
    stop("Does not appear to be an R package (no DESCRIPTION)", call. = FALSE)
  }

  # Check configure is executable if present
  config_path <- file.path(pkg_path, "configure")
  if (file.exists(config_path)) {
    Sys.chmod(config_path, "777")
  }

  # Call before_install for bundles (if provided)
  if (!is.null(bundle) && !is.null(before_install))
    before_install(bundle, pkg_path)

  pkg_path
}


decompress <- function(src, target) {
  stopifnot(file.exists(src))

  if (grepl("\\.zip$", src)) {
    my_unzip(src, target)
    outdir <- getrootdir(as.vector(utils::unzip(src, list = TRUE)$Name))

  } else if (grepl("\\.tar$", src)) {
    utils::untar(src, exdir = target)
    outdir <- getrootdir(utils::untar(src, list = TRUE))

  } else if (grepl("\\.(tar\\.gz|tgz)$", src)) {
    utils::untar(src, exdir = target, compressed = "gzip")
    outdir <- getrootdir(utils::untar(src, compressed = "gzip", list = TRUE))

  } else if (grepl("\\.(tar\\.bz2|tbz)$", src)) {
    utils::untar(src, exdir = target, compressed = "bzip2")
    outdir <- getrootdir(utils::untar(src, compressed = "bzip2", list = TRUE))

  } else {
    ext <- gsub("^[^.]*\\.", "", src)
    stop("Don't know how to decompress files with extension ", ext,
      call. = FALSE)
  }

  file.path(target, outdir)
}


# Returns everything before the last slash in a filename
# getdir("path/to/file") returns "path/to"
# getdir("path/to/dir/") returns "path/to/dir"
getdir <- function(path)  sub("/[^/]*$", "", path)

# Given a list of files, returns the root (the topmost folder)
# getrootdir(c("path/to/file", "path/to/other/thing")) returns "path/to"
getrootdir <- function(file_list) {
  slashes <- nchar(gsub("[^/]", "", file_list))
  if (min(slashes) == 0) return("")

  getdir(file_list[which.min(slashes)])
}

my_unzip <- function(src, target, unzip = getOption("unzip")) {
  if (unzip == "internal") {
    return(utils::unzip(src, exdir = target))
  }

  args <- paste(
    "-oq", shQuote(src),
    "-d", shQuote(target)
  )

  system_check(unzip, args, quiet = TRUE)
}

package_deps <- function(packages, dependencies = NA,
                         repos = getOption("repos"),
                         type = getOption("pkgType")) {

  if (identical(type, "both")) {
    type <- "binary"
  }

  if (length(repos) == 0)
    repos <- character()

  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com"
  cran <- available_packages(repos, type)

  deps <- sort(find_deps(packages, cran, top_dep = dependencies))

  # Remove base packages
  inst <- installed.packages()
  base <- unname(inst[inst[, "Priority"] %in% c("base", "recommended"), "Package"])
  deps <- setdiff(deps, base)

  inst_ver <- unname(inst[, "Version"][match(deps, rownames(inst))])
  cran_ver <- unname(cran[, "Version"][match(deps, rownames(cran))])
  diff <- compare_versions(inst_ver, cran_ver)

  structure(
    data.frame(
      package = deps,
      installed = inst_ver,
      available = cran_ver,
      diff = diff,
      stringsAsFactors = FALSE
    ),
    class = c("package_deps", "data.frame"),
    repos = repos,
    type = type
  )
}

dev_package_deps <- function(pkgdir, dependencies = NA,
                             repos = getOption("repos"),
                             type = getOption("pkgType")) {

  pkg <- load_pkg_description(pkgdir)
  install_dev_remotes(pkgdir)

  dependencies <- tolower(standardise_dep(dependencies))
  dependencies <- intersect(dependencies, names(pkg))

  parsed <- lapply(pkg[tolower(dependencies)], parse_deps)
  deps <- unlist(lapply(parsed, `[[`, "name"), use.names = FALSE)

  if (is_bioconductor(pkg)) {
    bioc_repos <- BiocInstaller::biocinstallRepos()

    missing_repos <- setdiff(names(bioc_repos), names(repos))

    if (length(missing_repos) > 0)
      repos[missing_repos] <- bioc_repos[missing_repos]
  }

  package_deps(deps, repos = repos, type = type)
}

compare_versions <- function(a, b) {
  stopifnot(length(a) == length(b))

  compare_var <- function(x, y) {
    if (is.na(y)) return(2L)
    if (is.na(x)) return(-2L)

    x <- package_version(x)
    y <- package_version(y)

    if (x < y) {
      -1L
    } else if (x > y) {
      1L
    } else {
      0L
    }
  }

  vapply(seq_along(a), function(i) compare_var(a[[i]], b[[i]]), integer(1))
}

install_dev_remotes <- function(pkgdir, ...) {

  pkg <- load_pkg_description(pkgdir)
  if (!has_dev_remotes(pkg)) {
    return()
  }

  types <- dev_remote_type(pkg[["remotes"]])

  lapply(types, function(type) type$fun(type$repository, ...))
}

# Parse the remotes field split into pieces and get install_ functions for each
# remote type
dev_remote_type <- function(remotes = "") {

  if (!nchar(remotes)) {
    return()
  }

  dev_packages <- trim_ws(unlist(strsplit(remotes, ",[[:space:]]*")))

  parse_one <- function(x) {
    pieces <- strsplit(x, "::", fixed = TRUE)[[1]]

    if (length(pieces) == 1) {
      type <- "github"
      repo <- pieces
    } else if (length(pieces) == 2) {
      type <- pieces[1]
      repo <- pieces[2]
    } else {
      stop("Malformed remote specification '", x, "'", call. = FALSE)
    }
    tryCatch(
      fun <- get(x = paste0("install_", tolower(type)),
        envir = asNamespace("remotes"),
        mode = "function",
        inherits = FALSE),
      error = function(e) {
        stop("Malformed remote specification '", x, "'", call. = FALSE)
      })
    list(repository = repo, type = type, fun = fun)
  }

  lapply(dev_packages, parse_one)
}

has_dev_remotes <- function(pkg) {
  !is.null(pkg[["remotes"]])
}


update_packages <- function(object, ..., quiet = FALSE, upgrade = TRUE) {
  ahead <- object$package[object$diff == 2L]
  if (length(ahead) > 0 && !quiet) {
    message("Skipping ", length(ahead), " packages not available: ",
      paste(ahead, collapse = ", "))
  }

  missing <- object$package[object$diff == 1L]
  if (length(missing) > 0 && !quiet) {
    message("Skipping ", length(missing), " packages ahead of CRAN: ",
      paste(missing, collapse = ", "))
  }

  if (upgrade) {
    behind <- object$package[object$diff < 0L]
  } else {
    behind <- object$package[is.na(object$available)]
  }
  if (length(behind) > 0L) {
    install_packages(behind, repos = attr(object, "repos"),
      type = attr(object, "type"), ...)
  }

}

install_packages <- function(packages, repos = getOption("repos"),
                             type = getOption("pkgType"), ...,
                             dependencies = FALSE, quiet = NULL) {
  if (identical(type, "both"))
    type <- "binary"
  if (is.null(quiet))
    quiet <- !identical(type, "source")

  message("Installing ", length(packages), " packages: ",
    paste(packages, collapse = ", "))

  safe_install_packages(
    packages,
    repos = repos,
    type = type,
    ...,
    dependencies = dependencies,
    quiet = quiet
  )
}

find_deps <- function(packages, available = available.packages(),
                      top_dep = TRUE, rec_dep = NA, include_pkgs = TRUE) {
  if (length(packages) == 0 || identical(top_dep, FALSE))
    return(character())

  top_dep <- standardise_dep(top_dep)
  rec_dep <- standardise_dep(rec_dep)

  top <- tools::package_dependencies(packages, db = available, which = top_dep)
  top_flat <- unlist(top, use.names = FALSE)

  if (length(rec_dep) != 0 && length(top_flat) > 0) {
    rec <- tools::package_dependencies(top_flat, db = available, which = rec_dep,
      recursive = TRUE)
    rec_flat <- unlist(rec, use.names = FALSE)
  } else {
    rec_flat <- character()
  }

  unique(c(if (include_pkgs) packages, top_flat, rec_flat))
}


standardise_dep <- function(x) {
  if (identical(x, NA)) {
    c("Depends", "Imports", "LinkingTo")
  } else if (isTRUE(x)) {
    c("Depends", "Imports", "LinkingTo", "Suggests")
  } else if (identical(x, FALSE)) {
    character(0)
  } else if (is.character(x)) {
    x
  } else {
    stop("Dependencies must be a boolean or a character vector", call. = FALSE)
  }
}

download <- function(path, url, auth_token, quiet = TRUE) {

  real_url <- url
  if (!is.null(auth_token)) {
    sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
    real_url <- paste0(url, sep, "access_token=", auth_token)
  }

  status <- download.file(
    real_url,
    path,
    method = download_method(),
    quiet = quiet,
    mode = "wb"
  )

  if (status != 0)  stop("Cannot download file from ", url, call. = FALSE)

  path
}

download_method <- function() {

  if (isTRUE(unname(capabilities("libcurl")))) {
    "libcurl"

  } else if (os_type() == "windows") {
    "wininet"

  } else {
    "auto"
  }
}

os_type <- function() {
  .Platform$OS.type
}

# Extract the commit hash from a git archive. Git archives include the SHA1
# hash as the comment field of the zip central directory record
# (see https://www.kernel.org/pub/software/scm/git/docs/git-archive.html)
# Since we know it's 40 characters long we seek that many bytes minus 2
# (to confirm the comment is exactly 40 bytes long)
git_extract_sha1 <- function(bundle) {

  # open the bundle for reading
  conn <- file(bundle, open = "rb", raw = TRUE)
  on.exit(close(conn))

  # seek to where the comment length field should be recorded
  seek(conn, where = -0x2a, origin = "end")

  # verify the comment is length 0x28
  len <- readBin(conn, "raw", n = 2)
  if (len[1] == 0x28 && len[2] == 0x00) {
    # read and return the SHA1
    rawToChar(readBin(conn, "raw", n = 0x28))
  } else {
    NULL
  }
}

github_GET <- function(path, ..., pat = github_pat()) {

  url <- paste0("https://api.github.com/", path)

  tmp <- tempfile()
  download(tmp, url, auth_token = pat)

  fromJSONFile(tmp)
}

github_commit <- function(username, repo, ref = "master") {

  url <- file.path("https://api.github.com",
                   "repos", username, repo, "commits", ref)

  tmp <- tempfile()
  download(tmp, url, auth_token = github_pat())

  fromJSONFile(tmp)
}

#' Retrieve Github personal access token.
#'
#' A github personal access token
#' Looks in env var \code{GITHUB_PAT}
#'
#' @keywords internal
#' @noRd
github_pat <- function() {
  pat <- Sys.getenv('GITHUB_PAT')
  if (identical(pat, "")) return(NULL)

  message("Using github PAT from envvar GITHUB_PAT")
  pat
}

#' Install a package from a git repository
#'
#' It is vectorised so you can install multiple packages with
#' a single command. You do not need to have git installed.
#'
#' @param url Location of package. The url should point to a public or
#'   private repository.
#' @param branch Name of branch or tag to use, if not master.
#' @param subdir A sub-directory within a git repository that may
#'   contain the package we are interested in installing.
#' @param args DEPRECATED. A character vector providing extra arguments to
#'   pass on to git.
#' @param ... passed on to \code{install.packages}
#' @noRd
#' @examples
#' \dontrun{
#' install_git("git://github.com/hadley/stringr.git")
#' install_git("git://github.com/hadley/stringr.git", branch = "stringr-0.2")
#'}
install_git <- function(url, subdir = NULL, branch = NULL, args = character(0),
                ...) {
  ## TODO
}
#' Attempts to install a package directly from GitHub.
#'
#' This function is vectorised on \code{repo} so you can install multiple
#' packages in a single command.
#'
#' @param repo Repository address in the format
#'   \code{username/repo[/subdir][@@ref|#pull]}. Alternatively, you can
#'   specify \code{subdir} and/or \code{ref} using the respective parameters
#'   (see below); if both is specified, the values in \code{repo} take
#'   precedence.
#' @param username User name. Deprecated: please include username in the
#'   \code{repo}
#' @param ref Desired git reference. Could be a commit, tag, or branch
#'   name, or a call to \code{\link{github_pull}}. Defaults to \code{"master"}.
#' @param subdir subdirectory within repo that contains the R package.
#' @param auth_token To install from a private repo, generate a personal
#'   access token (PAT) in \url{https://github.com/settings/applications} and
#'   supply to this argument. This is safer than using a password because
#'   you can easily delete a PAT without affecting any others. Defaults to
#'   the \code{GITHUB_PAT} environment variable.
#' @param host GitHub API host to use. Override with your GitHub enterprise
#'   hostname, for example, \code{"github.hostname.com/api/v3"}.
#' @param ... Other arguments passed on to \code{install.packages}.
#' @details
#' Attempting to install from a source repository that uses submodules
#' raises a warning. Because the zipped sources provided by GitHub do not
#' include submodules, this may lead to unexpected behaviour or compilation
#' failure in source packages. In this case, cloning the repository manually
#' may yield better results.
#' @export
#' @seealso \code{\link{github_pull}}
#' @examples
#' \dontrun{
#' install_github("klutometis/roxygen")
#' install_github("wch/ggplot2")
#' install_github(c("rstudio/httpuv", "rstudio/shiny"))
#' install_github(c("hadley/httr@@v0.4", "klutometis/roxygen#142",
#'   "mfrasca/r-logging/pkg"))
#'
#' # To install from a private repo, use auth_token with a token
#' # from https://github.com/settings/applications. You only need the
#' # repo scope. Best practice is to save your PAT in env var called
#' # GITHUB_PAT.
#' install_github("hadley/private", auth_token = "abc")
#'
#' }
install_github <- function(repo, username = NULL,
                           ref = "master", subdir = NULL,
                           auth_token = github_pat(),
                           host = "api.github.com", ...) {

  remotes <- lapply(repo, github_remote, username = username, ref = ref,
    subdir = subdir, auth_token = auth_token, host = host)

  install_remotes(remotes, ...)
}

github_remote <- function(repo, username = NULL, ref = NULL, subdir = NULL,
                       auth_token = github_pat(), sha = NULL,
                       host = "api.github.com") {

  meta <- parse_git_repo(repo)
  meta <- github_resolve_ref(meta$ref %||% ref, meta)

  if (is.null(meta$username)) {
    meta$username <- username %||% getOption("github.user") %||%
      stop("Unknown username.")
    warning("Username parameter is deprecated. Please use ",
      username, "/", repo, call. = FALSE)
  }

  remote("github",
    host = host,
    repo = meta$repo,
    subdir = meta$subdir %||% subdir,
    username = meta$username,
    ref = meta$ref,
    sha = sha,
    auth_token = auth_token
  )
}

#' @export
remote_download.github_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading GitHub repo ", x$username, "/", x$repo, "@", x$ref)
  }

  dest <- tempfile(fileext = paste0(".zip"))
  src_root <- paste0("https://", x$host, "/repos/", x$username, "/", x$repo)
  src <- paste0(src_root, "/zipball/", x$ref)

  if (github_has_submodules(x)) {
    warning("GitHub repo contains submodules, may not function as expected!",
            call. = FALSE)
  }

  download(dest, src, auth_token = x$auth_token)
}

github_has_submodules <- function(x) {
  src_root <- paste0("https://", x$host, "/repos/", x$username, "/", x$repo)
  src_submodules <- paste0(src_root, "/contents/.gitmodules?ref=", x$ref)

  tmp <- tempfile()
  res <- tryCatch(
    download(tmp, src_submodules, auth_token = x$auth_token),
    error = function(e) e
  )
  if (is(res, "error")) return(FALSE)

  ## download() sometimes just downloads the error page, because
  ## the libcurl backend in download.file() is broken
  ## If the request was successful (=submodules exist), then it has an
  ## 'sha' field.
  sha <- tryCatch(
    fromJSONFile(tmp)$sha,
    error = function(e) e
  )
  ! is(sha, "error") && ! is.null(sha)
}

#' @export
remote_metadata.github_remote <- function(x, bundle = NULL, source = NULL) {
  # Determine sha as efficiently as possible
  if (!is.null(x$sha)) {
    # Might be cached already (because re-installing)
    sha <- x$sha
  } else if (!is.null(bundle)) {
    # Might be able to get from zip archive
    sha <- git_extract_sha1(bundle)
  } else {
    # Otherwise can use github api
    sha <- github_commit(x$username, x$repo, x$ref)$sha
  }

  list(
    RemoteType = "github",
    RemoteHost = x$host,
    RemoteRepo = x$repo,
    RemoteUsername = x$username,
    RemoteRef = x$ref,
    RemoteSha = sha,
    RemoteSubdir = x$subdir,
    # Backward compatibility for packrat etc.
    GithubRepo = x$repo,
    GithubUsername = x$username,
    GithubRef = x$ref,
    GithubSHA1 = sha,
    GithubSubdir = x$subdir
  )
}

#' GitHub references
#'
#' Use as \code{ref} parameter to \code{\link{install_github}}.
#' Allows installing a specific pull request or the latest release.
#'
#' @param pull The pull request to install
#' @seealso \code{\link{install_github}}
#' @rdname github_refs
#' @export
github_pull <- function(pull) structure(pull, class = "github_pull")

#' @rdname github_refs
#' @export
github_release <- function() structure(NA_integer_, class = "github_release")

github_resolve_ref <- function(x, params) UseMethod("github_resolve_ref")

#' @export
github_resolve_ref.default <- function(x, params) {
  params$ref <- x
  params
}

#' @export
github_resolve_ref.NULL <- function(x, params) {
  params$ref <- "master"
  params
}

#' @export
github_resolve_ref.github_pull <- function(x, params) {
  # GET /repos/:user/:repo/pulls/:number
  path <- file.path("repos", params$username, params$repo, "pulls", x)
  response <- tryCatch(
    github_GET(path),
    error = function(e) e
  )

  ## Just because libcurl might download the error page...
  if (is(response, "error") || is.null(response$head)) {
    stop("Cannot find GitHub pull request ", params$username, "/",
         params$repo, "#", x)
  }

  params$username <- response$head$user$login
  params$ref <- response$head$ref
  params
}

# Retrieve the ref for the latest release
#' @export
github_resolve_ref.github_release <- function(x, params) {
  # GET /repos/:user/:repo/releases
  path <- paste("repos", params$username, params$repo, "releases", sep = "/")
  response <- tryCatch(
    github_GET(path),
    error = function(e) e
  )

  if (is(response, "error") || !is.null(response$message)) {
    stop("Cannot find repo ", params$username, "/", params$repo, ".")
  }

  if (length(response) == 0L)
    stop("No releases found for repo ", params$username, "/", params$repo, ".")

  params$ref <- response[[1L]]$tag_name
  params
}

# Parse concise git repo specification: [username/]repo[/subdir][#pull|@ref|@*release]
# (the *release suffix represents the latest release)
parse_git_repo <- function(path) {
  username_rx <- "(?:([^/]+)/)?"
  repo_rx <- "([^/@#]+)"
  subdir_rx <- "(?:/([^@#]*[^@#/]))?"
  ref_rx <- "(?:@([^*].*))"
  pull_rx <- "(?:#([0-9]+))"
  release_rx <- "(?:@([*]release))"
  ref_or_pull_or_release_rx <- sprintf("(?:%s|%s|%s)?", ref_rx, pull_rx, release_rx)
  github_rx <- sprintf("^(?:%s%s%s%s|(.*))$",
    username_rx, repo_rx, subdir_rx, ref_or_pull_or_release_rx)

  param_names <- c("username", "repo", "subdir", "ref", "pull", "release", "invalid")
  replace <- stats::setNames(sprintf("\\%d", seq_along(param_names)), param_names)
  params <- lapply(replace, function(r) gsub(github_rx, r, path, perl = TRUE))
  if (params$invalid != "")
    stop(sprintf("Invalid git repo: %s", path))
  params <- params[sapply(params, nchar) > 0]

  if (!is.null(params$pull)) {
    params$ref <- github_pull(params$pull)
    params$pull <- NULL
  }

  if (!is.null(params$release)) {
    params$ref <- github_release()
    params$release <- NULL
  }

  params
}
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
install_remote <- function(remote, ..., quiet = FALSE) {
  stopifnot(is.remote(remote))

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

  desc <- modifyList(desc, meta)

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

install <- function(pkgdir, dependencies = NA, quiet = TRUE, ...) {

  install_deps(pkgdir, dependencies = dependencies, quiet = quiet, ...)

  safe_install_packages(
      pkgdir,
      repos = NULL,
      quiet = quiet,
      type = "source",
    ...
  )

  invisible(TRUE)
}

safe_install_packages <- function(...) {

  lib <- paste(.libPaths(), collapse = ":")

  with_envvar(
    c(R_LIBS = lib,
      R_LIBS_USER = lib,
      R_LIBS_SITE = lib,
      R_PROFILE_USER = tempfile()),
    utils::install.packages(...)
  )
}

install_deps <- function(pkgdir, dependencies = NA,
                         threads = getOption("Ncpus", 1),
                         repos = getOption("repos"),
                         type = getOption("pkgType"),
                         ...,
                         upgrade = TRUE,
                         quiet = FALSE) {

  packages <- dev_package_deps(
    pkgdir,
    repos = repos,
    dependencies = dependencies,
    type = type
  )

  update_packages(
    packages,
    dependencies = dependencies,
    ...,
    Ncpus = threads,
    quiet = quiet,
    upgrade = upgrade
  )
}

tokenize_json <- function(text) {
  text <- paste(text, collapse = "\n")

  ESCAPE <- '(\\\\[^u[:cntrl:]]|\\\\u[0-9a-fA-F]{4})'
  CHAR <- '[^[:cntrl:]"\\\\]'

  STRING <- paste0('"', CHAR, '*(', ESCAPE, CHAR, '*)*"')
  NUMBER <- "-?(0|[1-9][0-9]*)([.][0-9]*)?([eE][+-]?[0-9]*)?"
  KEYWORD <- 'null|false|true'
  SPACE <- '[[:space:]]+'

  match <- gregexpr(
    pattern = paste0(
      STRING, "|", NUMBER, "|", KEYWORD, "|", SPACE, "|", "."
    ),
    text = text,
    perl = TRUE
  )

  grep("^\\s+$", regmatches(text, match)[[1]], value = TRUE, invert = TRUE)
}

throw <- function(...) {
  stop("JSON: ", ..., call. = FALSE)
}

fromJSONFile <- function(filename) {
  fromJSON(readLines(filename, warn = FALSE))
}

fromJSON <- function(text) {

  tokens <- tokenize_json(text)
  token <- NULL
  ptr <- 1

  read_token <- function() {
    if (ptr <= length(tokens)) {
      token <<- tokens[ptr]
      ptr <<- ptr + 1
    } else {
      token <<- 'EOF'
    }
  }

  parse_value <- function(name = "") {
    if (token == "{") {
      parse_object()
    } else if (token == "[") {
      parse_array()
    } else if (token == "EOF" || (nchar(token) == 1 && ! token %in% 0:9)) {
      throw("EXPECTED value GOT ", token)
    } else {
      j2r(token)
    }
  }

  parse_object <- function() {
    res <- structure(list(), names = character())

    read_token()

    ## Invariant: we are at the beginning of an element
    while (token != "}") {

      ## "key"
      if (grepl('^".*"$', token)) {
        key <- j2r(token)
      } else {
        throw("EXPECTED string GOT ", token)
      }

      ## :
      read_token()
      if (token != ":") { throw("EXPECTED : GOT ", token) }

      ## value
      read_token()
      res[key] <- list(parse_value())

      ## } or ,
      read_token()
      if (token == "}") {
        break
      } else if (token != ",") {
        throw("EXPECTED , or } GOT ", token)
      }
      read_token()
    }

    res
  }

  parse_array <- function() {
    res <- list()

    read_token()

    ## Invariant: we are at the beginning of an element
    while (token != "]") {
      ## value
      res <- c(res, list(parse_value()))

      ## ] or ,
      read_token()
      if (token == "]") {
        break
      } else if (token != ",") {
        throw("EXPECTED , GOT ", token)
      }
      read_token()
    }

    res
  }

  read_token()
  parse_value(tokens)
}

j2r <- function(token) {
  if (token == "null") {
    NULL
  } else if (token == "true") {
    TRUE
  } else if (token == "false") {
    FALSE
  } else if (grepl('^".*"$', token)) {
    trimq(token)
  } else {
    as.numeric(token)
  }
}

trimq <- function(x) {
  sub('^"(.*)"$', "\\1", x)
}

parse_deps <- function(string) {
  if (is.null(string)) return()
  stopifnot(is.character(string), length(string) == 1)
  if (grepl("^\\s*$", string)) return()

  pieces <- strsplit(string, ",")[[1]]

  # Get the names
  names <- gsub("\\s*\\(.*?\\)", "", pieces)
  names <- gsub("^\\s+|\\s+$", "", names)

  # Get the versions and comparison operators
  versions_str <- pieces
  have_version <- grepl("\\(.*\\)", versions_str)
  versions_str[!have_version] <- NA

  compare  <- sub(".*\\((\\S+)\\s+.*\\)", "\\1", versions_str)
  versions <- sub(".*\\(\\S+\\s+(.*)\\)", "\\1", versions_str)

  # Check that non-NA comparison operators are valid
  compare_nna   <- compare[!is.na(compare)]
  compare_valid <- compare_nna %in% c(">", ">=", "==", "<=", "<")
  if(!all(compare_valid)) {
    stop("Invalid comparison operator in dependency: ",
      paste(compare_nna[!compare_valid], collapse = ", "))
  }

  deps <- data.frame(name = names, compare = compare,
    version = versions, stringsAsFactors = FALSE)

  # Remove R dependency
  deps[names != "R", ]
}

load_pkg_description <- function(path) {
  path <- normalizePath(path)
  path_desc <- file.path(path, "DESCRIPTION")

  desc <- read_dcf(path_desc)
  names(desc) <- tolower(names(desc))
  desc$path <- path

  desc
}
#' Run a system command and check if it succeeds.
#'
#' @param cmd the command to run.
#' @param args a vector of command arguments.
#' @param quiet if \code{FALSE}, the command to be run will be echoed.
#' @param ... additional arguments passed to \code{\link[base]{system}}
#' @return \code{TRUE} if the command succeeds, an error will be thrown if the
#' command fails.
#' @noRd
system_check <- function(cmd, args = character(), quiet = FALSE, ...) {
  full <- paste(shQuote(cmd), " ", paste(args, collapse = " "), sep = "")

  if (!quiet) {
    message(wrap_command(full))
    message()
  }

  result <- suppressWarnings(
    system(full, intern = quiet, ignore.stderr = quiet, ...)
  )

  if (quiet) {
    status <- attr(result, "status") %||% 0
  } else {
    status <- result
  }

  if (!identical(as.character(status), "0")) {
    stop("Command failed (", status, ")", call. = FALSE)
  }

  invisible(TRUE)
}


wrap_command <- function(x) {
  lines <- strwrap(x, getOption("width") - 2, exdent = 2)
  continue <- c(rep(" \\", length(lines) - 1), "")
  paste(lines, continue, collapse = "\n")
}

`%||%` <- function (a, b) if (!is.null(a)) a else b

is_bioconductor <- function(x) {
  !is.null(x$biocviews)
}

trim_ws <- function(x) {
  gsub("^[[:space:]]+|[[:space:]]+$", "", x)
}

set_envvar <- function(envs) {
  if (length(envs) == 0) return()

  stopifnot(is.named(envs))

  old <- Sys.getenv(names(envs), names = TRUE, unset = NA)
  set <- !is.na(envs)

  both_set <- set & !is.na(old)

  if (any(set))  do.call("Sys.setenv", as.list(envs[set]))
  if (any(!set)) Sys.unsetenv(names(envs)[!set])

  invisible(old)
}

with_envvar <- function(new, code) {
  old <- set_envvar(new)
  on.exit(set_envvar(old))
  force(code)
}

is.named <- function(x) {
  !is.null(names(x)) && all(names(x) != "")
}


  install_github(...)

}
