
function(...) {

  ## This is the code of the package, put in here by brew

  
bioc_version <- function() {
  bver <- get(
    ".BioC_version_associated_with_R_version",
    envir = asNamespace("tools"),
    inherits = FALSE
  )

  if (is.function(bver)) bver() else bver
}

## This is mostly from https://bioconductor.org/biocLite.R

#' Deduce the URLs of the BioConductor repositories
#'
#' @return A named character vector of the URLs of the
#' BioConductor repositories, appropriate for the current
#' R version.
#'
#' @export

bioc_install_repos <- function() {

  vers <- getRversion()
  biocVers <- bioc_version()

  a <- NULL

  p <- file.path(Sys.getenv("HOME"), ".R", "repositories")
  if (file.exists(p)) {
    a <- ("tools" %:::% ".read_repositories")(p)
    if (!"BioCsoft" %in% rownames(a)) a <- NULL
  }

  if (is.null(a)) {
    p <- file.path(R.home("etc"), "repositories")
    a <- ("tools" %:::% ".read_repositories")(p)
  }

  ## add a conditional for Bioc releases occuring WITHIN
  ## a single R minor version. This is so that a user with a
  ## version of R (whose etc/repositories file references the
  ## no-longer-latest URL) and without BiocInstaller
  ## will be pointed to the most recent repository suitable
  ## for their version of R
  if (vers >= "3.2.2" && vers < "3.3.0") {
    ## transitioning to https support; check availability
    con <- file(fl <- tempfile(), "w")
    sink(con, type = "message")
    tryCatch(
      { xx <- close(file("https://bioconductor.org")) },
      error = function(e) { message(conditionMessage(e)) }
    )
    sink(type = "message")
    close(con)

    if (!length(readLines(fl))) {
      a[, "URL"] <- sub("^http:", "https:", a[, "URL"])
    }
  }
  if (vers >= "3.3.0") {
    a[, "URL"] <- sub(as.character(biocVers), "3.3", a[, "URL"])

  } else if (vers >= "3.2") {
    a[, "URL"] <- sub(as.character(biocVers), "3.2", a[, "URL"])

  } else if (vers == "3.1.1") {
    ## R-3.1.1's etc/repositories file at the time of the release
    ## of Bioc 3.0 pointed to the 2.14 repository, but we want
    ## new installations to access the 3.0 repository
    a[, "URL"] <- sub(as.character(biocVers), "3.0", a[, "URL"])

  } else if (vers == "3.1.0") {
    ## R-devel points to 2.14 repository
    a[, "URL"] <- sub(as.character(biocVers), "2.14", a[, "URL"])

  } else if (vers >= "2.15" && vers < "2.16") {
    a[, "URL"] <- sub(as.character(biocVers), "2.11", a[, "URL"])
    biocVers <- numeric_version("2.11")
  }

  repos <- intersect(
    rownames(a),
    c("BioCsoft", "BioCann", "BioCexp", "BioCextra")
  )

  structure(a[repos, "URL"], names = repos)
}

available_packages <- function(repos, type) {
  suppressWarnings(utils::available.packages(utils::contrib.url(repos, type), type = type))
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

get_desc_field <- function(path, field) {
  dcf <- read_dcf(path)
  dcf[[field]]
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
    untar(src, exdir = target)
    outdir <- getrootdir(untar(src, list = TRUE))

  } else if (grepl("\\.(tar\\.gz|tgz)$", src)) {
    untar(src, exdir = target, compressed = "gzip")
    outdir <- getrootdir(untar(src, compressed = "gzip", list = TRUE))

  } else if (grepl("\\.(tar\\.bz2|tbz)$", src)) {
    untar(src, exdir = target, compressed = "bzip2")
    outdir <- getrootdir(untar(src, compressed = "bzip2", list = TRUE))

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

  system_check(unzip, args)
}

#' Find all dependencies of a CRAN or dev package.
#'
#' Find all the dependencies of a package and determine whether they are ahead
#' or behind CRAN. A \code{print()} method identifies mismatches (if any)
#' between local and CRAN versions of each dependent package; an
#' \code{update()} method installs outdated or missing packages from CRAN.
#'
#' @param packages A character vector of package names.
#' @param pkgdir path to a package directory.
#' @param dependencies Which dependencies do you want to check?
#'   Can be a character vector (selecting from "Depends", "Imports",
#'    "LinkingTo", "Suggests", or "Enhances"), or a logical vector.
#'
#'   \code{TRUE} is shorthand for "Depends", "Imports", "LinkingTo" and
#'   "Suggests". \code{NA} is shorthand for "Depends", "Imports" and "LinkingTo"
#'   and is the default. \code{FALSE} is shorthand for no dependencies (i.e.
#'   just check this package, not its dependencies).
#' @param quiet If \code{TRUE}, suppress output.
#' @param upgrade If \code{TRUE}, also upgrade any of out date dependencies.
#' @param repos A character vector giving repositories to use.
#' @param type Type of package to \code{update}.  If "both", will switch
#'   automatically to "binary" to avoid interactive prompts during package
#'   installation.
#'
#' @param object A \code{package_deps} object.
#' @param ... Additional arguments passed to \code{install_packages}.
#'
#' @return
#'
#' A \code{data.frame} with columns:
#'
#' \tabular{ll}{
#' \code{package} \tab The dependent package's name,\cr
#' \code{installed} \tab The currently installed version,\cr
#' \code{available} \tab The version available on CRAN,\cr
#' \code{diff} \tab An integer denoting whether the locally installed version
#'   of the package is newer (1), the same (0) or older (-1) than the version
#'   currently available on CRAN.\cr
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' package_deps("devtools")
#' # Use update to update any out-of-date dependencies
#' update(package_deps("devtools"))
#' }

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
  inst <- utils::installed.packages()
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

#' \code{local_package_deps} extracts dependencies from a
#' local DESCRIPTION file.
#'
#' @export
#' @rdname package_deps

local_package_deps <- function(pkgdir = ".", dependencies = NA) {
  pkg <- load_pkg_description(pkgdir)

  dependencies <- tolower(standardise_dep(dependencies))
  dependencies <- intersect(dependencies, names(pkg))

  parsed <- lapply(pkg[tolower(dependencies)], parse_deps)
  unlist(lapply(parsed, `[[`, "name"), use.names = FALSE)
}

#' \code{dev_package_deps} lists the status of the dependencies
#' of a local package.
#'
#' @export
#' @rdname package_deps

dev_package_deps <- function(pkgdir = ".", dependencies = NA,
                             repos = getOption("repos"),
                             type = getOption("pkgType")) {

  pkg <- load_pkg_description(pkgdir)
  install_dev_remotes(pkgdir)
  repos <- c(repos, parse_additional_repositories(pkg))

  deps <- local_package_deps(pkgdir = pkgdir, dependencies = dependencies)

  if (is_bioconductor(pkg)) {
    bioc_repos <- bioc_install_repos()

    missing_repos <- setdiff(names(bioc_repos), names(repos))

    if (length(missing_repos) > 0)
      repos[missing_repos] <- bioc_repos[missing_repos]
  }

  package_deps(deps, repos = repos, type = type)
}

## -2 = not installed, but available on CRAN
## -1 = installed, but out of date
##  0 = installed, most recent version
##  1 = installed, version ahead of CRAN
##  2 = package not on CRAN

compare_versions <- function(installed, cran) {
  stopifnot(length(installed) == length(cran))

  compare_var <- function(i, c) {
    if (is.na(c)) return(c(notcran = 2L))
    if (is.na(i)) return(c(notinst = -2L))

    i <- package_version(i)
    c <- package_version(c)

    if (i < c) {
      c(outofdate = -1L)
    } else if (i > c) {
      c(aheadofcran = 1L)
    } else {
      c(equal = 0L)
    }
  }

  vapply(
    seq_along(installed),
    function(i) compare_var(installed[[i]], cran[[i]]),
    integer(1)
  )
}

install_dev_remotes <- function(pkgdir = ".", ...) {

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

#' @export
print.package_deps <- function(x, show_ok = FALSE, ...) {
  class(x) <- "data.frame"

  ahead <- x$diff > 0L
  behind <- x$diff < 0L
  same_ver <- x$diff == 0L

  x$diff <- NULL
  x[] <- lapply(x, format)

  if (any(behind)) {
    cat("Needs update -----------------------------\n")
    print(x[behind, , drop = FALSE], row.names = FALSE, right = FALSE)
  }

  if (any(ahead)) {
    cat("Not on CRAN ----------------------------\n")
    print(x[ahead, , drop = FALSE], row.names = FALSE, right = FALSE)
  }

  if (show_ok && any(same_ver)) {
    cat("OK ---------------------------------------\n")
    print(x[same_ver, , drop = FALSE], row.names = FALSE, right = FALSE)
  }
}

## -2 = not installed, but available on CRAN
## -1 = installed, but out of date
##  0 = installed, most recent version
##  1 = installed, version ahead of CRAN
##  2 = package not on CRAN

#' @export
#' @rdname package_deps
#' @importFrom stats update

update.package_deps <- function(object, ..., quiet = FALSE, upgrade = TRUE) {
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
    behind <- object$package[is.na(object$installed)]
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

find_deps <- function(packages, available = utils::available.packages(),
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

#' Update packages that are missing or out-of-date.
#'
#' Works similarly to \code{install.packages()} but doesn't install packages
#' that are already installed, and also upgrades out dated dependencies.
#'
#' @param packages Character vector of packages to update.
#' @inheritParams package_deps
#' @seealso \code{\link{package_deps}} to see which packages are out of date/
#'   missing.
#' @export
#' @examples
#' \dontrun{
#' update_packages("ggplot2")
#' update_packages(c("plyr", "ggplot2"))
#' }

update_packages <- function(packages, dependencies = NA,
                            repos = getOption("repos"),
                            type = getOption("pkgType")) {
  pkgs <- package_deps(packages, repos = repos, type = type)
  update(pkgs)
}

has_additional_repositories <- function(pkg) {
  "additional_repositories" %in% names(pkg)
}

parse_additional_repositories <- function(pkg) {
  if (has_additional_repositories(pkg)) {
    strsplit(pkg[["additional_repositories"]], "[,[:space:]]+")[[1]]
  }
}

has_devel <- function() {
  tryCatch(
    has_devel2(),
    error = function(e) FALSE
  )
}

## This is similar to devtools:::has_devel(), with some
## very minor differences.

has_devel2 <- function() {
  foo_path <- file.path(tempfile(fileext = ".c"))

  cat("void foo(int *bar) { *bar=1; }\n", file = foo_path)
  on.exit(unlink(foo_path))

  R(c("CMD", "SHLIB", basename(foo_path)), dirname(foo_path))
  dylib <- sub("\\.c$", .Platform$dynlib.ext, foo_path)
  on.exit(unlink(dylib), add = TRUE)

  dll <- dyn.load(dylib)
  on.exit(dyn.unload(dylib), add = TRUE)

  stopifnot(.C(dll$foo, 0L)[[1]] == 1L)
  TRUE
}

missing_devel_warning <- function(pkgdir) {
  pkgname <- tryCatch(
    get_desc_field(file.path(pkgdir, "DESCRIPTION"), "Package"),
    error = function(e) NULL
  ) %||% "<unknown>"

  sys <- sys_type()

  warning(
    "Package ",
    pkgname,
    " has compiled code, but no suitable ",
    "compiler(s) were found. Installation will likely fail.\n  ",
    if (sys == "windows") "Install Rtools and make sure it is in the PATH.",
    if (sys == "macos") "Install XCode and make sure it works.",
    if (sys == "linux") "Install compilers via your Linux package manager."
  )
}

R <- function(args, path = tempdir()) {

  r <- file.path(R.home("bin"), "R")

  args <- c(
    "--no-site-file", "--no-environ", "--no-save",
    "--no-restore", "--quiet",
    args
  )

  system_check(r, args, path = path)
}

#' @importFrom utils compareVersion

download <- function(path, url, auth_token = NULL, basic_auth = NULL,
                     quiet = TRUE) {

  real_url <- url

  if (!is.null(basic_auth)) {
    str <- paste0("://", basic_auth$user, ":", basic_auth$password, "@")
    real_url <- sub("://", str, url)
  }

  if (!is.null(auth_token)) {
    sep <- if (grepl("?", url, fixed = TRUE)) "&" else "?"
    real_url <- paste0(url, sep, "access_token=", auth_token)
  }

  if (compareVersion(get_r_version(), "3.2.0") == -1) {
    curl_download(real_url, path, quiet)

  } else {

    base_download(real_url, path, quiet)
  }

  path
 }

base_download <- function(url, path, quiet) {

  suppressWarnings(
    status <- utils::download.file(
      url,
      path,
      method = download_method(),
      quiet = quiet,
      mode = "wb"
    )
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

curl_download <- function(url, path, quiet) {

  if (!pkg_installed("curl")) {
    stop("The 'curl' package is required if R is older than 3.2.0")
  }

  curl::curl_download(url, path, quiet = quiet)
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

git <- function(args, quiet = TRUE, path = ".") {
  full <- paste0(shQuote(check_git_path()), " ", paste(args, collapse = ""))
  if (!quiet) {
    message(full)
  }

  result <- in_dir(path, system(full, intern = TRUE, ignore.stderr = quiet))

  status <- attr(result, "status") %||% 0
  if (!identical(as.character(status), "0")) {
    stop("Command failed (", status, ")", call. = FALSE)
  }

  result
}

# Retrieve the current running path of the git binary.
# @param git_binary_name The name of the binary depending on the OS.
git_path <- function(git_binary_name = NULL) {
  # Use user supplied path
  if (!is.null(git_binary_name)) {
    if (!file.exists(git_binary_name)) {
      stop("Path ", git_binary_name, " does not exist", .call = FALSE)
    }
    return(git_binary_name)
  }

  # Look on path
  git_path <- Sys.which("git")[[1]]
  if (git_path != "") return(git_path)

  # On Windows, look in common locations
  if (os_type() == "windows") {
    look_in <- c(
      "C:/Program Files/Git/bin/git.exe",
      "C:/Program Files (x86)/Git/bin/git.exe"
    )
    found <- file.exists(look_in)
    if (any(found)) return(look_in[found][1])
  }

  NULL
}

check_git_path <- function(git_binary_name = NULL) {

  path <- git_path(git_binary_name)

  if (is.null(path)) {
    stop("Git does not seem to be installed on your system.", call. = FALSE)
  }

  path
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

#' Install a package directly from bitbucket
#'
#' This function is vectorised so you can install multiple packages in
#' a single command.
#'
#' @inheritParams install_github
#' @param auth_user your account username if you're attempting to install
#'   a package hosted in a private repository (and your username is different
#'   to \code{username})
#' @param password your password
#' @param ref Desired git reference; could be a commit, tag, or branch name.
#'   Defaults to master.
#' @seealso Bitbucket API docs:
#'   \url{https://confluence.atlassian.com/bitbucket/use-the-bitbucket-cloud-rest-apis-222724129.html}
#'
#' @export
#' @examples
#' \dontrun{
#' install_bitbucket("sulab/mygene.r@@default")
#' install_bitbucket("dannavarro/lsr-package")
#' }
install_bitbucket <- function(repo, ref = "master", subdir = NULL,
                              auth_user = NULL, password = NULL, ...) {

  remotes <- lapply(repo, bitbucket_remote, ref = ref,
    subdir = subdir, auth_user = auth_user, password = password)

  install_remotes(remotes, ...)
}

bitbucket_remote <- function(repo, ref = NULL, subdir = NULL,
                              auth_user = NULL, password = NULL, sha = NULL) {

  meta <- parse_git_repo(repo)

  remote("bitbucket",
    repo = meta$repo,
    subdir = meta$subdir %||% subdir,
    username = meta$username,
    ref = meta$ref %||% ref,
    sha = sha,
    auth_user = auth_user,
    password = password
  )
}

#' @export
remote_download.bitbucket_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading bitbucket repo ", x$username, "/", x$repo, "@", x$ref)
  }

  dest <- tempfile(fileext = paste0(".zip"))
  src <- paste("https://bitbucket.org/", x$username, "/", tolower(x$repo), "/get/",
    x$ref, ".zip", sep = "")

  if (!is.null(x$password)) {
    auth <- list(
      user = x$auth_user %||% x$username,
      password = x$password
    )
  } else {
    auth <- NULL
  }

  download(dest, src, basic_auth = auth)
}

#' @export
remote_metadata.bitbucket_remote <- function(x, bundle = NULL, source = NULL) {
  # Determine sha as efficiently as possible
  if (!is.null(x$sha)) {
    # Might be cached already (because re-installing)
    sha <- x$sha
  } else if (!is.null(bundle)) {
    # Might be able to get from zip archive
    sha <- git_extract_sha1(bundle)
  } else {
    # Don't know
    sha <- NULL
  }

  list(
    RemoteType = "bitbucket",
    RemoteRepo = x$repo,
    RemoteUsername = x$username,
    RemoteRef = x$ref,
    RemoteSha = sha,
    RemoteSubdir = x$subdir
  )
}

#' Install a package from a git repository
#'
#' It is vectorised so you can install multiple packages with
#' a single command. You do not need to have the \code{git2r} package,
#' or an external git client installed.
#'
#' @param url Location of package. The url should point to a public or
#'   private repository.
#' @param branch Name of branch or tag to use, if not master.
#' @param subdir A sub-directory within a git repository that may
#'   contain the package we are interested in installing.
#' @param git Whether to use the \code{git2r} package, or an external
#'   git client via system. Default is \code{git2r} if it is installed,
#'   otherwise an external git installation.
#' @param ... passed on to \code{install.packages}
#' @export
#' @examples
#' \dontrun{
#' install_git("git://github.com/hadley/stringr.git")
#' install_git("git://github.com/hadley/stringr.git", branch = "stringr-0.2")
#'}
install_git <- function(url, subdir = NULL, branch = NULL,
                        git = c("auto", "git2r", "external"), ...) {

  git_remote <- select_git_remote(match.arg(git))
  remotes <- lapply(url, git_remote, subdir = subdir, branch = branch)
  install_remotes(remotes, ...)
}


select_git_remote <- function(git) {
  if (git == "auto") {
    git <- if (pkg_installed("git2r")) "git2r" else "external"
  }

  list(git2r = git_remote_git2r, external = git_remote_xgit)[[git]]
}


git_remote_git2r <- function(url, subdir = NULL, branch = NULL) {
  remote("git2r",
    url = url,
    subdir = subdir,
    branch = branch
  )
}


git_remote_xgit <- function(url, subdir = NULL, branch = NULL) {
  remote("xgit",
    url = url,
    subdir = subdir,
    branch = branch
  )
}

#' @export
remote_download.git2r_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading git repo ", x$url)
  }

  bundle <- tempfile()
  git2r::clone(x$url, bundle, progress = FALSE)

  if (!is.null(x$branch)) {
    r <- git2r::repository(bundle)
    git2r::checkout(r, x$branch)
  }

  bundle
}

#' @export
remote_metadata.git2r_remote <- function(x, bundle = NULL, source = NULL) {
  if (!is.null(bundle)) {
    r <- git2r::repository(bundle)
    sha <- git2r::commits(r)[[1]]@sha
  } else {
    sha <- NULL
  }

  list(
    RemoteType = "git",
    RemoteUrl = x$url,
    RemoteSubdir = x$subdir,
    RemoteRef = x$ref,
    RemoteSha = sha
  )
}


#' @export
remote_download.xgit_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading git repo ", x$url)
  }

  bundle <- tempfile()

  args <- c('clone', '--depth', '1', '--no-hardlinks')
  if (!is.null(x$branch)) args <- c(args, "--branch", x$branch)
  args <- c(args, x$args, x$url, bundle)
  git(paste0(args, collapse = " "), quiet = quiet)

  bundle
}

#' @export
remote_metadata.xgit_remote <- function(x, bundle = NULL, source = NULL) {
  list(
    RemoteType = "git",
    RemoteUrl = x$url,
    RemoteSubdir = x$subdir,
    RemoteRef = x$ref,
    RemoteSha = xgit_remote_sha1(x$url),
    RemoteArgs = if (length(x$args) > 0) paste0(deparse(x$args), collapse = " ")
  )
}

#' @importFrom utils read.delim

xgit_remote_sha1 <- function(url, ref = "master") {
  refs <- git(paste("ls-remote", url, ref))

  refs_df <- read.delim(text = refs, stringsAsFactors = FALSE, sep = "\t",
    header = FALSE)
  names(refs_df) <- c("sha", "ref")

  refs_df$sha[1]
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
  if (methods::is(res, "error")) return(FALSE)

  ## download() sometimes just downloads the error page, because
  ## the libcurl backend in download.file() is broken
  ## If the request was successful (=submodules exist), then it has an
  ## 'sha' field.
  sha <- tryCatch(
    fromJSONFile(tmp)$sha,
    error = function(e) e
  )
  ! methods::is(sha, "error") && ! is.null(sha)
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
  if (methods::is(response, "error") || is.null(response$head)) {
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

  if (methods::is(response, "error") || !is.null(response$message)) {
    stop("Cannot find repo ", params$username, "/", params$repo, ".")
  }

  if (length(response) == 0L)
    stop("No releases found for repo ", params$username, "/", params$repo, ".")

  params$ref <- response[[1L]]$tag_name
  params
}

#' Parse a concise GitHub repo specification
#'
#' The current format is:
#' \code{[username/]repo[/subdir][#pull|@ref|@*release]}
#' The \code{*release} suffix represents the latest release.
#'
#' @param repo Character scalar, the repo specification.
#' @return List with members: \code{username}, \code{repo}, \code{subdir}
#'   \code{ref}, \code{pull}, \code{release}. Members that do not
#'   appear in the input repo specification are omitted.
#'
#' @export
#' @examples
#' parse_github_repo_spec("metacran/crandb")
#' parse_github_repo_spec("jeroenooms/curl@v0.9.3")
#' parse_github_repo_spec("jimhester/covr#47")
#' parse_github_repo_spec("hadley/dplyr@*release")
#' parse_github_repo_spec("mangothecat/remotes@550a3c7d3f9e1493a2ba")

parse_github_repo_spec <- function(repo) {
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
  params <- lapply(replace, function(r) gsub(github_rx, r, repo, perl = TRUE))
  if (params$invalid != "")
    stop(sprintf("Invalid git repo: %s", repo))
  params <- params[sapply(params, nchar) > 0]

  params
}

parse_git_repo <- function(repo) {
  params <- parse_github_repo_spec(repo)

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

#' Install a package from a local file
#'
#' This function is vectorised so you can install multiple packages in
#' a single command.
#'
#' @param path path to local directory, or compressed file (tar, zip, tar.gz
#'   tar.bz2, tgz2 or tbz)
#' @inheritParams install_url
#' @export
#' @examples
#' \dontrun{
#' dir <- tempfile()
#' dir.create(dir)
#' pkg <- download.packages("testthat", dir, type = "source")
#' install_local(pkg[, 2])
#' }

install_local <- function(path, subdir = NULL, ...) {
  remotes <- lapply(path, local_remote, subdir = subdir)
  install_remotes(remotes, ...)
}

local_remote <- function(path, subdir = NULL, branch = NULL, args = character(0)) {
  remote("local",
    path = path,
    subdir = subdir
  )
}

#' @export
remote_download.local_remote <- function(x, quiet = FALSE) {
  # Already downloaded - just need to copy to tempdir()
  bundle <- tempfile()
  dir.create(bundle)
  file.copy(x$path, bundle, recursive = TRUE)

  # file.copy() creates directory inside of bundle
  dir(bundle, full.names = TRUE)[1]
}

#' @export
remote_metadata.local_remote <- function(x, bundle = NULL, source = NULL) {
  list(
    RemoteType = "local",
    RemoteUrl = x$path,
    RemoteSubdir = x$subdir
  )
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
#' @param ... Other arguments passed on to \code{install.packages}.
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

#' Install a package from a url
#'
#' This function is vectorised so you can install multiple packages in
#' a single command.
#'
#' @param url location of package on internet. The url should point to a
#'   zip file, a tar file or a bzipped/gzipped tar file.
#' @param subdir subdirectory within url bundle that contains the R package.
#' @param ... Other arguments passed on to \code{install.packages}.
#' @export
#'
#' @examples
#' \dontrun{
#' install_url("https://github.com/hadley/stringr/archive/master.zip")
#' }

install_url <- function(url, subdir = NULL, ...) {
  remotes <- lapply(url, url_remote, subdir = subdir)
  install_remotes(remotes, ...)
}

url_remote <- function(url, subdir = NULL) {
  remote("url",
    url = url,
    subdir = subdir
  )
}

#' @importFrom tools file_ext
#' @export
remote_download.url_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading package from url: ", x$url)
  }

  ext <- if (grepl("\\.tar\\.gz$", x$url)) "tar.gz" else file_ext(x$url)

  bundle <- tempfile(fileext = paste0(".", ext))
  download(bundle, x$url)
}

#' @export
remote_metadata.url_remote <- function(x, bundle = NULL, source = NULL) {
  list(
    RemoteType = "url",
    RemoteUrl = x$url,
    RemoteSubdir = x$subdir
  )
}

#' Install specified version of a CRAN package.
#'
#' If you are installing an package that contains compiled code, you will
#' need to have an R development environment installed.  You can check
#' if you do by running \code{devtools::has_devel} (you need the
#' \code{devtools} package for this).
#'
#' @export
#' @family package installation
#' @param package package name
#' @param version If the specified version is NULL or the same as the most
#'   recent version of the package, this function simply calls
#'   \code{install.packages}. Otherwise, it looks at the list of
#'   archived source tarballs and tries to install an older version instead.
#' @param ... Other arguments passed on to \code{install.packages}.
#' @inheritParams utils::install.packages
#' @author Jeremy Stephens
#' @importFrom utils available.packages contrib.url install.packages

install_version <- function(package, version = NULL, repos = getOption("repos"), type = getOption("pkgType"), ...) {

  url <- download_version_url(package, version, repos, type)
  install_url(url, ...)
}

package_find_repo <- function(package, repos) {
  for (repo in repos) {
    if (length(repos) > 1)
      message("Trying ", repo)

    archive <-
      tryCatch({
        con <- gzcon(url(sprintf("%s/src/contrib/Meta/archive.rds", repo), "rb"))
        on.exit(close(con))
        readRDS(con)
      },
      warning = function(e) list(),
      error = function(e) list())

    info <- archive[[package]]
    if (!is.null(info)) {
      info$repo <- repo
      return(info)
    }
  }

  stop(sprintf("couldn't find package '%s'", package))
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

download_version_url <- function(package, version, repos, type) {

  contriburl <- contrib.url(repos, type)
  available <- available.packages(contriburl)

  if (package %in% row.names(available)) {
    current.version <- available[package, 'Version']
    if (is.null(version) || version == current.version) {
      row <- available[which(rownames(available) == package)[1], ]
      return(paste0(
        row[["Repository"]],
        "/",
        row[["Package"]],
        "_",
        row[["Version"]],
        ".tar.gz"
      ))
    }
  }

  info <- package_find_repo(package, repos)

  if (is.null(version)) {
    # Grab the latest one: only happens if pulled from CRAN
    package.path <- row.names(info)[nrow(info)]
  } else {
    package.path <- paste(package, "/", package, "_", version, ".tar.gz",
      sep = "")
    if (!(package.path %in% row.names(info))) {
      stop(sprintf("version '%s' is invalid for package '%s'", version,
        package))
    }
  }

  paste(info$repo[1L], "/src/contrib/Archive/", package.path, sep = "")
}

install <- function(pkgdir = ".", dependencies = NA, quiet = TRUE, ...) {

  if (file.exists(file.path(pkgdir, "src")) && ! has_devel()) {
    missing_devel_warning(pkgdir)
  }

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

#' Install package dependencies if needed.
#'
#' @inheritParams package_deps
#' @param threads Number of threads to start, passed to
#'   \code{install.packages} as \code{Ncpus}.
#' @param ... additional arguments passed to \code{\link{install.packages}}.
#' @export
#' @examples
#' \dontrun{install_deps(".")}

install_deps <- function(pkgdir = ".", dependencies = NA,
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

  dep_deps <- if (isTRUE(dependencies)) NA else dependencies

  update(
    packages,
    dependencies = dep_deps,
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

system_check <- function(command, args = character(), quiet = TRUE,
                         error = TRUE, path = ".") {

  out <- tempfile()
  err <- tempfile()
  on.exit(unlink(out), add = TRUE)
  on.exit(unlink(err), add = TRUE)

  ## We suppress warnings, they are given if the command
  ## exits with a non-zero status
  res <- in_dir(
    path,
    suppressWarnings(
      system2(command, args = args, stdout = out, stderr = err)
    )
  )

  res <- list(
    stdout = tryCatch(
      suppressWarnings(win2unix(read_char(out))),
      error = function(e) ""
    ),
    stderr = tryCatch(
      suppressWarnings(win2unix(read_char(err))),
      error = function(e) ""
    ),
    status = res
  )

  if (error && res$status != 0) {
    stop("Command ", command, " failed ", res$stderr)
  }

  if (! quiet) {
    if (! identical(res$stdout, NA_character_)) cat(res$stdout)
    if (! identical(res$stderr, NA_character_)) cat(res$stderr)
  }

  res
}

win2unix <- function(str) {
  gsub("\r\n", "\n", str, fixed = TRUE)
}

read_char <- function(path, ...) {
  readChar(path, nchars = file.info(path)$size, ...)
}

`%||%` <- function (a, b) if (!is.null(a)) a else b

`%:::%` <- function (p, f) get(f, envir = asNamespace(p))

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

pkg_installed <- function(pkg) {

  if (pkg %in% loadedNamespaces()) {
    TRUE
  } else if (requireNamespace(pkg, quietly = TRUE)) {
    try(unloadNamespace(pkg))
    TRUE
  } else {
    FALSE
  }
}

with_something <- function(set, reset = set) {
  function(new, code) {
    old <- set(new)
    on.exit(reset(old))
    force(code)
  }
}

in_dir <- with_something(setwd)

get_r_version <- function() {
  paste(R.version$major, sep = ".", R.version$minor)
}

set_libpaths <- function(paths) {
  old <- .libPaths()
  .libPaths(paths)
  invisible(old)
}

with_libpaths <- with_something(set_libpaths, .libPaths)

## There are two kinds of tar on windows, one needs --force-local
## not to interpret : characters, the other does not. We try both ways.

untar <- function(tarfile, ...) {
  if (os_type() == "windows") {
    tryCatch(
      utils::untar(tarfile, extras = "--force-local", ...),
      error = function(e) utils::untar(tarfile, ...)
    )

  } else {
    utils::untar(tarfile, ...)
  }
}

os_type <- function() {
  .Platform$OS.type
}

sys_type <- function() {
  if (.Platform$OS.type == "windows") {
    "windows"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "macos"
  } else if (Sys.info()["sysname"] == "Linux") {
    "linux"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    stop("Unknown OS")
  }
}


  install_github(...)

}
