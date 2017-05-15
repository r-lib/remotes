
#' Find all dependencies of a CRAN or dev package.
#'
#' Find all the dependencies of a package and determine whether they are ahead
#' or behind CRAN. A \code{print()} method identifies mismatches (if any)
#' between local and CRAN versions of each dependent package; an
#' \code{update()} method installs outdated or missing packages from CRAN.
#'
#' @param packages A character vector of package names.
#' @param pkgdir path to a package directory, or to a package tarball.
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

  repos <- fix_repositories(repos)
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
      fun <- get(x = paste0("install_", tolower(type)), mode = "function"),
      error = function(e) {
        stop(
          "Malformed remote specification '", x, "'",
          ", error: ", conditionMessage(e),
          call. = FALSE
        )
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

fix_repositories <- function(repos) {
  if (length(repos) == 0)
    repos <- character()

  # Override any existing default values with the cloud mirror
  # Reason: A "@CRAN@" value would open a GUI for choosing a mirror
  repos[repos == "@CRAN@"] <- "http://cloud.r-project.org"
  repos
}
