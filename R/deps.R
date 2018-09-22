
#' Find all dependencies of a CRAN or dev package.
#'
#' Find all the dependencies of a package and determine whether they are ahead
#' or behind CRAN. A `print()` method identifies mismatches (if any)
#' between local and CRAN versions of each dependent package; an
#' `update()` method installs outdated or missing packages from CRAN.
#'
#' @param packages A character vector of package names.
#' @param pkgdir path to a package directory, or to a package tarball.
#' @param dependencies Which dependencies do you want to check?
#'   Can be a character vector (selecting from "Depends", "Imports",
#'    "LinkingTo", "Suggests", or "Enhances"), or a logical vector.
#'
#'   `TRUE` is shorthand for "Depends", "Imports", "LinkingTo" and
#'   "Suggests". `NA` is shorthand for "Depends", "Imports" and "LinkingTo"
#'   and is the default. `FALSE` is shorthand for no dependencies (i.e.
#'   just check this package, not its dependencies).
#' @param quiet If `TRUE`, suppress output.
#' @param upgrade If `TRUE`, also upgrade any of out date dependencies.
#' @param repos A character vector giving repositories to use.
#' @param type Type of package to `update`.
#'
#' @param object A `package_deps` object.
#' @param ... Additional arguments passed to `install_packages`.
#' @inheritParams install_github
#'
#' @return
#'
#' A `data.frame` with columns:
#'
#' \tabular{ll}{
#' `package` \tab The dependent package's name,\cr
#' `installed` \tab The currently installed version,\cr
#' `available` \tab The version available on CRAN,\cr
#' `diff` \tab An integer denoting whether the locally installed version
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

  repos <- fix_repositories(repos)
  cran <- available_packages(repos, type)

  deps <- sort(find_deps(packages, available = cran, top_dep = dependencies))

  # Remove base packages
  inst <- utils::installed.packages()
  base <- unname(inst[inst[, "Priority"] %in% c("base", "recommended"), "Package"])
  deps <- setdiff(deps, base)

  # get remote types
  remote <- structure(lapply(deps, package2remote, repos = repos, type = type), class = "remotes")

  inst_ver <- vapply(deps, local_sha, character(1))
  cran_ver <- vapply(remote, function(x) remote_sha(x), character(1))
  is_cran_remote <- vapply(remote, inherits, logical(1), "cran_remote")

  diff <- compare_versions(inst_ver, cran_ver, is_cran_remote)

  res <- structure(
    data.frame(
      package = deps,
      installed = inst_ver,
      available = cran_ver,
      diff = diff,
      is_cran = is_cran_remote,
      stringsAsFactors = FALSE
    ),
    class = c("package_deps", "data.frame"),
    repos = repos,
    type = type
  )

  res$remote <- remote

  res
}

#' `local_package_deps` extracts dependencies from a
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

#' `dev_package_deps` lists the status of the dependencies
#' of a local package.
#'
#' @export
#' @rdname package_deps

dev_package_deps <- function(pkgdir = ".", dependencies = NA,
                             repos = getOption("repos"),
                             type = getOption("pkgType"), ...) {

  pkg <- load_pkg_description(pkgdir)
  repos <- c(repos, parse_additional_repositories(pkg))

  deps <- local_package_deps(pkgdir = pkgdir, dependencies = dependencies)

  if (is_bioconductor(pkg)) {
    bioc_repos <- bioc_install_repos()

    missing_repos <- setdiff(names(bioc_repos), names(repos))

    if (length(missing_repos) > 0)
      repos[missing_repos] <- bioc_repos[missing_repos]
  }

  combine_deps(
    package_deps(deps, repos = repos, type = type),
    remote_deps(pkg, ...))
}

combine_deps <- function(cran_deps, remote_deps) {
  deps <- rbind(cran_deps, remote_deps)

  # Only keep the remotes that are specified in the cran_deps
  # Keep only the Non-CRAN remotes if there are duplicates as we want to install
  # the development version rather than the CRAN version. The remotes will
  # always be specified after the CRAN dependencies, so using fromLast will
  # filter out the CRAN dependencies.
  deps[!duplicated(deps$package, fromLast = TRUE), ]
}

## -2 = not installed, but available on CRAN
## -1 = installed, but out of date
##  0 = installed, most recent version
##  1 = installed, version ahead of CRAN
##  2 = package not on CRAN

compare_versions <- function(inst, remote, is_cran) {
  stopifnot(length(inst) == length(remote) && length(inst) == length(is_cran))

  compare_var <- function(i, c, cran) {
    if (!cran) {
      if (identical(i, c)) {
        return(CURRENT)
      } else {
        return(BEHIND)
      }
    }
    if (is.na(c)) return(UNAVAILABLE)           # not on CRAN
    if (is.na(i)) return(UNINSTALLED)           # not installed, but on CRAN

    i <- package_version(i)
    c <- package_version(c)

    if (i < c) {
      BEHIND                               # out of date
    } else if (i > c) {
      AHEAD                                # ahead of CRAN
    } else {
      CURRENT                              # most recent CRAN version
    }
  }

  vapply(seq_along(inst),
    function(i) compare_var(inst[[i]], remote[[i]], is_cran[[i]]),
    integer(1))
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

UNINSTALLED <- -2L
BEHIND <- -1L
CURRENT <- 0L
AHEAD <- 1L
UNAVAILABLE <- 2L

#' @export
#' @rdname package_deps
#' @importFrom stats update

update.package_deps <- function(object,
                           dependencies = NA,
                           upgrade = TRUE,
                           force = FALSE,
                           quiet = FALSE,
                           build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                           repos = getOption("repos"),
                           type = getOption("pkgType"),
                           ...) {

  unavailable_on_cran <- object$diff == UNAVAILABLE & object$is_cran

  unknown_remotes <- object$diff == UNAVAILABLE & !object$is_cran

  if (any(unavailable_on_cran) && !quiet) {
    message("Skipping ", sum(unavailable_on_cran), " packages not available: ",
      paste(object$package[unavailable_on_cran], collapse = ", "))
  }

  if (any(unknown_remotes)) {
    if (upgrade) {
      install_remotes(object$remote[unknown_remotes],
                      dependencies = dependencies,
                      upgrade = upgrade,
                      force = force,
                      quiet = quiet,
                      build = build,
                      build_opts = build_opts,
                      repos = repos,
                      type = type,
                      ...)
    } else if (!quiet) {
      message("Skipping ", sum(unknown_remotes), " packages not available: ",
        paste(object$package[unknown_remotes], collapse = ", "))
    }
  }

  ahead_of_cran <- object$diff == AHEAD & object$is_cran
  if (any(ahead_of_cran) && !quiet) {
    message("Skipping ", sum(ahead_of_cran), " packages ahead of CRAN: ",
      paste(object$package[ahead_of_cran], collapse = ", "))
  }

  ahead_remotes <- object$diff == AHEAD & !object$is_cran
  if (any(ahead_remotes)) {
    if (upgrade) {
      install_remotes(object$remote[ahead_remotes],
                      dependencies = dependencies,
                      upgrade = upgrade,
                      force = force,
                      quiet = quiet,
                      build = build,
                      build_opts = build_opts,
                      repos = repos,
                      type = type,
                      ...)
    } else if (!quiet) {
      message("Skipping ", sum(ahead_remotes), " packages ahead of remote: ",
        paste(object$package[ahead_remotes], collapse = ", "))
    }
  }

  if (upgrade) {
    behind <- object$diff < CURRENT
  } else {
    behind <- is.na(object$installed)
  }

  if (any(object$is_cran & behind)) {
    install_packages(object$package[object$is_cran & behind], repos = attr(object, "repos"),
      type = attr(object, "type"), dependencies = dependencies, quiet = quiet, ...)
  }

  install_remotes(object$remote[!object$is_cran & behind],
                  dependencies = dependencies,
                  upgrade = upgrade,
                  force = force,
                  quiet = quiet,
                  build = build,
                  build_opts = build_opts,
                  repos = repos,
                  type = type,
                  ...)

  invisible()
}

install_packages <- function(packages, repos = getOption("repos"),
                             type = getOption("pkgType"), ...,
                             dependencies = FALSE, quiet = NULL) {

  # We want to pass only args that exist in the downstream functions
  args_to_keep <-
    unique(
      names(
        c(
          formals(utils::install.packages),
          formals(utils::download.file)
        )
      )
    )

  args <- list(...)
  args <- args[names(args) %in% args_to_keep]

  if (is.null(quiet))
    quiet <- !identical(type, "source")

  message("Installing ", length(packages), " packages: ",
    paste(packages, collapse = ", "))

  do.call(
    safe_install_packages,
    c(list(
        packages,
        repos = repos,
        type = type,
        dependencies = dependencies,
        quiet = quiet
      ),
      args
    )
  )
}

find_deps <- function(packages, available = available_packages(),
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

#' Standardise dependencies using the same logical as [install.packages]
#'
#' @param x The dependencies to standardise.
#'   A character vector (selecting from "Depends", "Imports",
#'    "LinkingTo", "Suggests", or "Enhances"), or a logical vector.
#'
#'   `TRUE` is shorthand for "Depends", "Imports", "LinkingTo" and
#'   "Suggests". `NA` is shorthand for "Depends", "Imports" and "LinkingTo"
#'   and is the default. `FALSE` is shorthand for no dependencies.
#'
#' @seealso <http://r-pkgs.had.co.nz/description.html#dependencies> for
#' additional information on what each dependency type means.
#' @keywords internal
#' @export
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
#' Works similarly to [utils::install.packages()] but doesn't install packages
#' that are already installed, and also upgrades out dated dependencies.
#'
#' @param packages Character vector of packages to update.
#' @inheritParams install_github
#' @seealso [package_deps()] to see which packages are out of date/
#'   missing.
#' @export
#' @examples
#' \dontrun{
#' update_packages("ggplot2")
#' update_packages(c("plyr", "ggplot2"))
#' }

update_packages <- function(packages = TRUE,
                            dependencies = NA,
                            upgrade = TRUE,
                            force = FALSE,
                            quiet = FALSE,
                            build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                            repos = getOption("repos"),
                            type = getOption("pkgType"),
                            ...) {
  if (isTRUE(packages)) {
    packages <- utils::installed.packages()[, "Package"]
  }

  pkgs <- package_deps(packages, repos = repos, type = type)
  update(pkgs,
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
  repos[repos == "@CRAN@"] <- download_url("cloud.r-project.org")
  repos
}

parse_one_remote <- function(x, ...) {
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
  tryCatch({
    # We need to use `environment(sys.function())` instead of
    # `asNamespace("remotes")` because when used as a script in
    # install-github.R there is no remotes namespace.

    fun <- get(paste0(tolower(type), "_remote"),
      envir = environment(sys.function()), mode = "function", inherits = FALSE)

    res <- fun(repo, ...)
    }, error = function(e) stop("Unknown remote type: ", type, "\n  ", conditionMessage(e), call. = FALSE)
  )
  res
}

split_remotes <- function(x) {
  pkgs <- trim_ws(unlist(strsplit(x, ",[[:space:]]*")))
  if (any((res <- grep("[[:space:]]+", pkgs)) != -1)) {
    stop("Missing commas separating Remotes: '", pkgs[res], "'", call. = FALSE)
  }
  pkgs
}


remote_deps <- function(pkg, ...) {
  if (!has_dev_remotes(pkg)) {
    return(NULL)
  }

  dev_packages <- split_remotes(pkg[["remotes"]])
  remote <- lapply(dev_packages, parse_one_remote, ...)

  package <- vapply(remote, remote_package_name, character(1), USE.NAMES = FALSE)
  installed <- vapply(package, local_sha, character(1), USE.NAMES = FALSE)
  available <- vapply(remote, remote_sha, character(1), USE.NAMES = FALSE)
  diff <- installed == available
  diff <- ifelse(!is.na(diff) & diff, CURRENT, BEHIND)

  res <- structure(
    data.frame(
      package = package,
      installed = installed,
      available = available,
      diff = diff,
      is_cran = FALSE,
      stringsAsFactors = FALSE
      ),
    class = c("package_deps", "data.frame"))

  res$remote <- structure(remote, class = "remotes")

  res
}
