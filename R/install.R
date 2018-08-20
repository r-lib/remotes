
install <- function(pkgdir = ".", dependencies = NA, quiet = TRUE, build =
  TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"), ..., repos =
  getOption("repos")) {

  if (file.exists(file.path(pkgdir, "src")) && ! has_devel()) {
    missing_devel_warning(pkgdir)
  }

  ## Check for circular dependencies. We need to know about the root
  ## of the install process.
  if (is_root_install()) on.exit(exit_from_root_install(), add = TRUE)
  if (check_for_circular_dependencies(pkgdir, quiet)) {
    return(invisible(FALSE))
  }

  install_deps(pkgdir, dependencies = dependencies, quiet = quiet,
    build = build, build_opts = build_opts, repos = repos, ...)

  if (isTRUE(build)) {
    dir <- tempfile()
    dir.create(dir)
    on.exit(unlink(dir), add = TRUE)

    pkgdir <- safe_build_package(pkgdir, build_opts, dir, quiet)
  }

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

  if (has_package("crancache") && has_package("callr")) {
    i.p <- "crancache" %::% "install_packages"
  } else {
    i.p <- utils::install.packages
  }

  with_envvar(
    c(R_LIBS = lib,
      R_LIBS_USER = lib,
      R_LIBS_SITE = lib),

    # Set options(warn = 2) for this process and child processes, so that
    # warnings from `install.packages()` are converted to errors.
    with_options(list(warn = 2),
      with_rprofile_user("options(warn = 2)",
        i.p(...)
      )
    )
  )
}

safe_build_package <- function(pkgdir, build_opts, dest_path, quiet, use_pkgbuild = is_installed("pkgbuild")) {
  if (use_pkgbuild) {
    vignettes <- TRUE
    manual <- FALSE
    has_no_vignettes <- grepl("--no-build-vignettes", build_opts)
    if (any(has_no_vignettes)) {
      vignettes <- FALSE
    }
    has_no_manual <- grepl("--no-manual", build_opts)
    if (!any(has_no_manual)) {
      manual <- TRUE
    }
    build_opts <- build_opts[!(has_no_vignettes | has_no_manual)]
    pkgbuild::build(pkgdir, dest_path = dest_path, binary = FALSE,
      vignettes = vignettes, manual = manual, args = build_opts, quiet = quiet)
  } else {
    # No pkgbuild, so we need to call R CMD build ourselves

    lib <- paste(.libPaths(), collapse = ":")
    env <- c(R_LIBS = lib,
      R_LIBS_USER = lib,
      R_LIBS_SITE = lib,
      R_PROFILE_USER = tempfile())

    pkgdir <- normalizePath(pkgdir)

    in_dir(dest_path, {
      with_envvar(env, {
        output <- rcmd("build", c(build_opts, shQuote(pkgdir)), quiet = quiet)
      })
    })

    file.path(dest_path, gsub("^[*] building .(.*).$", "\\1", output[length(output)]))
  }
}

#' Install package dependencies if needed.
#'
#' @inheritParams package_deps
#' @param threads Number of threads to start, passed to
#'   \code{\link[utils]{install.packages}} as \code{Ncpus}.
#' @param ... additional arguments passed to \code{\link[utils]{install.packages}}.
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
