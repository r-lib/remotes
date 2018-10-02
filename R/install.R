install <- function(pkgdir, dependencies, quiet, build, build_opts, upgrade,
                    repos, type, ...) {

  if (file.exists(file.path(pkgdir, "src"))) {
    if (has_package("pkgbuild")) {
      pkgbuild::local_build_tools(required = TRUE)
    } else if (!has_devel()) {
      missing_devel_warning(pkgdir)
    }
  }

  ## Check for circular dependencies. We need to know about the root
  ## of the install process.
  if (is_root_install()) on.exit(exit_from_root_install(), add = TRUE)
  if (check_for_circular_dependencies(pkgdir, quiet)) {
    return(invisible(FALSE))
  }

  install_deps(pkgdir, dependencies = dependencies, quiet = quiet,
    build = build, build_opts = build_opts, repos = repos, type = type, ...)

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

  pkg_name <- load_pkg_description(pkgdir)$package
  invisible(pkg_name)
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

safe_build_package <- function(pkgdir, build_opts, dest_path, quiet, use_pkgbuild = !is_standalone() && pkg_installed("pkgbuild")) {
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

    message("Running `R CMD build`...")
    in_dir(dest_path, {
      with_envvar(env, {
        output <- rcmd("build", c(build_opts, shQuote(pkgdir)), quiet = quiet,
                       fail_on_status = FALSE)
      })
    })

    if (output$status != 0) {
      cat("STDOUT:\n")
      cat(output$stdout, sep = "\n")
      cat("STDERR:\n")
      cat(output$stderr, sep = "\n")
      if (sys_type() == "windows" &&
          any(grepl("over-long path length", output$stderr))) {
        message(
          "\nIt seems that this package contains files with very long paths.\n",
          "This is not supported on most Windows versions. Please contact the\n",
          "package authors and tell them about this. See this GitHub issue\n",
          "for more details: https://github.com/r-lib/remotes/issues/84\n")
      }
      stop(sprintf("Failed to `R CMD build` package, try `build = FALSE`."),
           call. = FALSE)
    }

    file.path(dest_path, sub("^[*] building[^[:alnum:]]+([[:alnum:]_.]+)[^[:alnum:]]+$", "\\1", output$stdout[length(output$stdout)]))
  }
}

#' Install package dependencies if needed.
#'
#' @inheritParams package_deps
#' @param ... additional arguments passed to [utils::install.packages()].
#' @param build If `TRUE` build the package before installing.
#' @param build_opts Options to pass to `R CMD build`, only used when `build`
#' is `TRUE`.
#' @export
#' @examples
#' \dontrun{install_deps(".")}

install_deps <- function(pkgdir = ".", dependencies = NA,
                         repos = getOption("repos"),
                         type = getOption("pkgType"),
                         upgrade = FALSE,
                         quiet = FALSE,
                         build = TRUE,
                         build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                         ...) {

  packages <- dev_package_deps(
    pkgdir,
    repos = repos,
    dependencies = dependencies,
    type = type,
    ...
  )

  dep_deps <- if (isTRUE(dependencies)) NA else dependencies

  if (!quiet) {
    print(packages)
  }

  update(
    packages,
    dependencies = dep_deps,
    quiet = quiet,
    upgrade = upgrade,
    build = build,
    build_opts = build_opts,
    ...
  )
}
