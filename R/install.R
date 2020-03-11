install <- function(pkgdir, dependencies, quiet, build, build_opts, build_manual, build_vignettes,
                    upgrade, repos, type, ...) {

  warn_for_potential_errors()

  if (file.exists(file.path(pkgdir, "src"))) {
    if (has_package("pkgbuild")) {
      pkgbuild::local_build_tools(required = TRUE)
    } else if (!has_devel()) {
      missing_devel_warning(pkgdir)
    }
  }

  pkg_name <- load_pkg_description(pkgdir)$package

  ## Check for circular dependencies. We need to know about the root
  ## of the install process.
  if (is_root_install()) on.exit(exit_from_root_install(), add = TRUE)
  if (check_for_circular_dependencies(pkgdir, quiet)) {
    return(invisible(pkg_name))
  }

  install_deps(pkgdir, dependencies = dependencies, quiet = quiet,
    build = build, build_opts = build_opts, build_manual = build_manual,
    build_vignettes = build_vignettes, upgrade = upgrade, repos = repos,
    type = type, ...)

  if (isTRUE(build)) {
    dir <- tempfile()
    dir.create(dir)
    on.exit(unlink(dir), add = TRUE)

    pkgdir <- safe_build_package(pkgdir, build_opts, build_manual, build_vignettes, dir, quiet)
  }

  safe_install_packages(
    pkgdir,
    repos = NULL,
    quiet = quiet,
    type = "source",
    ...
  )

  invisible(pkg_name)
}


safe_install_packages <- function(...) {

  lib <- paste(.libPaths(), collapse = .Platform$path.sep)

  if (!is_standalone() &&
      has_package("crancache") && has_package("callr")) {
    i.p <- "crancache" %::% "install_packages"
  } else {
    i.p <- utils::install.packages
  }

  with_options(list(install.lock = getOption("install.lock", TRUE)), {
    with_envvar(
      c(R_LIBS = lib,
        R_LIBS_USER = lib,
        R_LIBS_SITE = lib,
        RGL_USE_NULL = "TRUE"),

      # Set options(warn = 2) for this process and child processes, so that
      # warnings from `install.packages()` are converted to errors.
      if (should_error_for_warnings()) {
        with_options(list(warn = 2),
          with_rprofile_user("options(warn = 2)",
            i.p(...)
          )
        )
      } else {
        i.p(...)
      }
    )
  })
}

normalize_build_opts <- function(build_opts, build_manual, build_vignettes) {
  if (!isTRUE(build_manual)) {
    build_opts <- union(build_opts, "--no-manual")
  } else {
    build_opts <- setdiff(build_opts, "--no-manual")
  }

  if (!isTRUE(build_vignettes)) {
    build_opts <- union(build_opts, "--no-build-vignettes")
  } else {
    build_opts <- setdiff(build_opts, "--no-build-vignettes")
  }

  build_opts
}

safe_build_package <- function(pkgdir, build_opts, build_manual, build_vignettes, dest_path, quiet, use_pkgbuild = !is_standalone() && pkg_installed("pkgbuild")) {
  build_opts <- normalize_build_opts(build_opts, build_manual, build_vignettes)

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

    lib <- paste(.libPaths(), collapse = .Platform$path.sep)
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
      msg_for_long_paths(output)
      stop(sprintf("Failed to `R CMD build` package, try `build = FALSE`."),
           call. = FALSE)
    }

    building_regex <- paste0(
      "^[*] building[^[:alnum:]]+",     # prefix, "* building '"
      "([-[:alnum:]_.]+)",              # package file name, e.g. xy_1.0-2.tar.gz
      "[^[:alnum:]]+$"                   # trailing quote
    )

    pkgfile <- sub(building_regex, "\\1", output$stdout[length(output$stdout)])
    file.path(dest_path, pkgfile)
  }
}

msg_for_long_paths <- function(output) {
  if (sys_type() == "windows" &&
      (r_error_matches("over-long path", output$stderr) ||
       r_error_matches("over-long path length", output$stderr))) {
    message(
      "\nIt seems that this package contains files with very long paths.\n",
      "This is not supported on most Windows versions. Please contact the\n",
      "package authors and tell them about this. See this GitHub issue\n",
      "for more details: https://github.com/r-lib/remotes/issues/84\n")
  }
}

r_error_matches <- function(msg, str) {
  any(grepl(msg, str)) ||
    any(grepl(gettext(msg, domain = "R"), str))
}

#' Install package dependencies if needed.
#'
#' @inheritParams package_deps
#' @param ... additional arguments passed to [utils::install.packages()].
#' @param build If `TRUE` build the package before installing.
#' @param build_opts Options to pass to `R CMD build`, only used when `build`
#' @param build_manual If `FALSE`, don't build PDF manual ('--no-manual').
#' @param build_vignettes If `FALSE`, don't build package vignettes ('--no-build-vignettes').
#' is `TRUE`.
#' @export
#' @examples
#' \dontrun{install_deps(".")}

install_deps <- function(pkgdir = ".", dependencies = NA,
                         repos = getOption("repos"),
                         type = getOption("pkgType"),
                         upgrade = c("default", "ask", "always", "never"),
                         quiet = FALSE,
                         build = TRUE,
                         build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                         build_manual = FALSE, build_vignettes = FALSE,
                         ...) {
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
    quiet = quiet,
    upgrade = upgrade,
    build = build,
    build_opts = build_opts,
    build_manual = build_manual,
    build_vignettes = build_vignettes,
    type = type,
    ...
  )
}

should_error_for_warnings <- function() {

  force_suggests <- Sys.getenv("_R_CHECK_FORCE_SUGGESTS_", "true")

  no_errors <- Sys.getenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS", !config_val_to_logical(force_suggests))

  !config_val_to_logical(no_errors)
}
