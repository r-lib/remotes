
`%||%` <- function (a, b) if (!is.null(a)) a else b

`%:::%` <- function (p, f) get(f, envir = asNamespace(p))

`%::%` <- function (p, f) get(f, envir = asNamespace(p))

viapply <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, integer(1L), ..., USE.NAMES = USE.NAMES)
}

vlapply <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X, FUN, logical(1L), ..., USE.NAMES = USE.NAMES)
}

rcmd <- function(cmd, args, path = R.home("bin"), quiet, fail_on_status = TRUE) {
  if (os_type() == "windows") {
    real_cmd <- file.path(path, "Rcmd.exe")
    args <- c(cmd, args)
  } else {
    real_cmd <- file.path(path, "R")
    args <- c("CMD", cmd, args)
  }

  stdoutfile <- tempfile()
  stderrfile <- tempfile()
  on.exit(unlink(c(stdoutfile, stderrfile), recursive = TRUE), add = TRUE)
  status <- system2(real_cmd, args, stderr = stderrfile, stdout = stdoutfile)
  out <- tryCatch(readLines(stdoutfile, warn = FALSE), error = function(x) "")
  err <- tryCatch(readLines(stderrfile, warn = FALSE), error = function(x) "")

  if (fail_on_status && status != 0) {
    cat("STDOUT:\n")
    cat(out, sep = "\n")
    cat("STDERR:\n")
    cat(err, sep = "\n")
    stop(sprintf("Error running '%s' (status '%i')", cmd, status), call. = FALSE)
  }
  if (!quiet) {
    cat(out, sep = "\n")
  }

  list(stdout = out, stderr = err, status = status)
}

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

has_package <- function(pkg) {
  if (pkg %in% loadedNamespaces()) {
    TRUE
  } else {
    requireNamespace(pkg, quietly = TRUE)
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

set_options <- function(x) {
  do.call(options, as.list(x))
}

with_options <- with_something(set_options)

# Read the current user .Rprofile. Here is the order it is searched, from
# ?Startup
#
# 'R_PROFILE_USER’ environment variable (and tilde expansion
# will be performed).  If this is unset, a file called ‘.Rprofile’
# is searched for in the current directory or in the user's home
# directory (in that order).  The user profile file is sourced into
# the workspace.
read_rprofile_user <- function() {
  f <- normalizePath(Sys.getenv("R_PROFILE_USER", ""), mustWork = FALSE)
  if (file.exists(f)) {
    return(readLines(f))
  }

  f <- normalizePath("~/.Rprofile", mustWork = FALSE)
  if (file.exists(f)) {
    return(readLines(f))
  }

  character()
}

with_rprofile_user <- function(new, code) {
  temp_rprofile <- tempfile()
  on.exit(unlink(temp_rprofile), add = TRUE)

  writeLines(c(read_rprofile_user(), new), temp_rprofile)
  with_envvar(c("R_PROFILE_USER" = temp_rprofile), {
    force(code)
  })
}

## There are two kinds of tar on windows, one needs --force-local
## not to interpret : characters, the other does not. We try both ways.

untar <- function(tarfile, ...) {
  if (os_type() == "windows") {

    tarhelp <- tryCatch(
      system2("tar", "--help", stdout = TRUE, stderr = TRUE),
      error = function(x) "")

    if (any(grepl("--force-local", tarhelp)))  {
      status <- try(
        suppressWarnings(utils::untar(tarfile, extras = "--force-local", ...)),
        silent = TRUE)
      if (! is_tar_error(status)) {
        return(status)

      } else {
        message("External tar failed with `--force-local`, trying without")
      }
    }
  }

  utils::untar(tarfile, ...)
}

is_tar_error <- function(status) {
  inherits(status, "try-error") ||
    is_error_status(status) ||
    is_error_status(attr(status, "status"))
}

is_error_status <- function(x) {
  is.numeric(x) && length(x) > 0 && !is.na(x) && x != 0
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

is_dir <- function(path) {
  file.info(path)$isdir
}

untar_description <- function(tarball, dir = tempfile()) {
  files <- untar(tarball, list = TRUE)
  desc <- grep("^[^/]+/DESCRIPTION$", files, value = TRUE)
  if (length(desc) < 1) stop("No 'DESCRIPTION' file in package")
  untar(tarball, desc, exdir = dir)
  file.path(dir, desc)
}

## copied from rematch2@180fb61
re_match <- function(text, pattern, perl = TRUE, ...) {

  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = perl, ...)

  start  <- as.vector(match)
  length <- attr(match, "match.length")
  end    <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[ start == -1 ] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE,
    .text = text,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {

    gstart  <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[ gstart == -1 ] <- NA_character_
    dim(groupstr) <- dim(gstart)

    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  class(res) <- c("tbl_df", "tbl", class(res))
  res
}

is_standalone <- function() {
  isTRUE(as.logical(Sys.getenv("R_REMOTES_STANDALONE", "false")))
}

# This code is adapted from the perl MIME::Base64 module https://perldoc.perl.org/MIME/Base64.html
# https://github.com/gisle/mime-base64/blob/cf23d49e517c6ed8f4b24295f63721e8c9935010/Base64.xs#L197

XX <- 255L
EQ <- 254L
INVALID <- XX

index_64 <- as.integer(c(
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,62, XX,XX,XX,63,
    52,53,54,55, 56,57,58,59, 60,61,XX,XX, XX,EQ,XX,XX,
    XX, 0, 1, 2,  3, 4, 5, 6,  7, 8, 9,10, 11,12,13,14,
    15,16,17,18, 19,20,21,22, 23,24,25,XX, XX,XX,XX,XX,
    XX,26,27,28, 29,30,31,32, 33,34,35,36, 37,38,39,40,
    41,42,43,44, 45,46,47,48, 49,50,51,XX, XX,XX,XX,XX,

    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX,
    XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX, XX,XX,XX,XX
))

base64_decode <- function(x) {
  if (is.character(x)) {
    x <- charToRaw(x)
  }

  len <- length(x)
  idx <- 1
  c <- integer(4)
  out <- raw()
  while(idx <= len) {
    i <- 1
    while(i <= 4) {
      uc <- index_64[[as.integer(x[[idx]]) + 1L]]
      idx <- idx + 1
      if (uc != INVALID) {
        c[[i]] <- uc
        i <- i + 1
      }
      if (idx > len) {
        if (i <= 4) {
          if (i <= 2) return(rawToChar(out))
          if (i == 3) {
            c[[3]] <- EQ
            c[[4]] <- EQ
          }
          break
        }
      }
  }

    if (c[[1]] == EQ || c[[2]] == EQ) {
      break
    }

    #print(sprintf("c1=%d,c2=%d,c3=%d,c4=%d\n", c[1],c[2],c[3],c[4]))

    out[[length(out) + 1]] <- as.raw(bitwOr(bitwShiftL(c[[1]], 2L), bitwShiftR(bitwAnd(c[[2]], 0x30), 4L)))

    if (c[[3]] == EQ) {
      break
    }

    out[[length(out) + 1]] <- as.raw(bitwOr(bitwShiftL(bitwAnd(c[[2]], 0x0F), 4L), bitwShiftR(bitwAnd(c[[3]], 0x3C), 2L)))

    if (c[[4]] == EQ) {
      break
    }

    out[[length(out) + 1]] <- as.raw(bitwOr(bitwShiftL(bitwAnd(c[[3]], 0x03), 6L), c[[4]]))
  }
  rawToChar(out)
}

build_url <- function(host, ...) {
  download_url(do.call(file.path, as.list(c(host, ...))))
}

download_url <- function(url) {
  if (!grepl("^[[:alpha:]]+://", url)) {
    scheme <- if (download_method_secure()) "https://" else "http://"
    return(paste0(scheme, url))
  }
  url
}

is_na <- function(x) {
  length(x) == 1 && is.na(x)
}

dir.exists <- function(paths) {
  if (getRversion() < "3.2") {
    x <- base::file.info(paths)$isdir
    !is.na(x) & x
  } else {
    ("base" %::% "dir.exists")(paths)
  }
}

is_binary_pkg <- function(x) {
  file_ext(x) %in% c("tgz", "zip")
}

format_str <- function(x, width = Inf, trim = TRUE, justify = "none", ...) {
  x <- format(x, trim = trim, justify = justify, ...)

  if (width < Inf) {
    x_width <- nchar(x, "width")
    too_wide <- x_width > width
    if (any(too_wide)) {
      x[too_wide] <- paste0(substr(x[too_wide], 1, width - 3), "...")
    }
  }
  x
}

warn_for_potential_errors <- function() {
  if (sys_type() == "windows" && grepl(" ", R.home()) &&
      getRversion() <= "3.4.2") {
    warning(immediate. = TRUE,
      "\n!!! Installation will probably fail!\n",
      "This version of R has trouble with building and installing packages if\n",
      "the R HOME directory (currently '", R.home(), "')\n",
      "has space characters. Possible workarounds include:\n",
      "- installing R to the C: drive,\n",
      "- installing it into a path without a space, or\n",
      "- creating a drive letter for R HOME via the `subst` windows command, and\n",
      "  starting R from the new drive.\n",
      "See also https://github.com/r-lib/remotes/issues/98\n")
  }
}

# Return all directories in the input paths
directories <- function(paths) {
  dirs <- unique(dirname(paths))
  out <- dirs[dirs != "."]
  while(length(dirs) > 0 && any(dirs != ".")) {
    out <- unique(c(out, dirs[dirs != "."]))
    dirs <- unique(dirname(dirs))
  }
  sort(out)
}

in_r_build_ignore <- function(paths, ignore_file) {
  ignore <- ("tools" %:::% "get_exclude_patterns")()

  if (file.exists(ignore_file)) {
    ignore <- c(ignore, readLines(ignore_file, warn = FALSE))
  }

  matches_ignores <- function(x) {
    any(vlapply(ignore, grepl, x, perl = TRUE, ignore.case = TRUE))
  }

  # We need to search for the paths as well as directories in the path, so
  # `^foo$` matches `foo/bar`
  should_ignore <- function(path) {
    any(vlapply(c(path, directories(path)), matches_ignores))
  }

  vlapply(paths, should_ignore)
}
