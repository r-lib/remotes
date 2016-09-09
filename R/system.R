
system_check <- function(command, args = character(), quiet = TRUE,
                         error = TRUE, path = ".") {

  out <- tempfile()
  err <- tempfile()
  on.exit(unlink(out), add = TRUE)
  on.exit(unlink(err), add = TRUE)

  ## We suppress warnings, they are given if the command
  ## exits with a non-zero status
  res <- suppressWarnings(
    in_dir(
      path,
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
