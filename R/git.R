
# Extract the commit hash from a git archive. Git archives include the SHA1
# hash as the comment field of the tarball pax extended header
# (see https://www.kernel.org/pub/software/scm/git/docs/git-archive.html)
# For GitHub archives this should be the first header after the default one
# (512 byte) header.
git_extract_sha1_tar <- function(bundle) {

  # open the bundle for reading
  # We use gzcon for everything because (from ?gzcon)
  # > Reading from a connection which does not supply a ‘gzip’ magic
  # > header is equivalent to reading from the original connection
  conn <- gzcon(file(bundle, open = "rb", raw = TRUE))
  on.exit(close(conn))

  # The default pax header is 512 bytes long and the first pax extended header
  # with the comment should be 51 bytes long
  # `52 comment=` (11 chars) + 40 byte SHA1 hash
  len <- 0x200 + 0x33
  res <- rawToChar(readBin(conn, "raw", n = len)[0x201:len])

  if (grepl("^52 comment=", res)) {
    sub("52 comment=", "", res)
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
