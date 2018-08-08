
#' Install a package from a git repository
#'
#' It is vectorised so you can install multiple packages with
#' a single command. You do not need to have the \code{git2r} package,
#' or an external git client installed.
#'
#' @param url Location of package. The url should point to a public or
#'   private repository.
#' @param branch Name of branch, tag or SHA reference to use, if not HEAD.
#' @param subdir A sub-directory within a git repository that may
#'   contain the package we are interested in installing.
#' @param git Whether to use the \code{git2r} package, or an external
#'   git client via system. Default is \code{git2r} if it is installed,
#'   otherwise an external git installation.
#' @param ... passed on to \code{\link[utils]{install.packages}}
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
    sha <- git2r::commits(r)[[1]]$sha
  } else {
    sha <- NA_character_
  }

  list(
    RemoteType = "git2r",
    RemoteUrl = x$url,
    RemoteSubdir = x$subdir,
    RemoteBranch = x$branch,
    RemoteSha = sha
  )
}

#' @export
remote_package_name.git2r_remote <- function(remote, ...) {

  tmp <- tempfile()
  on.exit(unlink(tmp))
  description_path <- paste0(collapse = "/", c(remote$subdir, "DESCRIPTION"))

  # Try using git archive --remote to retrieve the DESCRIPTION, if the protocol
  # or server doesn't support that return NA
  res <- try(silent = TRUE,
    system_check(git_path(),
      args = c("archive", "-o", tmp, "--remote", remote$url,
        if (is.null(remote$branch)) "HEAD" else remote$branch,
        description_path),
      quiet = TRUE))

  if (inherits(res, "try-error")) {
    return(NA_character_)
  }

  # git archive returns a tar file, so extract it to tempdir and read the DCF
  utils::untar(tmp, files = description_path, exdir = tempdir())

  read_dcf(file.path(tempdir(), description_path))$Package
}

#' @export
remote_sha.git2r_remote <- function(remote, ...) {
  tryCatch({
    res <- git2r::remote_ls(remote$url, credentials=remote$credentials, ...)

    branch <- remote$branch %||% "HEAD"

    found <- grep(pattern = paste0("/", branch), x = names(res))

    # If none found, assume it is a Sha1, so return the branch
    if (length(found) == 0) {
      return(remote$branch)
    }

    unname(res[found[1]])
  }, error = function(e) NA_character_)
}

#' @export
format.xgit_remote <- function(x, ...) {
  "Git"
}

#' @export
format.git2r_remote <- function(x, ...) {
  "Git"
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
    RemoteType = "xgit",
    RemoteUrl = x$url,
    RemoteSubdir = x$subdir,
    RemoteBranch = x$branch,
    RemoteSha = remote_sha(x),
    RemoteArgs = if (length(x$args) > 0) paste0(deparse(x$args), collapse = " ")
  )
}

#' @importFrom utils read.delim

#' @export
remote_package_name.xgit_remote <- remote_package_name.git2r_remote

#' @export
remote_sha.xgit_remote <- function(remote, ...) {
  url <- remote$url
  branch <- remote$branch

  refs <- git(paste("ls-remote", url, branch))

  refs_df <- read.delim(text = refs, stringsAsFactors = FALSE, sep = "\t",
    header = FALSE)
  names(refs_df) <- c("sha", "ref")

  refs_df$sha[[1]]
}
