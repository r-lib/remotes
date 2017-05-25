
#' Install a package from a git repository
#'
#' It is vectorised so you can install multiple packages with
#' a single command. You do not need to have the \code{git2r} package,
#' or an external git client installed.
#'
#' @param url Location of package. The url should point to a public or
#'   private repository.
#' @param branch Name of branch or tag to use, if not master.
#' @param submodules if \code{TRUE} download submodules before installing.
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
install_git <- function(url, subdir = NULL, branch = NULL, submodules = FALSE,
                        git = c("auto", "git2r", "external"), ...) {

  git_remote <- select_git_remote(match.arg(git))
  remotes <- lapply(url, git_remote, subdir = subdir, branch = branch,
                    submodules=submodules)
  install_remotes(remotes, ...)
}


select_git_remote <- function(git) {
  if (git == "auto") {
    git <- if (pkg_installed("git2r")) "git2r" else "external"
  }

  list(git2r = git_remote_git2r, external = git_remote_xgit)[[git]]
}


git_remote_git2r <- function(url, subdir = NULL, branch = NULL,
                             submodules = submodules) {
  remote("git2r",
    url = url,
    subdir = subdir,
    branch = branch,
    submodules = submodules
  )
}


git_remote_xgit <- function(url, subdir = NULL, branch = NULL,
                            submodules = submodules) {
  remote("xgit",
    url = url,
    subdir = subdir,
    branch = branch,
    submodules = submodules
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

read_gitmodules <- function(file) {
  modules <- readChar(file, file.info(file)$size)
  modules <- gsub('\n(\\[submodule \\".*?\\"\\])', '%split%\\1', modules)
  modules <- strsplit(modules, '%split%')
  modules <- lapply(unlist(modules), parse_module)
  modules <- modules[sapply(modules, function(x) all(c("name", "path", "url", "branch") %in% names(x)))]
  modules
}

parse_module <- function(module_string) {
  module_strings <- unlist(strsplit(module_string, '\n'))
  module <- module_string[-1]

  name <- gsub('\\[submodule \\"(.*?)\\"\\]',
               '\\1',
               module_strings[grepl('submodule', module_strings)])
  path <- gsub('.*= *(.*)',
               '\\1',
               module_strings[grepl('path', module_strings)])
  url <- gsub('.*= *(.*)',
              '\\1',
              module_strings[grepl('url', module_strings)])
  branch <- gsub('.*= *(.*)',
                 '\\1',
                 module_strings[grepl('branch', module_strings)])

  # if there is no url string return an empty list, which we filter out later
  if(!any(grepl('url', module_strings))){
    return(list())
  }

  # if there is no branch string, set branch to NULL
  if(!any(grepl('branch', module_strings))){
    branch <- NULL
  }


  list(name = name, path = path, url = url, branch = branch)
}

download_module <- function(module_list, source) {
  # convert git@ ssh syntax to http:// syntax
  module_list$url <- gsub("git@(.*):(.*)/(.*).git", "http://\\1/\\2/\\3", module_list$url)

  message("Downloading submodule ", module_list$name, " from ", module_list$url)
  git2r::clone(module_list$url,
               file.path(source, module_list$path),
               branch = module_list$branch,
               progress = FALSE)

  return(TRUE)
}

download_modules <- function(modules_list, ...) {
  invisible(vapply(modules_list, download_module, ..., FUN.VALUE = logical(1)))
}
