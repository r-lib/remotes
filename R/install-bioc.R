#' Install a package from a Bioconductor repository
#'
#' This function requires \code{git} to be installed on your system in order to
#' be used.
#'
#' It is vectorised so you can install multiple packages with
#' a single command.
#'
#' '
#' @inheritParams install_git
#' @param repo Repository address in the format
#'   \code{[username:password@@][release/]repo[#commit]}. Valid values for
#'   the release are \sQuote{devel},
#'   \sQuote{release} (the default if none specified), or numeric release
#'   numbers (e.g. \sQuote{3.3}).
#' @param mirror The bioconductor git mirror to use
#' @param ... Other arguments passed on to \code{\link{install}}
#' @export
#' @family package installation
#' @examples
#' \dontrun{
#' install_bioc("SummarizedExperiment")
#' install_bioc("release/SummarizedExperiment")
#' install_bioc("3.3/SummarizedExperiment")
#' install_bioc("SummarizedExperiment#abc123")
#' install_bioc("user:password@release/SummarizedExperiment")
#' install_bioc("user:password@devel/SummarizedExperiment")
#' install_bioc("user:password@SummarizedExperiment#abc123")
#'}
install_bioc <- function(repo, mirror = getOption("BioC_git", "https://git.bioconductor.org/packages"),
  git = c("auto", "git2r", "external"), ..., quiet = FALSE) {

  bioc_remote <- select_bioc_git_remote(match.arg(git))

  remotes <- lapply(repo, bioc_remote, mirror = mirror)

  install_remotes(remotes, ..., quiet = quiet)
}

select_bioc_git_remote <- function(git) {
  if (git == "auto") {
    git <- if (pkg_installed("git2r")) "git2r" else "external"
  }
  switch(git,
    git2r = bioc_git2r_remote,
    external = bioc_xgit_remote)
}

# Parse concise git repo specification: [username:password@][branch/]repo[#commit]
parse_bioc_repo <- function(path) {
  user_pass_rx <- "(?:([^:]+):([^:@]+)@)?"
  release_rx <- "(?:(devel|release|[0-9.]+)/)?"
  repo_rx <- "([^/@#]+)"
  commit_rx <- "(?:[#]([a-zA-Z0-9]+))?"
  bioc_rx <- sprintf("^(?:%s%s%s%s|(.*))$", user_pass_rx, release_rx, repo_rx, commit_rx)

  param_names <- c("username", "password", "release", "repo", "commit", "invalid")
  replace <- stats::setNames(sprintf("\\%d", seq_along(param_names)), param_names)
  params <- lapply(replace, function(r) gsub(bioc_rx, r, path, perl = TRUE))
  if (params$invalid != "")
    stop(sprintf("Invalid bioc repo: %s", path))

  params <- params[sapply(params, nchar) > 0]

  if (!is.null(params$release) && !is.null(params$commit)) {
    stop("release and commit should not both be specified")
  }

  params
}

bioc_git2r_remote <- function(repo, mirror = getOption("BioC_git", "https://git.bioconductor.org/packages")) {
  meta <- parse_bioc_repo(repo)

  branch <- bioconductor_branch(meta$release, meta$sha)

  if (!is.null(meta$username) && !is.null(meta$password)) {
    meta$credentials <- git2r::cred_user_pass(meta$username, meta$password)
  }

  remote("bioc_git2r",
         mirror = mirror,
         repo = meta$repo,
         release = meta$release %||% "release",
         sha = meta$commit,
         branch = branch,
         credentials = meta$credentials
  )
}

bioc_xgit_remote <- function(repo, mirror = getOption("BioC_git", "https://git.bioconductor.org/packages")) {
  meta <- parse_bioc_repo(repo)

  branch <- bioconductor_branch(meta$release, meta$sha)

  if (!is.null(meta$username) && !is.null(meta$password)) {
    meta$credentials <- git2r::cred_user_pass(meta$username, meta$password)
  }

  remote("bioc_xgit",
         mirror = mirror,
         repo = meta$repo,
         release = meta$release %||% "release",
         sha = meta$commit,
         branch = branch,
         credentials = meta$credentials
  )
}

#' @export
remote_download.bioc_git2r_remote <- function(x, quiet = FALSE) {
  url <- paste0(x$mirror, "/", x$repo)

  if (!quiet) {
    message("Downloading Bioconductor repo ", url)
  }

  bundle <- tempfile()
  git2r::clone(url, bundle, credentials=x$credentials, progress = FALSE)

  if (!is.null(x$branch)) {
    r <- git2r::repository(bundle)
    git2r::checkout(r, x$branch)
  }

  bundle
}

#' @export
remote_download.bioc_xgit_remote <- function(x, quiet = FALSE) {
  url <- paste0(x$mirror, "/", x$repo)

  if (!quiet) {
    message("Downloading Bioconductor repo ", url)
  }

  bundle <- tempfile()

  args <- c('clone', '--depth', '1', '--no-hardlinks')

  if (!is.null(x$branch)) {
    args <- c(args, "--branch", x$branch)
  }

  args <- c(args, x$args, url, bundle)
  git(paste0(args, collapse = " "), quiet = quiet)

  bundle
}

#' @export
remote_metadata.bioc_git2r_remote <- function(x, bundle = NULL, source = NULL) {
  url <- paste0(x$mirror, "/", x$repo)

  if (!is.null(bundle)) {
    r <- git2r::repository(bundle)
    sha <- git_repo_sha1(r)
  } else {
    sha <- NULL
  }

  list(
    RemoteType = "bioc_git2r",
    RemoteMirror = x$mirror,
    RemoteRepo = x$repo,
    RemoteRelease = x$release,
    RemoteSha = sha,
    RemoteBranch = x$branch
  )
}

#' @export
remote_metadata.bioc_xgit_remote <- function(x, bundle = NULL, source = NULL) {
  list(
    RemoteType = "bioc_xgit",
    RemoteMirror = x$mirror,
    RemoteRepo = x$repo,
    RemoteRelease = x$release,
    RemoteSha = remote_sha(x),
    RemoteBranch = x$branch,
    RemoteArgs = if (length(x$args) > 0) paste0(deparse(x$args), collapse = " ")
  )
}

#' @export
remote_package_name.bioc_git2r_remote <- function(remote, ...) {
  remote$repo
}

#' @export
remote_package_name.bioc_xgit_remote <- function(remote, ...) {
  remote$repo
}

#' @export
remote_sha.bioc_git2r_remote <- function(remote, ...) {
  tryCatch({
    url <- paste0(remote$mirror, "/", remote$repo)

    res <- git2r::remote_ls(url, credentials=remote$credentials, ...)

    found <- grep(pattern = paste0("/", remote$branch), x = names(res))

    if (length(found) == 0) {
      return(NA)
    }

    unname(res[found[1]])
  }, error = function(e) NA_character_)
}

remote_sha.bioc_xgit_remote <- function(remote, ...) {
  url <- paste0(remote$mirror, "/", remote$repo)
  ref <- remote$branch

  refs <- git(paste("ls-remote", url, ref))

  refs_df <- read.delim(text = refs, stringsAsFactors = FALSE, sep = "\t",
    header = FALSE)
  names(refs_df) <- c("sha", "ref")

  refs_df$sha[[1]]
}

bioconductor_branch <- function(release, sha) {
  if (!is.null(sha)) {
    sha
  } else {
    if (is.null(release)) {
      release <- "release"
    }
    if (release == "release") {
      release <- bioconductor_release()
    }
    switch(
      tolower(release),
      devel = "master",
      paste0("RELEASE_",  gsub("\\.", "_", release))
    )
  }
}

bioconductor_release <- function() {
  tmp <- tempfile()
  utils::download.file("http://bioconductor.org/config.yaml", tmp, quiet = TRUE)

  gsub("release_version:[[:space:]]+\"([[:digit:].]+)\"", "\\1",
       grep("release_version:", readLines(tmp), value = TRUE))
}

#' @export
format.bioc_git2r_remote <- function(x, ...) {
  "Bioc"
}

#' @export
format.bioc_xgit_remote <- function(x, ...) {
  "Bioc"
}

# sha of most recent commit
git_repo_sha1 <- function(r) {
  rev <- git2r::repository_head(r)
  if (is.null(rev)) {
    return(NULL)
  }

  if (git2r::is_commit(rev)) {
    rev$sha
  } else {
    git2r::branch_target(rev)
  }
}

