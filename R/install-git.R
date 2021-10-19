
#' Install a package from a git repository
#'
#' It is vectorised so you can install multiple packages with
#' a single command. You do not need to have the `git2r` package,
#' or an external git client installed.
#'
#' If you need to set git credentials for use in the `Remotes` field you can do
#' so by placing the credentials in the `remotes.git_credentials` global
#' option.
#'
#' @param url Location of package. The url should point to a public or
#'   private repository.
#' @param ref Name of branch, tag or SHA reference to use, if not HEAD.
#' @param branch Deprecated, synonym for ref.
#' @param subdir A sub-directory within a git repository that may
#'   contain the package we are interested in installing.
#' @param credentials A git2r credentials object passed through to clone.
#'   Supplying this argument implies using `git2r` with `git`.
#' @param git Whether to use the `git2r` package, or an external
#'   git client via system. Default is `git2r` if it is installed,
#'   otherwise an external git installation.
#' @param ... Other arguments passed on to [utils::install.packages()].
#' @inheritParams install_github
#' @family package installation
#' @export
#' @examples
#' \dontrun{
#' install_git("https://github.com/hadley/stringr.git")
#' install_git("https://github.com/hadley/stringr.git", ref = "stringr-0.2")
#' }
install_git <- function(url, subdir = NULL, ref = NULL, branch = NULL,
                        credentials = git_credentials(),
                        git = c("auto", "git2r", "external"),
                        dependencies = NA,
                        upgrade = c("default", "ask", "always", "never"),
                        force = FALSE,
                        quiet = FALSE,
                        build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
                        build_manual = FALSE, build_vignettes = FALSE,
                        repos = getOption("repos"),
                        type = getOption("pkgType"),
                        ...) {
  if (!missing(branch)) {
    warning("`branch` is deprecated, please use `ref`")
    ref <- branch
  }

  remotes <- lapply(url, git_remote,
    subdir = subdir, ref = ref,
    credentials = credentials, git = match.arg(git)
  )

  install_remotes(remotes,
    credentials = credentials,
    dependencies = dependencies,
    upgrade = upgrade,
    force = force,
    quiet = quiet,
    build = build,
    build_opts = build_opts,
    build_manual = build_manual,
    build_vignettes = build_vignettes,
    repos = repos,
    type = type,
    ...
  )
}


git_remote <- function(url, subdir = NULL, ref = NULL, credentials = git_credentials(),
                       git = c("auto", "git2r", "external"), ...) {
  git <- match.arg(git)
  if (git == "auto") {
    git <- if (!is_standalone() && pkg_installed("git2r")) "git2r" else "external"
  }

  if (!is.null(credentials) && git != "git2r") {
    stop("`credentials` can only be used with `git = \"git2r\"`", call. = FALSE)
  }

   url_parts = re_match( url,
         "(?<protocol>[^/]*://)?(?<authhost>[^/]+)(?<path>[^@]*)(@(?<ref>.*))?")

  ref <- ref %||% (if (url_parts$ref == "") NULL else url_parts$ref)

  url = paste0(url_parts$protocol, url_parts$authhost, url_parts$path)

  list(git2r = git_remote_git2r, external = git_remote_xgit)[[git]](url, subdir, ref, credentials)
}


git_remote_git2r <- function(url, subdir = NULL, ref = NULL, credentials = git_credentials()) {
  remote("git2r",
    url = url,
    subdir = subdir,
    ref = ref,
    credentials = credentials
  )
}


git_remote_xgit <- function(url, subdir = NULL, ref = NULL, credentials = git_credentials()) {
  remote("xgit",
    url = url,
    subdir = subdir,
    ref = ref
  )
}

#' @export
remote_download.git2r_remote <- function(x, quiet = FALSE) {
  if (!quiet) {
    message("Downloading git repo ", x$url)
  }

  bundle <- tempfile()
  git2r::clone(x$url, bundle, credentials = x$credentials, progress = FALSE)

  if (!is.null(x$ref)) {
    r <- git2r::repository(bundle)
    git2r::checkout(r, x$ref)
  }

  bundle
}

#' @export
remote_metadata.git2r_remote <- function(x, bundle = NULL, source = NULL, sha = NULL) {
  if (!is.null(bundle)) {
    r <- git2r::repository(bundle)
    sha <- git2r::commits(r)[[1]]$sha
  } else {
    sha <- NULL
  }

  list(
    RemoteType = "git2r",
    RemoteUrl = x$url,
    RemoteSubdir = x$subdir,
    RemoteRef = x$ref,
    RemoteSha = sha
  )
}

#' @export
remote_package_name.git2r_remote <- function(remote, ...) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  description_path <- paste0(collapse = "/", c(remote$subdir, "DESCRIPTION"))

  if (grepl("^https?://", remote$url)) {
    # assumes GitHub-style "<repo>/raw/<ref>/<path>" url
    url <- build_url(sub("\\.git$", "", remote$url), "raw", remote_sha(remote, ...), description_path)
    download_args <- list(path = tmp, url = url)
    if (!is.null(remote$credentials)) {
      if (inherits(remote$credentials, "cred_user_pass")) {
        download_args$basic_auth <- list(
          user = remote$credentials$username,
          password = remote$credentials$password
        )
      } else if (inherits(remote$credentials, "cred_env")) {
        if (Sys.getenv(remote$credentials$username) == "") {
          stop(paste0("Environment variable `", remote$credentials$username, "` is unset."), .call = FALSE)
        }
        if (Sys.getenv(remote$credentials$password) == "") {
          stop(paste0("Environment variable `", remote$credentials$password, "` is unset."), .call = FALSE)
        }
        download_args$basic_auth <- list(
          user = Sys.getenv(remote$credentials$username),
          password = Sys.getenv(remote$credentials$username)
       )
      } else if (inherits(remote$credentials, "cred_token")) {
        if (Sys.getenv(remote$credentials$token) == "") {
          stop(paste0("Environment variable `", remote$credentials$token, "` is unset."), .call = FALSE)
        }
        download_args$auth_token <- Sys.getenv(remote$credentials$token)
      } else if (inherits(remote$credentials, "cred_ssh_key")) {
        stop(paste(
          "Unable to fetch the package DESCRIPTION file using SSH key authentication.",
          "Try using `git2r::cred_user_pass`, `git2r::cred_env`, or `git2r::cred_token` instead of `git2r::cred_ssh_key` for authentication."
        ), .call = FALSE)
      } else {
        stop(paste(
          "`remote$credentials` is not NULL and it does not inherit from a recognized class.",
          "Recognized classes for `remote$credentials` are `cred_user_pass`, `cred_env`, `cred_token`, and `cred_ssh_key`."
        ), .call = FALSE)
      }
    }
    tryCatch({
      do.call(download, args = download_args)
      read_dcf(tmp)$Package
    }, error = function(e) {
      NA_character_
    })
  } else {
    # Try using git archive --remote to retrieve the DESCRIPTION, if the protocol
    # or server doesn't support that return NA
    res <- try(
      silent = TRUE,
      system_check(git_path(),
        args = c(
          "archive", "-o", tmp, "--remote", remote$url,
          if (is.null(remote$ref)) "HEAD" else remote$ref,
          description_path
        ),
        quiet = TRUE
      )
    )

    if (inherits(res, "try-error")) {
      return(NA_character_)
    }

    # git archive returns a tar file, so extract it to tempdir and read the DCF
    utils::untar(tmp, files = description_path, exdir = tempdir())

    read_dcf(file.path(tempdir(), description_path))$Package
  }
}

#' @export
remote_sha.git2r_remote <- function(remote, ...) {
  tryCatch(
    {
      # set suppressWarnings in git2r 0.23.0+
      res <- suppressWarnings(git2r::remote_ls(remote$url, credentials = remote$credentials))

      ref <- remote$ref %||% "HEAD"

      if (ref != "HEAD") ref <- paste0("/", ref)

      found <- grep(pattern = paste0(ref, "$"), x = names(res))

      # If none found, it is either a SHA, so return the pinned sha or NA
      if (length(found) == 0) {
        return(remote$ref %||% NA_character_)
      }

      unname(res[found[1]])
    },
    error = function(e) {
      warning(e)
      NA_character_
    }
  )
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

  args <- c("clone", "--depth", "1", "--no-hardlinks")
  args <- c(args, x$args, x$url, bundle)
  git(paste0(args, collapse = " "), quiet = quiet)

  if (!is.null(x$ref)) {
    git(paste0(c("fetch", "origin", x$ref), collapse = " "), quiet = quiet, path = bundle)
    git(paste0(c("checkout", "FETCH_HEAD"), collapse = " "), quiet = quiet, path = bundle)
  }

  bundle
}

#' @export
remote_metadata.xgit_remote <- function(x, bundle = NULL, source = NULL, sha = NULL) {
  if (is_na(sha)) {
    sha <- NULL
  }

  list(
    RemoteType = "xgit",
    RemoteUrl = x$url,
    RemoteSubdir = x$subdir,
    RemoteRef = x$ref,
    RemoteSha = sha,
    RemoteArgs = if (length(x$args) > 0) paste0(deparse(x$args), collapse = " ")
  )
}

#' @importFrom utils read.delim

#' @export
remote_package_name.xgit_remote <- remote_package_name.git2r_remote

#' @export
remote_sha.xgit_remote <- function(remote, ...) {
  url <- remote$url
  ref <- remote$ref

  refs <- git(paste("ls-remote", url, ref))

  # If none found, it is either a SHA, so return the pinned SHA or NA
  if (length(refs) == 0) {
    return(remote$ref %||% NA_character_)
  }

  refs_df <- read.delim(
    text = refs, stringsAsFactors = FALSE, sep = "\t",
    header = FALSE
  )
  names(refs_df) <- c("sha", "ref")

  refs_df$sha[[1]]
}

#' Specify git credentials to use
#'
#' The global option `remotes.git_credentials` is used to set the git
#' credentials.
#' @export
#' @keywords internal
git_credentials <- function() {
  getOption("remotes.git_credentials", NULL)
}
