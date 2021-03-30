DEFAULT_RSPM_REPO_ID <-  "1" # cran
DEFAULT_RSPM <-  "https://packagemanager.rstudio.com"

#' Query the system requirements for a dev package (and its dependencies)
#'
#' Returns a character vector of commands to run that will install system
#' requirements for the queried operating system.
#'
#' @param os,os_release The operating system and operating system release version, see
#'   <https://github.com/rstudio/r-system-requirements#operating-systems> for the
#'   list of supported operating systems.
#'
#'   If `os_release` is `NULL`, `os` must consist of the operating system
#'   and the version separated by a dash, e.g. `"ubuntu-18.04"`.
#' @param path The path to the dev package's root directory.
#' @param package A CRAN package name. If not `NULL`, this is used and `path` is ignored.
#' @param curl The location of the curl binary on your system.
#' @return A character vector of commands needed to install the system requirements for the package.
#' @export
system_requirements <- function(os, os_release = NULL, path = ".", package = NULL, curl = Sys.which("curl")) {
  if (is.null(os_release)) {
    os_release <- strsplit(os_release, "-", fixed = TRUE)[[1]]
    if (length(os_release) != 2) {
      stop("If os_release is missing, os must consist of name and release.", call. = FALSE)
    }

    os <- os_release[[1]]
    os_release <- os_release[[2]]
  }

  os_versions <- supported_os_versions()

  os <- match.arg(os, names(os_versions))

  os_release <- match.arg(os_release, os_versions[[os]])

  if (!nzchar(curl)) {
    stop("`curl` must be on the `PATH`.", call. = FALSE)
  }

  rspm <- Sys.getenv("RSPM_ROOT", DEFAULT_RSPM)
  rspm_repo_id <- Sys.getenv("RSPM_REPO_ID", DEFAULT_RSPM_REPO_ID)
  rspm_repo_url <- sprintf("%s/__api__/repos/%s", rspm, rspm_repo_id)

  if (!is.null(package)) {
    res <- system2(
      curl,
      args = c(
        "--silent",
        shQuote(sprintf("%s/sysreqs?all=false&pkgname=%s&distribution=%s&release=%s",
          rspm_repo_url,
          package,
          os,
          os_release)
      )),
      stdout = TRUE
    )
    res <- json$parse(res)

    pre_install <- unique(unlist(c(res[["pre_install"]], lapply(res[["requirements"]], function(x) x[["requirements"]][["pre_install"]]))))
    install_scripts <- unique(unlist(c(res[["install_scripts"]], lapply(res[["requirements"]], function(x) x[["requirements"]][["install_scripts"]]))))
  } else {
    desc_file <- normalizePath(file.path(path, "DESCRIPTION"), mustWork = FALSE)
    if (!file.exists(desc_file)) {
      stop("`", path, "` must contain a package.", call. = FALSE)
    }

    res <- system2(
      curl,
      args = c(
        "--silent",
        "--data-binary",
        shQuote(paste0("@", desc_file)),
        shQuote(sprintf("%s/sysreqs?distribution=%s&release=%s&suggests=true",
            rspm_repo_url,
            os,
            os_release)
        )
        ),
      stdout = TRUE
    )
    res <- json$parse(res)

    pre_install <- unique(unlist(c(res[["pre_install"]], lapply(res[["dependencies"]], `[[`, "pre_install"))))
    install_scripts <- unique(unlist(c(res[["install_scripts"]], lapply(res[["dependencies"]], `[[`, "install_scripts"))))
  }

  as.character(c(pre_install, install_scripts))
}

# Adapted from https://github.com/rstudio/r-system-requirements/blob/master/systems.json
# OSs commented out are not currently supported by the API
supported_os_versions <- function() {
  list(
    #"debian" = c("8", "9"),
    "ubuntu" = c("14.04", "16.04", "18.04", "20.04"),
    "centos" = c("6", "7", "8"),
    "redhat" = c("6", "7", "8"),
    "opensuse" = c("42.3", "15.0"),
    "sle" = c("12.3", "15.0")
    #"windows" = c("")
  )
}
