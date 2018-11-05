## This is mostly from https://bioconductor.org/biocLite.R

bioc_version <- function() {
  bver <- get(
    ".BioC_version_associated_with_R_version",
    envir = asNamespace("tools"),
    inherits = FALSE
  )

  if (is.function(bver)) {
    bver()
  } else {
    bver
  }
}

bioc_repos <- function(bioc_ver = bioc_version()) {
  bioc_ver <- as.package_version(bioc_ver)

  a <- NULL

  p <- file.path(Sys.getenv("HOME"), ".R", "repositories")
  if (file.exists(p)) {
    a <- ("tools" %:::% ".read_repositories")(p)
    if (!"BioCsoft" %in% rownames(a)) a <- NULL
  }

  if (is.null(a)) {
    p <- file.path(R.home("etc"), "repositories")
    a <- ("tools" %:::% ".read_repositories")(p)
  }

  # BioCextra was removed in Bioc 3.6
  if (bioc_ver < "3.6") {
    repo_types <- c("BioCsoft", "BioCann", "BioCexp", "BioCextra")
  } else {
    repo_types <- c("BioCsoft", "BioCann", "BioCexp")
  }

  repos <- intersect(
    rownames(a),
    repo_types
  )

  default_bioc_version <- bioc_version()

  if (!identical(default_bioc_version, bioc_ver)) {
    a[repos, "URL"] <- sub(as.character(default_bioc_version), bioc_ver, a[repos, "URL"], fixed = TRUE)
  }
  structure(a[repos, "URL"], names = repos)
}

#' Deduce the URLs of the BioConductor repositories
#'
#' @return A named character vector of the URLs of the
#' BioConductor repositories, appropriate for the current
#' R version.
#'
#' @param r_ver R version to use.
#' @param bioc_ver corresponding to the R version to use.
#' @export
#' @keywords internal

bioc_install_repos <- function(r_ver = getRversion(), bioc_ver = bioc_version()) {
  r_ver <- package_version(r_ver)
  bioc_ver <- package_version(bioc_ver)

  repos <- bioc_repos()

  ## add a conditional for Bioc releases occuring WITHIN
  ## a single R minor version. This is so that a user with a
  ## version of R (whose etc/repositories file references the
  ## no-longer-latest URL) and without BiocInstaller
  ## will be pointed to the most recent repository suitable
  ## for their version of R
  if (r_ver >= "3.2.2" && r_ver < "3.3.0") {
    ## transitioning to https support; check availability
    con <- file(fl <- tempfile(), "w")
    sink(con, type = "message")
    tryCatch(
      { xx <- close(file("https://bioconductor.org")) },
      error = function(e) { message(conditionMessage(e)) }
    )
    sink(type = "message")
    close(con)

    if (!length(readLines(fl))) {
      repos <- sub("^http:", "https:", repos)
    }
  }
  if (r_ver >= "3.5") {
    repos <- bioc_repos("3.8")

  } else if (r_ver >= "3.4") {
    repos <- bioc_repos("3.6")

  } else if (r_ver >= "3.3") {
    repos <- bioc_repos("3.4")

  } else if (r_ver >= "3.2") {
    repos <- bioc_repos("3.2")

  } else if (r_ver > "3.1.1") {
    repos <- bioc_repos("3.0")
  } else if (r_ver == "3.1.1") {
    ## R-3.1.1's etc/repositories file at the time of the release
    ## of Bioc 3.0 pointed to the 2.14 repository, but we want
    ## new installations to access the 3.0 repository
    repos <- bioc_repos("3.0")

  } else if (r_ver == "3.1.0") {
    ## R-devel points to 2.14 repository
    repos <- bioc_repos("2.14")
  } else {
    stop("Unsupported R version", call. = FALSE)
  }

  repos
}
