
bioc_version <- function() {
  bver <- get(
    ".BioC_version_associated_with_R_version",
    envir = asNamespace("tools"),
    inherits = FALSE
  )

  if (is.function(bver)) bver() else bver
}

## This is mostly from https://bioconductor.org/biocLite.R

#' Deduce the URLs of the BioConductor repositories
#'
#' @return A named character vector of the URLs of the
#' BioConductor repositories, appropriate for the current
#' R version.
#'
#' @export

bioc_install_repos <- function() {

  vers <- getRversion()
  biocVers <- bioc_version()

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

  ## add a conditional for Bioc releases occuring WITHIN
  ## a single R minor version. This is so that a user with a
  ## version of R (whose etc/repositories file references the
  ## no-longer-latest URL) and without BiocInstaller
  ## will be pointed to the most recent repository suitable
  ## for their version of R
  if (vers >= "3.2.2" && vers < "3.3.0") {
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
      a[, "URL"] <- sub("^http:", "https:", a[, "URL"])
    }
  }
  if (vers >= "3.4") {
    a[, "URL"] <- sub(as.character(biocVers), "3.5", a[, "URL"])

  } else if (vers >= "3.3.0") {
    a[, "URL"] <- sub(as.character(biocVers), "3.4", a[, "URL"])

  } else if (vers >= "3.2") {
    a[, "URL"] <- sub(as.character(biocVers), "3.2", a[, "URL"])

  } else if (vers == "3.1.1") {
    ## R-3.1.1's etc/repositories file at the time of the release
    ## of Bioc 3.0 pointed to the 2.14 repository, but we want
    ## new installations to access the 3.0 repository
    a[, "URL"] <- sub(as.character(biocVers), "3.0", a[, "URL"])

  } else if (vers == "3.1.0") {
    ## R-devel points to 2.14 repository
    a[, "URL"] <- sub(as.character(biocVers), "2.14", a[, "URL"])

  } else if (vers >= "2.15" && vers < "2.16") {
    a[, "URL"] <- sub(as.character(biocVers), "2.11", a[, "URL"])
    biocVers <- numeric_version("2.11")
  }

  repos <- intersect(
    rownames(a),
    c("BioCsoft", "BioCann", "BioCexp", "BioCextra")
  )

  structure(a[repos, "URL"], names = repos)
}
