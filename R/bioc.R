## This is mostly from https://github.com/Bioconductor/BiocManager/blob/ba18f67fb886048b991c0f04a87894cf8c35b076/R/version.R

bioc_version <- function(r_ver = getRversion()) {
  r_ver <- package_version(r_ver)
  map <- bioc_version_map(r_ver)

   if (r_ver[, 1:2] < min(map$R)){
      stop("Unsupported R version for Bioconductor", call. = FALSE)
    } else {
    map <- map[map$R == r_ver[, 1:2],]
    if ("release" %in% map$BiocStatus)
      idx <- map$BiocStatus == "release"
    else if ("devel" %in% map$BiocStatus)
      idx <- map$BiocStatus == "devel"
    else if ("out-of-date" %in% map$BiocStatus)
      idx <- map$BiocStatus == "out-of-date"
    else
      idx <- map$BiocStatus == "future"

    return(tail(map$Bioc[idx], 1))
  }

}

bioc_version_map <- function(r_ver = getRversion()){
  # dput(BiocManager:::.version_map())
  map <- structure(list(Bioc = structure(list(c(1L, 6L), c(1L, 7L), c(1L,
                                                                      8L), c(1L, 9L), c(2L, 0L), c(2L, 1L), c(2L, 2L), 2:3, c(2L, 4L
                                                                      ), c(2L, 5L), c(2L, 6L), c(2L, 7L), c(2L, 8L), c(2L, 9L), c(2L,
                                                                                                                                  10L), c(2L, 11L), c(2L, 12L), c(2L, 13L), c(2L, 14L), c(3L, 0L
                                                                                                                                  ), c(3L, 1L), c(3L, 2L), c(3L, 3L), 3:4, c(3L, 5L), c(3L, 6L),
                                              c(3L, 7L), c(3L, 8L), c(3L, 9L), c(3L, 9L)), class = c("package_version",
                                                                                                     "numeric_version")), R = structure(list(c(2L, 1L), c(2L, 2L),
                                                                                                                                             2:3, c(2L, 4L), c(2L, 5L), c(2L, 6L), c(2L, 7L), c(2L, 8L
                                                                                                                                             ), c(2L, 9L), c(2L, 10L), c(2L, 11L), c(2L, 12L), c(2L, 13L
                                                                                                                                             ), c(2L, 14L), c(2L, 15L), c(2L, 15L), c(3L, 0L), c(3L, 0L
                                                                                                                                             ), c(3L, 1L), c(3L, 1L), c(3L, 2L), c(3L, 2L), c(3L, 3L),
                                                                                                                                             c(3L, 3L), 3:4, 3:4, c(3L, 5L), c(3L, 5L), c(3L, 6L), c(3L,
                                                                                                                                                                                                     7L)), class = c("package_version", "numeric_version")), BiocStatus = structure(c(1L,
                                                                                                                                                                                                                                                                                      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                                                                                                                                                                                                                                                                      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 3L, 4L), .Label = c("out-of-date",
                                                                                                                                                                                                                                                                                                                                                      "release", "devel", "future"), class = "factor")), .Names = c("Bioc",
                                                                                                                                                                                                                                                                                                                                                                                                                    "R", "BiocStatus"), row.names = c(NA, -30L), class = "data.frame")


     # if config got out-of-date
  if (r_ver[, 1:2] > max(map$R)){
    map <- get_online_bioc_version_map()
  }

  return(map)
}

get_online_bioc_version_map <- function(){
  config <- "https://bioconductor.org/config.yaml"
  txt <- tryCatch(readLines(config), error = identity)
  if (inherits(txt, "error") && startsWith(config, "https://")) {
    config <- sub("https", "http", config)
    txt <- tryCatch(readLines(config), error = identity)
  }
  if (inherits(txt, "error"))
    stop("Unable to read Bioconductor config", call. = FALSE)

  grps <- grep("^[^[:blank:]]", txt)
  start <- match(grep("r_ver_for_bioc_ver", txt), grps)
  map <- txt[seq(grps[start] + 1, grps[start + 1] - 1)]
  map <- trimws(gsub("\"", "", sub(" #.*", "", map)))

  pattern <- "(.*): (.*)"
  bioc <- package_version(sub(pattern, "\\1", map))
  r <- package_version(sub(pattern, "\\2", map))

  pattern <- "^release_version: \"(.*)\""
  release <- package_version(
    sub(pattern, "\\1", grep(pattern, txt, value=TRUE))
  )
  pattern <- "^devel_version: \"(.*)\""
  devel <- package_version(
    sub(pattern, "\\1", grep(pattern, txt, value=TRUE))
  )
  status <- rep("out-of-date", length(bioc))
  status[bioc == release] <- "release"
  status[bioc == devel] <- "devel"

  ## append final version for 'devel' R
  bioc <- c(
    bioc, max(bioc)
    ## package_version(paste(unlist(max(bioc)) + 0:1, collapse = "."))
  )
  r <- c(r, package_version(paste(unlist(max(r)) + 0:1, collapse = ".")))
  status <- c(status, "future")

  data.frame(
    Bioc = bioc, R = r,
    BiocStatus = factor(
      status,
      levels = c("out-of-date", "release", "devel", "future")
    )
  )
}

bioc_repos <- function(bioc_ver, r_ver) {

  r_ver <- package_version(r_ver)
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

  default_bioc_version <- bioc_version(r_ver)

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

bioc_install_repos <- function(r_ver = getRversion(),
                               bioc_ver = Sys.getenv("BIOCONDUCTOR_VERSION",
                                                     bioc_version(r_ver))) {

  r_ver <- package_version(r_ver)
  bioc_ver <- package_version(bioc_ver)
  # check version compatibility
  map <- bioc_version_map(r_ver)

  if (!bioc_ver %in% map$Bioc[map$R == r_ver[, 1:2],]){
    stop("Incompatible R and Bioconductor version, check BIOCONDUCTOR_VERSION",
         call. = FALSE)
  }

  # get repos
  repos <- bioc_repos(bioc_ver = bioc_ver, r_ver = r_ver)

  repos
}
