
context("Install a specific version from CRAN")

test_that("install_version", {
  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  repos <- getOption("repos")
  if (length(repos) == 0) repos <- character()
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com"

  install_version("pkgconfig", "1.0.0", lib = lib, repos = repos, quiet = TRUE)

  expect_silent(packageDescription("pkgconfig", lib.loc = lib))
  desc <- packageDescription("pkgconfig", lib.loc = lib)
  expect_equal(desc$Version, "1.0.0")
  expect_null(desc$RemoteType)
  expect_null(desc$RemoteSubdir)
  expect_null(desc$RemoteUrl)
})

test_that("package_find_archives() works correctly", {
  skip_on_cran()
  skip_if_offline()

  repos <- c(CRANextras = "http://www.stats.ox.ac.uk/pub/RWin", CRAN = "http://cran.rstudio.com")
  # ROI.plugin.glpk is the smallest package in the CRAN archive
  package <- "ROI.plugin.glpk"

  res <- package_find_archives(package, repos[1])
  expect_null(res)

  res <- package_find_archives(package, repos[2])
  expect_true(NROW(res) >= 1)
  expect_equal(res$repo[1], "http://cran.rstudio.com")
  expect_match(rownames(res), package)
})

test_that("install_version for current version", {
  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  repos <- getOption("repos")
  if (length(repos) == 0) repos <- character()
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com"

  install_version("pkgconfig", NULL, lib = lib, repos = repos, type = "source", quiet = TRUE)

  expect_silent(packageDescription("pkgconfig", lib.loc = lib))
})


test_that("install_version and invalid version number", {
  skip_on_cran()
  skip_if_offline()

  repos <- getOption("repos")
  if (length(repos) == 0) repos <- character()
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com"

  expect_error(
    install_version("pkgconfig", "109.42", repos = repos),
    "version '109.42' is invalid for package 'pkgconfig'"
  )

  expect_error(
    download_version("pkgconfig", "109.42", repos = repos),
    "version '109.42' is invalid for package 'pkgconfig'"
  )
})


test_that("install_version and non-existing package", {
  skip_on_cran()
  skip_if_offline()

  repos <- getOption("repos")
  if (length(repos) == 0) repos <- character()
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com"

  expect_error(
    install_version("42xxx", "1.0.0", repos = repos),
    "couldn't find package '42xxx'"
  )
})


test_that("install_version for archived packages", {
  skip_on_cran()
  skip_if_offline()

  repos <- getOption("repos")
  if (length(repos) == 0) repos <- character()
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com"

  lib <- tempfile()

  mockery::stub(install_version, "install_url", function(url, ...) url)
  mockery::stub(install_version, "add_metadata", NULL)

  expect_match(
    fixed = TRUE,
    install_version("igraph0", type = "source", lib = lib, repos = repos),
    "src/contrib/Archive/igraph0/igraph0_0.5.7.tar.gz"
  )

  mockery::stub(download_version, "download", function(url, ...) url)
  expect_match(
    fixed = TRUE,
    download_version("igraph0", type = "source", lib = lib, repos = repos),
    "src/contrib/Archive/igraph0/igraph0_0.5.7.tar.gz"
  )
})


test_that("download_version_url for multiple repositories", {
  # Despite its name, download_version_url() doesn't download anything, so its test probably fits
  # better here than in test-download.R.

  # download_version_url() is the workhorse function for install_version().

  repos <- c(
    "Prod" = "http://example.com/repo-prod",
    "Dev" = "http://example.com/repo-dev",
    "CRAN" = "http://cran.rstudio.example.com"
  )

  available <-
    "
  Package Version   Repository
  Foo     1.0       http://example.com/repo-prod/src/contrib
  Bar     2.0       http://example.com/repo-prod/src/contrib
  Foo     1.0-287   http://example.com/repo-dev/src/contrib
  dplyr   0.8.3     http://cran.rstudio.example.com/src/contrib
  "
  available <- as.matrix(read.table(textConnection(available), header = TRUE))
  rownames(available) <- available[, "Package"]

  mockery::stub(download_version_url, "package_find_archives", function(package, repo, verbose = FALSE) {
    pathfunc <- function(package, version) {
      sprintf("%s/%s_%s.tar.gz", package, package, version)
    }

    arch <-
      if (repo == repos["Prod"]) {
        list("Foo" = data.frame(size = 1:2, row.names = pathfunc("Foo", c("0.8", "0.9"))))
      } else if (repo == repos["Dev"]) {
        list("Foo" = data.frame(size = 1:2, row.names = pathfunc("Foo", c("0.8-123", "0.9-456"))))
      } else {
        list()
      }

    arch[[package]]
  })

  # Latest released version
  expect_equal(
    download_version_url("Foo", "1.0", repos, available = available),
    "http://example.com/repo-prod/src/contrib/Foo_1.0.tar.gz"
  )

  # Latest snapshot
  expect_equal(
    download_version_url("Foo", "1.0.287", repos, available = available),
    "http://example.com/repo-dev/src/contrib/Foo_1.0-287.tar.gz"
  )

  # Find snapshot satisfying
  expect_equal(
    download_version_url("Foo", "> 1.0", repos, available = available),
    "http://example.com/repo-dev/src/contrib/Foo_1.0-287.tar.gz"
  )

  # Error when no suitable version found
  expect_error(download_version_url("Foo", "> 2.0", repos, available = available, verbose = FALSE))

  # Find version in release archives
  expect_equal(
    download_version_url("Foo", "< 1.0", repos, available = available),
    "http://example.com/repo-prod/src/contrib/Archive/Foo/Foo_0.9.tar.gz"
  )

  # Find version in snapshot archives
  expect_equal(
    download_version_url("Foo", "> 0.9, < 1.0", repos, available = available),
    "http://example.com/repo-dev/src/contrib/Archive/Foo/Foo_0.9-456.tar.gz"
  )
})


test_that("install_version for other types fails", {
  expect_error(
    install_version("igraph0", type = "binary"),
    "must be 'source'"
  )

  expect_error(
    install_version("igraph0", type = "win.binary"),
    "must be 'source'"
  )

  expect_error(
    install_version("igraph0", type = "mac.binary"),
    "must be 'source'"
  )
})

test_that("version requirement comparisons", {
  # Two different formats for requirement specs
  required1 <- c("< 2.1", "> 1.5")
  required2 <- "< 2.1, > 1.5"

  for (required in list(required1, required2)) {
    expect_true(version_satisfies_criteria("2.0", required), label = required)
    expect_true(version_satisfies_criteria(NULL, required))
    expect_false(version_satisfies_criteria("2.1", required))
    expect_false(version_satisfies_criteria("1.5", required))

    mockery::stub(package_installed, "packageDescription", function(...) "2.0")
    expect_true(package_installed("foo", required))

    mockery::stub(package_installed, "packageDescription", function(...) "1.0")
    expect_false(package_installed("foo", required))
  }

  expect_equal(
    version_criteria("1.5"),
    data.frame(compare = "==", version = "1.5", stringsAsFactors = FALSE)
  )

  expect_equivalent(
    version_criteria(NULL),
    data.frame(compare = NA_character_, version = NA_character_, stringsAsFactors = FALSE)
  )

  expect_equivalent(
    version_criteria(NA),
    data.frame(compare = NA_character_, version = NA_character_, stringsAsFactors = FALSE)
  )

  expect_equal(
    version_criteria(c("> 1.5, < 2.0")),
    data.frame(compare = c(">", "<"), version = c("1.5", "2.0"), stringsAsFactors = FALSE)
  )

  expect_equal(
    version_criteria(c("> 1.5", "< 2.0")),
    data.frame(compare = c(">", "<"), version = c("1.5", "2.0"), stringsAsFactors = FALSE)
  )

  expect_equal(
    version_from_tarball("ROI.plugin.glpk/ROI.plugin.glpk_0.0-1.tar.gz"),
    "0.0-1"
  )

  expect_equal(
    version_from_tarball("ROI.plugin.glpk_0.0-1.tar.gz"),
    "0.0-1"
  )

  expect_equal(
    version_from_tarball(c("ROI.plugin.glpk_0.0-1.tar.gz", "ROI.plugin.glpk_2.3-1.tar.gz")),
    c("0.0-1", "2.3-1")
  )
})



test_that("parse_deps errors", {
  expect_error(parse_deps(42), "is.character.*is not TRUE")

  expect_error(
    parse_deps("remotes (++ 1.0.0)"),
    "Invalid comparison operator in dependency"
  )

  expect_error(
    parse_deps("remotes (>=1.0.0)"),
    "Invalid comparison operator in dependency"
  )
})


test_that("parse_deps omits R", {
  expect_equal(
    parse_deps("R (>= 2.15.3)"),
    structure(
      list(
        name = character(0),
        compare = character(0),
        version = character(0)
      ),
      row.names = integer(0),
      class = "data.frame"
    )
  )

  expect_equal(
    parse_deps("R (>= 2.15.3), devtools (>= 1.0.1)"),
    structure(
      list(
        name = "devtools",
        compare = ">=",
        version = "1.0.1"
      ),
      row.names = 2L,
      class = "data.frame"
    )
  )

  expect_equal(
    parse_deps("devtools (>= 1.0.1), R (>= 2.15.3)"),
    structure(
      list(
        name = "devtools",
        compare = ">=",
        version = "1.0.1"
      ),
      row.names = 1L,
      class = "data.frame"
    )
  )
})
