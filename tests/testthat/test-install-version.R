
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

test_that("package_find_repo() works correctly with multiple repos", {

  skip_on_cran()
  skip_if_offline()

  repos <- c(CRANextras = "http://www.stats.ox.ac.uk/pub/RWin", CRAN = "http://cran.rstudio.com")
  # ROI.plugin.glpk is the smallest package in the CRAN archive
  package <- "ROI.plugin.glpk"
  res <- package_find_repo(package, repos = repos)

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


test_that("intall_version and invalid version number", {

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

  expect_match(fixed = TRUE,
    install_version("igraph0", type = "source", lib = lib, repos = repos),
    "src/contrib/Archive/igraph0/igraph0_0.5.7.tar.gz"
  )

  mockery::stub(download_version, "download", function(url, ...) url)
  expect_match(fixed = TRUE,
    download_version("igraph0", type = "source", lib = lib, repos = repos),
    "src/contrib/Archive/igraph0/igraph0_0.5.7.tar.gz"
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
