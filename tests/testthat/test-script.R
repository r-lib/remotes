
context("install-github.R script")

test_that("install-github.R script is up to date", {
  root <- system.file(package = .packageName)
  tmp <- test_temp_file(".R")

  withr::with_dir(
    rprojroot::find_package_root_file(),
    brew::brew(file.path(root, "install-github.Rin"), tmp))

  expect_equal(
    tools::md5sum(tmp)[[1]],
    tools::md5sum(file.path(root, "install-github.R"))[[1]])
})

test_that("use install-github.R script", {
  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  script <- system.file(package = .packageName, "install-github.R")
  lib <- test_temp_dir()
  expect_error(
    source(script)$value("cran/falsy", lib = lib, quiet = TRUE),
    NA)
  expect_equal(
    packageDescription("falsy", lib.loc = lib)$RemoteRepo,
    "falsy")
})

test_that("install-github.R script does not load any package", {
  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  script <- system.file(package = .packageName, "install-github.R")
  lib <- test_temp_dir()

  pkgs <- callr::r(
    function(script, lib) {
      ## tools is ok to load
      library(tools)
      orig <- loadedNamespaces()
      source(script)$value("cran/falsy", lib = lib)
      new <- loadedNamespaces()
      list(orig, new)
    },
    args = list(script = script, lib = lib),
    cmdargs = c("--vanilla", "--slave", "--no-save", "--no-restore"),
    timeout = 20
  )

  expect_equal(pkgs[[1]], pkgs[[2]])
})
