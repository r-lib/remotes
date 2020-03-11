
context("install-github.R script")

test_that("install-github.R script is up to date", {
  skip_on_cran()

  root <- system.file(package = packageName())
  tmp <- test_temp_file(".R")

  withr::with_dir(
    rprojroot::find_package_root_file(),
    brew::brew(file.path(root, "install-github.Rin"), tmp))

  expect_equal(
    readLines(tmp),
    readLines(file.path(root, "install-github.R")))
})

test_that("use install-github.R script", {
  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  script <- system.file(package = packageName(), "install-github.R")
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

  script <- system.file(package = packageName(), "install-github.R")
  lib <- test_temp_dir()

  pkgs <- callr::r(
    function(script, lib) {
      ## compiler and tools are ok to load
      library(compiler)
      library(tools)
      orig <- loadedNamespaces()

      source(script)$value("cran/falsy", lib = lib)

      new <- loadedNamespaces()

      ## Need to load curl on R < 3.2.0, for HTTPS, so we ignore this
      if (getRversion() < "3.2.0") new <- setdiff(new, "curl")
      list(orig, new)
    },
    args = list(script = script, lib = lib),
    cmdargs = c("--vanilla", "--slave", "--no-save", "--no-restore"),
    timeout = 20
  )

  expect_equal(pkgs[[1]], pkgs[[2]])
})
