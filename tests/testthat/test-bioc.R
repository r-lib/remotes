
context("BioConductor packages")

test_that("bioc_install_repos", {
  current_bioc_version <- bioc_version()

  expect_equal(
    bioc_install_repos("3.1.0"),
    bioc_repos("2.14")
  )

  expect_equal(
    bioc_install_repos("3.1.1"),
    bioc_repos("3.0")
  )

  expect_equal(
    bioc_install_repos("3.2"),
    bioc_repos("3.2")
  )

  expect_equal(
    bioc_install_repos("3.3"),
    bioc_repos("3.4")
  )

  expect_equal(
    bioc_install_repos("3.4"),
    bioc_repos("3.6")
  )

  expect_equal(
    bioc_install_repos("3.5"),
    bioc_repos("3.7")
  )

  expect_error(
    bioc_install_repos("2.10"),
    "Unsupported"
  )

  skip_if_offline()
  skip_on_cran()
  expect_equal(
    bioc_install_repos("3.2.2"),
    bioc_repos("3.2")
  )
})

test_that("installing bioc packages", {

  skip_on_cran()
  skip_if_offline()

  Sys.unsetenv("R_TESTS")

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  bioc_branch <- paste0("RELEASE_", gsub("[.]", "_", bioc_version()))

  withr::with_libpaths(
    lib,
    install_git("https://git.bioconductor.org/packages/Biobase",
      branch = bioc_branch,
      lib = lib,
      quiet = TRUE)
  )

  expect_silent(packageDescription("Biobase", lib.loc = lib))
  expect_equal(
    packageDescription("Biobase", lib.loc = lib)$RemoteUrl,
    "https://git.bioconductor.org/packages/Biobase")

  expect_silent(packageDescription("BiocGenerics", lib.loc = lib))
})


