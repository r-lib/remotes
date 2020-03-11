context("install_bioc.R")

test_that("bioc repo paths are parsed correctly", {
  expect_equal(parse_bioc_repo("devtools"), list(repo="devtools"))
  expect_equal(parse_bioc_repo("devtools#abc123"), list(repo="devtools", commit="abc123"))
  expect_equal(parse_bioc_repo("user:pass@devtools"), list(username = "user", password = "pass", repo="devtools"))
  expect_equal(parse_bioc_repo("devel/devtools"), list(release = "devel", repo="devtools"))
  expect_equal(parse_bioc_repo("3.1/devtools"), list(release = "3.1", repo="devtools"))
  expect_equal(parse_bioc_repo("release/devtools"), list(release = "release", repo="devtools"))
  expect_equal(parse_bioc_repo("user:pass@devtools#abc123"), list(username = "user", password = "pass", repo="devtools", commit = "abc123"))
  expect_error(parse_bioc_repo("user:pass@3.1/devtools#abc123"), "release and commit should not both be specified")
  expect_error(parse_bioc_repo("user@devtools"), "Invalid bioc repo")
  expect_error(parse_bioc_repo("user:@devtools"), "Invalid bioc repo")
  expect_error(parse_bioc_repo("@devtools"), "Invalid bioc repo")
  expect_error(parse_bioc_repo("devtools/"), "Invalid bioc repo")
  expect_error(parse_bioc_repo("junk/devtools"), "Invalid bioc repo")
})

test_that("install_bioc with git2r", {

  skip_without_package("git2r")
  skip_on_cran()
  skip_if_offline()

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  mirror <- getOption("BioC_git", "https://git.bioconductor.org/packages")

  # This package has no dependencies or compiled code and is old
  install_bioc("MeasurementError.cor", mirror = mirror, git = "git2r", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("MeasurementError.cor", lib.loc = lib))
  expect_equal(packageDescription("MeasurementError.cor", lib.loc = lib)$RemoteType, "bioc_git2r")

  remote <- package2remote("MeasurementError.cor", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "bioc_git2r_remote")
  expect_equal(format(remote), "Bioc")
  expect_equal(remote$mirror, mirror)
  expect_equal(remote$repo, "MeasurementError.cor")
  expect_equal(remote$release, "release")
  expect_true(!is.na(remote$sha) && nzchar(remote$sha))
  expect_true(!is.na(remote$branch) && nzchar(remote$branch))
})

test_that("install_bioc with xgit", {

  skip_without_program("git")
  skip_on_cran()
  skip_if_offline()

  lib <- tempfile()
  on.exit(unlink(lib, recursive = TRUE), add = TRUE)
  dir.create(lib)

  mirror <- getOption("BioC_git", "https://git.bioconductor.org/packages")


  # This package has no dependencies or compiled code and is old
  install_bioc("MeasurementError.cor", mirror = mirror, git = "external", lib = lib, quiet = TRUE)

  expect_silent(packageDescription("MeasurementError.cor", lib.loc = lib))
  expect_equal(packageDescription("MeasurementError.cor", lib.loc = lib)$RemoteType, "bioc_xgit")

  remote <- package2remote("MeasurementError.cor", lib = lib)
  expect_s3_class(remote, "remote")
  expect_s3_class(remote, "bioc_xgit_remote")
  expect_equal(format(remote), "Bioc")
  expect_equal(remote$mirror, mirror)
  expect_equal(remote$repo, "MeasurementError.cor")
  expect_equal(remote$release, "release")
  expect_true(!is.na(remote$sha) && nzchar(remote$sha))
  expect_true(!is.na(remote$branch) && nzchar(remote$branch))
})
