
context("BioConductor packages")

test_that("bioc is standalone", {
  ## baseenv() makes sure that the remotes package env is not used
  env <- new.env(parent = baseenv())
  env$bioc <- bioconductor
  stenv <- env$bioc$.internal
  objs <- ls(stenv, all.names = TRUE)
  funs <- Filter(function(x) is.function(stenv[[x]]), objs)
  funobjs <- mget(funs, stenv)

  expect_message(
    mapply(codetools::checkUsage, funobjs, funs,
           MoreArgs = list(report = message)),
    NA)
})

test_that("we can parse the YAML config", {
  skip_if_offline()
  expect_silent(yaml <- bioconductor$get_yaml_config(forget = TRUE))
  expect_true(length(yaml) > 20)
  map <- bioconductor$get_version_map()
  expect_true("release" %in% map$bioc_status)
  expect_true("devel" %in% map$bioc_status)
  expect_true("future" %in% map$bioc_status)

  expect_true(inherits(bioconductor$get_release_version(),
                       "package_version"))
  expect_true(inherits(bioconductor$get_devel_version(),
                       "package_version"))
})

test_that("internal map is current", {
  # If there is a new bioc version out, then we'll error.
  # This is to notify us that we need to update the package's
  # internal map.
  skip_on_cran()
  skip_if_offline()
  expect_equal(
    bioconductor$get_release_version(),
    package_version("3.10"))
})

test_that("set of repos are correct", {
  # Compare our set of repos to the set returned by BiocManager.
  # They should match. If they don't we need to update the package.
  skip_if_offline()
  skip_on_cran()
  skip_without_package("BiocManager")
  withr::local_envvar(list(BIOC_VERSION = NA, BIOC_MIRROR = NA))
  withr::local_options(list(Bioc_mirror = NULL))

  # We can only run this if the matching R version is new enough,
  # so we skip on other platforms
  dev_ver <- bioconductor$get_devel_version()
  map <- bioconductor$get_version_map()
  required <- map$r_version[map$bioc_version == dev_ver]
  if (!getRversion()[, 1:2] %in% required) skip("Needs newer R version")

  my_repos <- bioconductor$get_repos(dev_ver)
  bm_repos <- asNamespace("BiocManager")$repositories(version = dev_ver)
  bm_repos <- bm_repos[names(bm_repos) != "CRAN"]
  expect_equal(sort(names(my_repos)), sort(names(bm_repos)))
  expect_equal(my_repos, bm_repos[names(my_repos)])
})

test_that("bioc_install_repos", {

  expect_equal(
    bioc_install_repos("3.1.0"),
    bioc_install_repos(bioc_ver = "3.0")
  )

  expect_equal(
    bioc_install_repos("3.1.1"),
    bioc_install_repos(bioc_ver = "3.0")
  )

  expect_equal(
    bioc_install_repos("3.2"),
    bioc_install_repos(bioc_ver = "3.2")
  )

  expect_equal(
    bioc_install_repos("3.3"),
    bioc_install_repos(bioc_ver = "3.4")
  )

  expect_equal(
    bioc_install_repos("3.4"),
    bioc_install_repos(bioc_ver = "3.6")
  )

  expect_equal(
    bioc_install_repos("3.5"),
    bioc_install_repos(bioc_ver = "3.8")
  )

  # This particular version needs to do a connection test for https support
  skip_on_cran()
  skip_if_offline()
  expect_equal(
    bioc_install_repos("3.2.2"),
    bioc_install_repos(bioc_ver = "3.2")
  )
})

test_that("CRANextras exists in versions prior to Bioc 3.6", {
  expect_equal(
    names(bioc_install_repos(bioc_ver = "3.5")),
    c("BioCsoft", "BioCann", "BioCexp", "BioCextra")
  )

  expect_equal(
    names(bioc_install_repos(bioc_ver = "3.6")),
    c("BioCsoft", "BioCann", "BioCexp")
  )
})
