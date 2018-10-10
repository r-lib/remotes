context("test-install-dev")

test_that("install_dev works with GitHub URLs", {
  mockery::stub(install_dev, "install_github", identity)

  expect_equal(install_dev("dplyr"), "tidyverse/dplyr")

  expect_equal(install_dev("reprex"), "tidyverse/reprex")

  expect_equal(install_dev("mongolite"), "jeroen/mongolite")

  # only has a GH URL in BugReports
  expect_equal(install_dev("digest"), "eddelbuettel/digest")
})

test_that("install_dev works with uset CRAN mirrors", {
  mockery::stub(install_dev, "install_github", identity)

  expect_equal(install_dev("dplyr", cran_url = NULL), "tidyverse/dplyr")

  expect_equal(install_dev("dplyr", cran_url = "@CRAN@"), "tidyverse/dplyr")
})

test_that("install_dev fails if there is no URL field", {
  expect_error(install_dev("primerTree"), "Could not determine development repository")
})

test_that("install_dev fails if there is no URL field with a GitHub, GitLab or Bitbucket URL", {
  expect_error(install_dev("XML"), "Could not determine development repository")
})

test_that("install_dev works with GitLab URLs", {
  mockery::stub(install_dev, "install_gitlab", identity)

  expect_equal(install_dev("iemiscdata"), "iembry/iemiscdata")
})

test_that("install_dev works with Bitbucket URLs", {
  mockery::stub(install_dev, "install_bitbucket", identity)

  expect_equal(install_dev("argparser"), "djhshih/argparser")
})
