context("test-install-dev")

test_that("install_dev works with GitHub URLs", {
  skip_on_cran()
  skip_if_offline()

  mockery::stub(install_dev, "install_github", identity)

  expect_equal(install_dev("dplyr"), "tidyverse/dplyr")

  expect_equal(install_dev("reprex"), "tidyverse/reprex")

  expect_equal(install_dev("mongolite"), "jeroen/mongolite")

  # only has a GH URL in BugReports
  expect_equal(install_dev("digest"), "eddelbuettel/digest")
})

test_that("install_dev works with uset CRAN mirrors", {
  skip_on_cran()
  skip_if_offline()

  mockery::stub(install_dev, "install_github", identity)

  expect_equal(install_dev("dplyr", cran_url = NULL), "tidyverse/dplyr")

  expect_equal(install_dev("dplyr", cran_url = "@CRAN@"), "tidyverse/dplyr")
})

test_that("install_dev fails if there is no URL field", {
  skip_on_cran()
  skip_if_offline()

  expect_error(install_dev("primerTree"), "Could not determine development repository")
})

test_that("install_dev fails if there is no URL field with a GitHub, GitLab or Bitbucket URL", {
  skip_on_cran()
  skip_if_offline()

  expect_error(install_dev("XML"), "Could not determine development repository")
})

test_that("install_dev works with GitLab URLs", {
  skip_on_cran()
  skip_if_offline()

  mockery::stub(install_dev, "install_gitlab", identity)

  expect_equal(install_dev("iemiscdata"), "iembry/iemiscdata")
})

test_that("install_dev works with Bitbucket URLs", {
  skip_on_cran()
  skip_if_offline()

  mockery::stub(install_dev, "install_bitbucket", identity)

  expect_equal(install_dev("argparser"), "djhshih/argparser")
})
