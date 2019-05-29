
context("BioConductor packages")

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
