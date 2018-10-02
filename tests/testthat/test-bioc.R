
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

  # This particular version needs to do a connection test for https support
  skip_if_offline()
  skip_on_cran()
  expect_equal(
    bioc_install_repos("3.2.2"),
    bioc_repos("3.2")
  )
})

test_that("CRANextras exists in versions prior to Bioc 3.6", {
  # BioCextra was removed in R 3.5
  skip_if(getRversion() >= "3.5")

  expect_equal(
    names(bioc_repos("3.5")),
    c("BioCsoft", "BioCann", "BioCexp", "BioCextra")
  )

  expect_equal(
    names(bioc_repos("3.6")),
    c("BioCsoft", "BioCann", "BioCexp")
  )
})
