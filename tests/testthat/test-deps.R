
context("Deps")

test_that("standardise_dep", {

  expect_equal(
    standardise_dep(NA),
    c("Depends", "Imports", "LinkingTo")
  )

  expect_equal(
    standardise_dep(TRUE),
    c("Depends", "Imports", "LinkingTo", "Suggests")
  )

  expect_equal(
    standardise_dep(FALSE),
    character(0)
  )

  expect_equal(
    standardise_dep(c("Imports", "Suggests")),
    c("Imports", "Suggests")
  )

  expect_error(
    standardise_dep(1:10),
    "Dependencies must be a boolean or a character vector"
  )

})


test_that("compare_versions", {

  expect_equal(
    compare_versions(
      c("1.0.0", "1.0.0", "1.0.0"),
      c("1.0.1", "0.9.0", "1.0.0")
    ),
    c(-1L, 1L, 0L)
  )

  expect_equal(
    compare_versions(
      c(NA, "1.0.0"),
      c("1.0.0", NA)
    ),
    c(-2L, 2L)
  )

})


test_that("install_dev_remotes", {

  with_mock(
    `remotes::load_pkg_description` = function(...) {
      list(
        package = "foo",
        remotes = "github::hadley/testthat,klutometis/roxygen"
      )
    },
    `remotes::install_github` = function(...) {
      message("install_github called")
    },
    expect_message(
      install_dev_remotes("ize"),
      "install_github called"
    )
  )

})


test_that("printing package_deps", {

  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "magrittr"),
    installed = c("1.0", "1.0", "1.0"),
    available = c("1.0", NA, "1.0"),
    diff = c(0L, 2L, 0L)
  )
  class(object) <- c("package_deps", "data.frame")

  expect_output(
    print(object),
    "Not on CRAN.*\n.*package.*\n.*falsy.*"
  )

  expect_output(
    print(object, show_ok = TRUE),
    paste(
      sep = "\n",
      "Not on CRAN.*",
      ".*package.*",
      ".*falsy.*",
      "OK.*",
      ".*package.*",
      " dotenv.*",
      " magrittr"
    )
  )

  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "magrittr"),
    installed = c("1.0", "1.0", NA),
    available = c("1.0", "1.1", "1.0"),
    diff = c(0L, -1L, -2L)
  )
  class(object) <- c("package_deps", "data.frame")

  expect_output(
    print(object),
    paste(
      sep = "\n",
      "Needs update ------.*",
      " package .*",
      " falsy .*",
      " magrittr .*"
    )
  )
})
