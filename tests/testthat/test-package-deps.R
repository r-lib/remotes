
context("Package dependencies")

test_that("parse_deps", {

  expect_null(parse_deps(NULL))
  expect_null(parse_deps(""))
  expect_null(parse_deps("  "))
  expect_null(parse_deps("\n"))

  expect_equal(
    parse_deps("devtools (>= 1.0.1)"),
    structure(
      list(
        name = "devtools",
        compare = ">=",
        version = "1.0.1"),
      row.names = 1L,
      class = "data.frame"
    )
  )

  expect_equal(
    parse_deps("devtools (>= 1.0.1), foobar, foobar2 (== 0.0.1)"),
    structure(
      list(
        name = c("devtools", "foobar", "foobar2"),
        compare = c(">=", NA, "=="),
        version = c("1.0.1", NA, "0.0.1")),
      row.names = 1:3,
      class = "data.frame"
    )
  )

  # Whitespace should be ignored
  expect_equal(
    parse_deps("devtools (>= 1.0.1) \n, foobar, foobar2 ( == 0.0.1 )"),
    structure(
      list(
        name = c("devtools", "foobar", "foobar2"),
        compare = c(">=", NA, "=="),
        version = c("1.0.1", NA, "0.0.1")),
      row.names = 1:3,
      class = "data.frame"
    )
  )

  expect_equal(
    parse_deps("package (>= 1.0.1)  , package2 (< 0.0.1 )  "),
    structure(
      list(
        name = c("package", "package2"),
        compare = c(">=", "<"),
        version = c("1.0.1", "0.0.1")),
      row.names = 1:2,
      class = "data.frame"
    )
  )
})

test_that("version requirement comparisons", {
  # Two different formats for requirement specs
  required1 <- c("< 2.1", "> 1.5")
  required2 <- "< 2.1, > 1.5"

  for (required in list(required1, required2)) {
    expect_true(version_satisfies_criteria('2.0', required), label = required)
    expect_true(version_satisfies_criteria(NULL, required))
    expect_false(version_satisfies_criteria('2.1', required))
    expect_false(version_satisfies_criteria('1.5', required))

    mockery::stub(package_installed, "packageDescription", function(...) "2.0")
    expect_true(package_installed('foo', required))

    mockery::stub(package_installed, "packageDescription", function(...) "1.0")
    expect_false(package_installed('foo', required))
  }

  expect_equal(
    version_criteria('1.5'),
    data.frame(compare='==', version='1.5', stringsAsFactors=FALSE)
  )

  expect_equivalent(
    version_criteria(NULL),
    data.frame(compare=NA_character_, version=NA_character_, stringsAsFactors=FALSE)
  )

  expect_equivalent(
    version_criteria(NA),
    data.frame(compare=NA_character_, version=NA_character_, stringsAsFactors=FALSE)
  )

  expect_equal(
    version_criteria(c('> 1.5, < 2.0')),
    data.frame(compare=c('>', '<'), version=c('1.5', '2.0'), stringsAsFactors=FALSE)
  )

  expect_equal(
    version_criteria(c('> 1.5', '< 2.0')),
    data.frame(compare=c('>', '<'), version=c('1.5', '2.0'), stringsAsFactors=FALSE)
  )

  expect_equal(
    version_from_tarball("ROI.plugin.glpk/ROI.plugin.glpk_0.0-1.tar.gz"),
    "0.0-1"
  )

  expect_equal(
    version_from_tarball("ROI.plugin.glpk_0.0-1.tar.gz"),
    "0.0-1"
  )

  expect_equal(
    version_from_tarball(c("ROI.plugin.glpk_0.0-1.tar.gz", "ROI.plugin.glpk_2.3-1.tar.gz")),
    c("0.0-1", "2.3-1")
  )
})



test_that("parse_deps errors", {

  expect_error(parse_deps(42), "is.character.*is not TRUE")

  expect_error(
    parse_deps("remotes (++ 1.0.0)"),
    "Invalid comparison operator in dependency"
  )

  expect_error(
    parse_deps("remotes (>=1.0.0)"),
    "Invalid comparison operator in dependency"
  )

})


test_that("parse_deps omits R", {

  expect_equal(
    parse_deps("R (>= 2.15.3)"),
    structure(
      list(
        name = character(0),
        compare = character(0),
        version = character(0)),
      row.names = integer(0),
      class = "data.frame"
    )
  )

  expect_equal(
    parse_deps("R (>= 2.15.3), devtools (>= 1.0.1)"),
    structure(
      list(
        name = "devtools",
        compare = ">=",
        version = "1.0.1"),
      row.names = 2L,
      class = "data.frame"
    )
  )

  expect_equal(
    parse_deps("devtools (>= 1.0.1), R (>= 2.15.3)"),
    structure(
      list(
        name = "devtools",
        compare = ">=",
        version = "1.0.1"),
      row.names = 1L,
      class = "data.frame"
    )
  )

})
