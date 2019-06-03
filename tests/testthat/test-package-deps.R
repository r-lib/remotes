
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
