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
        version = "1.0.1"
      ),
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
        version = c("1.0.1", NA, "0.0.1")
      ),
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
        version = c("1.0.1", NA, "0.0.1")
      ),
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
        version = c("1.0.1", "0.0.1")
      ),
      row.names = 1:2,
      class = "data.frame"
    )
  )
})
