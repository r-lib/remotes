
context("Utilities")

test_that("%||%", {
  expect_equal(NULL %||% "foo", "foo")
  expect_equal("foo" %||% "bar", "foo")
  expect_equal(NULL %||% NULL, NULL)
})

test_that("trim_ws", {
  expect_equal(trim_ws("foobar"), "foobar")
  expect_equal(trim_ws(" foobar"), "foobar")
  expect_equal(trim_ws("    foobar"), "foobar")
  expect_equal(trim_ws("foobar "), "foobar")
  expect_equal(trim_ws("foobar    "), "foobar")
  expect_equal(trim_ws(" foobar "), "foobar")
  expect_equal(trim_ws("    foobar    "), "foobar")

  expect_equal(trim_ws(character()), character())
  
  expect_equal(trim_ws(c("1", "2")), c("1", "2"))
  expect_equal(trim_ws(c(" 1", "2")), c("1", "2"))
  expect_equal(trim_ws(c("1 ", "2")), c("1", "2"))
  expect_equal(trim_ws(c(" 1 ", " 2")), c("1", "2"))
  expect_equal(trim_ws(c("1", " 2 ")), c("1", "2"))
  expect_equal(trim_ws(c("1 ", "2 ")), c("1", "2"))
  expect_equal(trim_ws(c("1  ", "  2")), c("1", "2"))
})
