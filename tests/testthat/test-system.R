
context("System commands")

test_that("system_check", {

  expect_error(
    with_mock(
      `base::system2` = function(command, args, ...) {
        structure("output", status = 1)
      },
      system_check("foobar", args = c("arg1", "arg2", quiet = TRUE))
    ),
    "Command foobar failed "
  )

  expect_error(
    with_mock(
      `base::system2` = function(command, args, ...) 42,
      system_check("foobar", args = c("arg1", "arg2", quiet = FALSE))
    ),
    "Command foobar failed"
  )

})
