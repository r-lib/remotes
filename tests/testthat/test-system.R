
context("System commands")

test_that("system_check", {

  expect_message(
    with_mock(
      `base::system` = function(command, intern = FALSE, ...) {
        if (intern) {
          c("command output\n")
        } else {
          0L
        }
      },
      system_check("foobar", args = c("arg1", "arg2"), quiet = FALSE)
    ),
    "'foobar' arg1 arg2"
  )

  expect_silent(
    with_mock(
      `base::system` = function(command, intern = FALSE, ...) {
        if (intern) {
          c("command output\n")
        } else {
          0L
        }
      },
      system_check("foobar", args = c("arg1", "arg2"), quiet = TRUE)
    )
  )

})
