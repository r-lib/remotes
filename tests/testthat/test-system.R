test_that("system_check", {

  local_mocked_bindings(
    system2 = function(...) structure("output", status = 1),
    .package = "base"
  )

  expect_error(
    system_check("foobar", args = c("arg1", "arg2", quiet = TRUE)),
    "Command foobar failed"
  )

  local_mocked_bindings(
    system2 = function(...) 42,
    .package = "base"
  )

  expect_error(
    system_check("foobar", args = c("arg1", "arg2", quiet = FALSE)),
    "Command foobar failed"
  )
})
