
context("System commands")

test_that("system_check", {

  mockery::stub(system_check, "system2", structure("output", status = 1))
  expect_error(
    system_check("foobar", args = c("arg1", "arg2", quiet = TRUE)),
    "Command foobar failed"
  )

  mockery::stub(system_check, "system2", 42)
  expect_error(
    system_check("foobar", args = c("arg1", "arg2", quiet = FALSE)),
    "Command foobar failed"
  )
})
