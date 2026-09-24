test_that("has_devel", {
  # All platforms when tests are run should have a compiler
  expect_true(has_devel())

  # has_devel should return FALSE if an error occurs from has_devel2
  local_mocked_bindings(
    has_devel2 = function(...) stop("failed"),
    .package = "remotes"
  )
  expect_false(has_devel())
})

test_that("has_devel2", {
  # has_devel2 should error if an error occurs from R CMD SHLIB
  local_mocked_bindings(
    R = function(...) stop("failed"),
    .package = "remotes"
  )
  expect_error(has_devel2())
})

test_that("missing_devel_warning", {
  local_mocked_bindings(
    has_devel2 = function(...) FALSE,
    .package = "remotes"
  )

  expect_warning(
    missing_devel_warning("noremotes"), "has compiled code, but no suitable compiler")

  # Windows
  local_mocked_bindings(
    sys_type = function(...) "windows",
    .package = "remotes"
  )
  expect_warning(
    missing_devel_warning("noremotes"), "Install Rtools")

  # MacOS
  local_mocked_bindings(
    sys_type = function(...) "macos",
    .package = "remotes"
  )

  expect_warning(
    missing_devel_warning("noremotes"), "Install XCode")

  # Linux
  local_mocked_bindings(
    sys_type = function(...) "linux",
    .package = "remotes"
  )
  expect_warning(
    missing_devel_warning("noremotes"), "Install compilers")
})
