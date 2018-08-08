context("devel")

test_that("has_devel", {
  # All platforms when tests are run should have a compiler
  expect_true(has_devel())

  # has_devel should return FALSE if an error occurs from has_devel2
  mockery::stub(
    has_devel, "has_devel2", function(...) stop("failed"))
  expect_false(has_devel())
})

test_that("has_devel2", {
  # has_devel2 should error if an error occurs from R CMD SHLIB
  mockery::stub(
    has_devel2, "R", function(...) stop("failed"))
  expect_error(has_devel2())
})

test_that("missing_devel_warning", {
  mockery::stub(
    missing_devel_warning, "has_devel2", function(...) FALSE)

  expect_warning(
    missing_devel_warning("noremotes"), "has compiled code, but no suitable compiler")

  # Windows
  mockery::stub(
    missing_devel_warning, "sys_type", function() "windows")
  expect_warning(
    missing_devel_warning("noremotes"), "Install Rtools")

  # MacOS
  mockery::stub(
    missing_devel_warning, "sys_type", function() "macos")
  expect_warning(
    missing_devel_warning("noremotes"), "Install XCode")

  # Linux
  mockery::stub(
    missing_devel_warning, "sys_type", function() "linux")
  expect_warning(
    missing_devel_warning("noremotes"), "Install compilers")
})
