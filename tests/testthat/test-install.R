context("install")

test_that("safe_build_package works with pkgbuild", {
  out <- tempfile()
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE))
  opts <- c("--no-resave-data", "--no-manual", "--no-build-vignettes")
  expect_equal(
    safe_build_package(test_path("noremotes"), build_opts = opts , out, quiet = TRUE, use_pkgbuild = TRUE),
    file.path(out, "noremotes_1.0.0.tar.gz"))
})

test_that("safe_build_package works without pkgbuild", {
  out <- tempfile()
  dir.create(out)
  on.exit(unlink(out))
  opts <- c("--no-resave-data", "--no-manual", "--no-build-vignettes")
  expect_equal(
    safe_build_package(test_path("noremotes"), build_opts = opts , out, quiet = TRUE, use_pkgbuild = FALSE),
    file.path(out, "noremotes_1.0.0.tar.gz"))
})

test_that("safe_build_package fails appropriately with pkgbuild", {
  out <- tempfile()
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE))
  opts <- c("--no-resave-data", "--no-manual", "--no-build-vignettes")
  expect_error(
    safe_build_package(test_path("invalidpkg"), build_opts = opts , out, quiet = TRUE, use_pkgbuild = TRUE),
    "System command error"
  )
})

test_that("safe_build_package fails appropriately without pkgbuild", {
  out <- tempfile()
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE))
  opts <- c("--no-resave-data", "--no-manual", "--no-build-vignettes")
  capture.output(
    expect_error(fixed = TRUE,
    safe_build_package(test_path("invalidpkg"), build_opts = opts , out, quiet = TRUE, use_pkgbuild = FALSE),
    "Failed to `R CMD build` package"
  ))
})

test_that("safe_build_package calls pkgbuild with appropriate arguments", {
  mockery::stub(safe_build_package, "pkgbuild::build", function(...) list(...))

  expect_equal(
    safe_build_package(
      "foo",
      build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
      "bar",
      quiet = TRUE,
      use_pkgbuild = TRUE),

    list("foo",
      dest_path = "bar",
      binary = FALSE,
      vignettes = FALSE,
      manual = FALSE,
      args = "--no-resave-data", quiet = TRUE)
  )

  expect_equal(
    safe_build_package(
      "foo",
      build_opts = c("--no-resave-data", "--no-build-vignettes"),
      "bar",
      quiet = TRUE,
      use_pkgbuild = TRUE),

    list("foo",
      dest_path = "bar",
      binary = FALSE,
      vignettes = FALSE,
      manual = TRUE,
      args = "--no-resave-data", quiet = TRUE)
  )

  expect_equal(
    safe_build_package(
      "foo",
      build_opts = c("--no-resave-data"),
      "bar",
      quiet = TRUE,
      use_pkgbuild = TRUE),

    list("foo",
      dest_path = "bar",
      binary = FALSE,
      vignettes = TRUE,
      manual = TRUE,
      args = "--no-resave-data", quiet = TRUE)
  )
})

test_that("should_error_for_warnings works", {

  # If both unset, should error -> TRUE
  withr::with_envvar(c("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = NA, "_R_CHECK_FORCE_SUGGESTS_" = NA),
    expect_true(should_error_for_warnings())
  )

  # If no errors true, should error -> FALSE
  withr::with_envvar(c("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true", "_R_CHECK_FORCE_SUGGESTS_" = NA),
    expect_false(should_error_for_warnings())
  )

  # If no errors unset, and force_suggests false, should error -> FALSE
  withr::with_envvar(c("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = NA, "_R_CHECK_FORCE_SUGGESTS_" = "false"),
    expect_false(should_error_for_warnings())
  )

  # If no errors unset, and force_suggests true, should error -> TRUE
  withr::with_envvar(c("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = NA, "_R_CHECK_FORCE_SUGGESTS_" = "true"),
    expect_true(should_error_for_warnings())
  )
})
