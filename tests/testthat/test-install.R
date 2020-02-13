context("install")

test_that("safe_build_package works with pkgbuild", {
  out <- tempfile()
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE))
  opts <- c("--no-resave-data", "--no-manual", "--no-build-vignettes")
  expect_equal(
    safe_build_package(test_path("noremotes"), build_opts = opts, build_manual = FALSE, build_vignettes = FALSE, out, quiet = TRUE, use_pkgbuild = TRUE),
    file.path(out, "noremotes_1.0.0.tar.gz"))
})

test_that("safe_build_package works without pkgbuild", {
  out <- tempfile()
  dir.create(out)
  on.exit(unlink(out))
  opts <- c("--no-resave-data", "--no-manual", "--no-build-vignettes")
  expect_equal(
    safe_build_package(test_path("noremotes"), build_opts = opts, build_manual = FALSE, build_vignettes = FALSE, out, quiet = TRUE, use_pkgbuild = FALSE),
    file.path(out, "noremotes_1.0.0.tar.gz"))
})

test_that("safe_build_package fails appropriately with pkgbuild", {
  out <- tempfile()
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE))
  opts <- c("--no-resave-data", "--no-manual", "--no-build-vignettes")
  expect_error(
    safe_build_package(test_path("invalidpkg"), build_opts = opts, build_manual = FALSE, build_vignettes = FALSE, out, quiet = TRUE, use_pkgbuild = TRUE),
    "System command",
    class = "system_command_status_error"
  )
})

test_that("safe_build_package fails appropriately without pkgbuild", {
  out <- tempfile()
  dir.create(out)
  on.exit(unlink(out, recursive = TRUE))
  opts <- c("--no-resave-data", "--no-manual", "--no-build-vignettes")
  capture.output(
    expect_error(fixed = TRUE,
    safe_build_package(test_path("invalidpkg"), build_opts = opts, build_manual = FALSE, build_vignettes = FALSE, out, quiet = TRUE, use_pkgbuild = FALSE),
    "Failed to `R CMD build` package"
  ))
})

test_that("safe_build_package calls pkgbuild with appropriate arguments", {
  mockery::stub(safe_build_package, "pkgbuild::build", function(...) list(...))

  expect_equal(
    safe_build_package(
      "foo",
      build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
      build_manual = FALSE,
      build_vignettes = FALSE,
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
      build_manual = TRUE,
      build_vignettes = FALSE,
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
      build_manual = TRUE,
      build_vignettes = TRUE,
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
  withr::with_envvar(c("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "1", "_R_CHECK_FORCE_SUGGESTS_" = NA),
    expect_false(should_error_for_warnings())
  )

  # If no errors unset, and force_suggests false, should error -> FALSE
  withr::with_envvar(c("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = NA, "_R_CHECK_FORCE_SUGGESTS_" = "false"),
    expect_false(should_error_for_warnings())
  )
  withr::with_envvar(c("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = NA, "_R_CHECK_FORCE_SUGGESTS_" = "0"),
    expect_false(should_error_for_warnings())
  )

  # If no errors unset, and force_suggests true, should error -> TRUE
  withr::with_envvar(c("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = NA, "_R_CHECK_FORCE_SUGGESTS_" = "true"),
    expect_true(should_error_for_warnings())
  )
  withr::with_envvar(c("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = NA, "_R_CHECK_FORCE_SUGGESTS_" = "1"),
    expect_true(should_error_for_warnings())
  )

})

test_that("normalize_build_opts works", {
  build_opts <- c("--no-resave-data", "--no-manual", "--no-build-vignettes")

  # If build_vignettes = TRUE, should not include --no-build-vignettes
  expect_equal(
    normalize_build_opts(character(), build_vignettes = TRUE, build_manual = TRUE),
    character()
  )
  expect_equal(
    normalize_build_opts("--no-build-vignettes", build_vignettes = TRUE, build_manual = TRUE),
    character()
  )

  # If build_vignettes = FALSE, should always include --no-build-vignettes
  expect_equal(
    normalize_build_opts(character(), build_vignettes = FALSE, build_manual = TRUE),
    "--no-build-vignettes"
  )
  expect_equal(
    normalize_build_opts("--no-build-vignettes", build_vignettes = FALSE, build_manual = TRUE),
    "--no-build-vignettes"
  )

  # If build_manual = TRUE, should not include --no-manual
  expect_equal(
    normalize_build_opts(character(), build_vignettes = TRUE, build_manual = TRUE),
    character()
  )
  expect_equal(
    normalize_build_opts("--no-manual", build_vignettes = TRUE, build_manual = TRUE),
    character()
  )

  # If build_manual = FALSE, should always include --no-build-manual
  expect_equal(
    normalize_build_opts(character(), build_vignettes = TRUE, build_manual = FALSE),
    "--no-manual"
  )
  expect_equal(
    normalize_build_opts("--no-manual", build_vignettes = TRUE, build_manual = FALSE),
    "--no-manual"
  )

  # Other arguments are ignored regardless of build_manual and build_vignettes
  expect_equal(
    normalize_build_opts(build_opts, build_vignettes = FALSE, build_manual = FALSE),
    c("--no-resave-data", "--no-manual", "--no-build-vignettes")
  )
  expect_equal(
    normalize_build_opts(build_opts, build_vignettes = TRUE, build_manual = FALSE),
    c("--no-resave-data", "--no-manual")
  )
  expect_equal(
    normalize_build_opts(build_opts, build_vignettes = FALSE, build_manual = TRUE),
    c("--no-resave-data", "--no-build-vignettes")
  )
  expect_equal(
    normalize_build_opts(build_opts, build_vignettes = TRUE, build_manual = TRUE),
    c("--no-resave-data")
  )
})
