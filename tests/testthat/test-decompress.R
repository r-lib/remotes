
context("Decompress")

test_that("decompress various file types", {

  types <- c("zip", "tar", "tar.gz", "tgz", "tar.bz2", "tbz")

  for (type in types) {

    fname <- paste0("foo.", type)
    archive <- system.file(package = packageName(), "archives", fname)
    dec <- tempfile()
    decompress(archive, dec)

    expect_true(
      file.exists(file.path(dec, "foo", "R", "foo.R")),
      info = type
    )
  }

})

test_that("decompress with internal unzip", {

  types <- c("zip", "tar", "tar.gz", "tgz", "tar.bz2", "tbz")

  for (type in types) {

    fname <- paste0("foo.", type)
    archive <- system.file(package = packageName(), "archives", fname)

    dec <- tempfile()
    on.exit(unlink(dec, recursive = TRUE), add = TRUE)

    with_mock(
      `base::getOption` = function(x, default = NULL) {
        if (x == "unzip") {
          "internal"
        } else {
          if (missing(default) || x %in% names(options())) {
            options()[[x]]
          } else {
            default
          }
        }
      },
      decompress(archive, dec)
    )

    expect_true(
      file.exists(file.path(dec, "foo", "R", "foo.R")),
      info = type
    )
  }

})

test_that("decompress errors on unknown file types", {

  tmp <- tempfile(fileext = ".foobar")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cat("surprise!", file = tmp)
  expect_error(
    decompress(tmp, tempdir()),
    "Don't know how to decompress"
  )

})

test_that("source_pkg", {

  foo_dir <- system.file(package = packageName(), "archives", "foo")
  expect_equal(source_pkg(foo_dir), foo_dir)

  foo_tgz <- system.file(package = packageName(), "archives", "foo.tar.gz")
  pkg_dir <- source_pkg(foo_tgz)
  on.exit(unlink(pkg_dir, recursive = TRUE), add = TRUE)
  expect_true(file.exists(file.path(pkg_dir, "R", "foo.R")))
  expect_true(file.exists(file.path(pkg_dir, "configure")))
  expect_match(file.info(file.path(pkg_dir, "configure"))$mode, "7..")

  bad_dir <- system.file(package = packageName(), "archives")
  expect_error(
    source_pkg(bad_dir),
    "Does not appear to be an R package"
  )
})
