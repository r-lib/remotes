
context("Decompress")

test_that("decompress various file types", {

  types <- c("zip", "tar", "tar.gz", "tgz")

  for (type in types) {

    fname <- paste0("foo.", type)
    archive <- file.path("archives", fname)
    dec <- tempfile()
    decompress(archive, dec)

    expect_true(
      file.exists(file.path(dec, "foo", "R", "foo.R")),
      info = type
    )
  }

})

test_that("decompress with internal unzip", {

  types <- c("zip", "tar", "tar.gz", "tgz")

  for (type in types) {

    fname <- paste0("foo.", type)
    archive <- file.path("archives", fname)

    dec <- tempfile()
    on.exit(unlink(dec, recursive = TRUE), add = TRUE)

    mockery::stub(
      decompress,
      "getOption",
      function(x, default = NULL) {
        if (x == "unzip") {
          "internal"
        } else {
          if (missing(default) || x %in% names(options())) {
            options()[[x]]
          } else {
            default
          }
        }
      }
    )

    decompress(archive, dec)

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

  foo_dir <- file.path("archives", "foo")
  expect_equal(source_pkg(foo_dir), foo_dir)

  bad_dir <- "archives"
  expect_error(
    source_pkg(bad_dir),
    "Does not appear to be an R package"
  )

  foo_tgz <- file.path("archives", "foo.tar.gz")
  pkg_dir <- source_pkg(foo_tgz)
  on.exit(unlink(pkg_dir, recursive = TRUE), add = TRUE)
  expect_true(file.exists(file.path(pkg_dir, "R", "foo.R")))
  expect_true(file.exists(file.path(pkg_dir, "configure")))

  skip_on_os("windows")

  expect_match(
    as.character(file.info(file.path(pkg_dir, "configure"))$mode),
    "7.."
  )
})

test_that("getrootdir",  {
  cases <- list(
    list(c("foo/bar", "foo/"), "foo"),
    list(c("/foo/bar/baz", "/foo/bar"), "/foo"),
    list(c("this/foo/bar", "this/that"), "this"),
    list(c("", "yes"), ".")
  )

  for (c in seq_along(cases)) {
    expect_identical(getrootdir(cases[[c]][[1]]), cases[[c]][[2]], info = c)
  }

  expect_error(getrootdir(character()))
})

test_that("my_unzip respects options('unzip')", {
  mockery::stub(my_unzip, "utils::unzip", function(...) int <<- TRUE)
  mockery::stub(my_unzip, "system_check", function(...) int <<- FALSE)

  int <- NULL
  withr::with_options(c("unzip" = "internal"), my_unzip("blah", "tg"))
  expect_true(int)

  int <- NULL
  withr::with_options(c("unzip" = ""), my_unzip("blah", "tg"))
  expect_true(int)

  int <- NULL
  withr::with_options(c("unzip" = "somethingelse"), my_unzip("blah", "tg"))
  expect_false(int)
})
