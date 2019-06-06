
context("Utilities")

test_that("%||%", {
  expect_equal(NULL %||% "foo", "foo")
  expect_equal("foo" %||% "bar", "foo")
  expect_equal(NULL %||% NULL, NULL)
})

test_that("trim_ws", {
  expect_equal(trim_ws("foobar"), "foobar")
  expect_equal(trim_ws(" foobar"), "foobar")
  expect_equal(trim_ws("    foobar"), "foobar")
  expect_equal(trim_ws("foobar "), "foobar")
  expect_equal(trim_ws("foobar    "), "foobar")
  expect_equal(trim_ws(" foobar "), "foobar")
  expect_equal(trim_ws("    foobar    "), "foobar")

  expect_equal(trim_ws(character()), character())

  expect_equal(trim_ws(c("1", "2")), c("1", "2"))
  expect_equal(trim_ws(c(" 1", "2")), c("1", "2"))
  expect_equal(trim_ws(c("1 ", "2")), c("1", "2"))
  expect_equal(trim_ws(c(" 1 ", " 2")), c("1", "2"))
  expect_equal(trim_ws(c("1", " 2 ")), c("1", "2"))
  expect_equal(trim_ws(c("1 ", "2 ")), c("1", "2"))
  expect_equal(trim_ws(c("1  ", "  2")), c("1", "2"))
})

test_that("is_bioconductor", {

  D <- load_pkg_description(test_path("Biobase"))
  expect_true(is_bioconductor(D))

  D <- load_pkg_description(test_path("MASS"))
  expect_false(is_bioconductor(D))

})

test_that("pkg_installed", {

  expect_true(pkg_installed("methods"))
  expect_false(pkg_installed("there-is-no-such-package"))

  if (pkg_installed("codetools")) {
    tryCatch(
      {
        unloadNamespace("codetools")
        expect_true(pkg_installed("codetools"))
        expect_false("codetools" %in% loadedNamespaces())
      },
      error = function(e) { }
    )
  }

})

test_that("in_dir", {

  tmp <- tempfile()
  dir.create(tmp)

  ## We need the basename, because of the symbolic links

  wd <- getwd()
  expect_equal(
    basename(in_dir(tmp, getwd())),
    basename(tmp)
  )
  expect_equal(getwd(), wd)

  in_dir2 <- with_something(setwd)
  wd <- getwd()
  expect_equal(
    basename(in_dir2(tmp, getwd())),
    basename(tmp)
  )
  expect_equal(getwd(), wd)

})

# Adapted from https://github.com/gisle/mime-base64/blob/cf23d49e517c6ed8f4b24295f63721e8c9935010/t/base64.t
test_that("base64_decode", {

  decode_tests <- c(
    'YWE='   = 'aa',
    ' YWE='  =  'aa',
    'Y WE='  =  'aa',
    'YWE= '  =  'aa',
    "Y\nW\r\nE=" =  'aa',
    'YWE=====' =  'aa',    # extra padding
    'YWE'      =  'aa',    # missing padding
    'YWFh====' =  'aaa',
    'YQ'       =  'a',
    'Y'        = '',
    'x=='      = ''
  )

  for (i in seq_along(decode_tests)) {
    encoded <- names(decode_tests)[[i]]
    expected <- decode_tests[[i]]

    decoded <- base64_decode(encoded)
    expect_equal(decoded, expected)
  }
})

test_that("windows untar, --force-local errors", {
  do <- function(has, tar_result) {
    withr::local_envvar(c(TAR = ""))
    calls <- 0
    mockery::stub(untar, "system2", if (has) "--force-local" else "nah")
    mockery::stub(untar, "os_type", "windows")
    mockery::stub(untar, "utils::untar", function(extras, ...) {
      calls <<- calls + 1L
      if (grepl("force-local", extras)) tar_result() else "ok"
    })

    expect_equal(untar("foobar"), "ok")
    expect_equal(calls, 1 + has)
  }

  ## Has force-local but tar fails with it
  do(TRUE, function() stop("failed"))
  do(TRUE, function() 1L)
  do(TRUE, function() structure("blah", status = 1L))

  ## Does not have force-local
  do(FALSE, function() stop("failed"))
  do(FALSE, function() 1L)
  do(FALSE, function() structure("blah", status = 1L))
})

test_that("directories works", {
  expect_equal(directories("foo"), character())
  expect_equal(directories("foo/bar"), "foo")
  expect_equal(sort(directories("foo/bar/baz")),
               sort(c("foo", "foo/bar")))

  expect_equal(directories(c("foo/bar", "foo/baz")), "foo")

  expect_equal(sort(directories(c("foo/bar/baz", "foo2/1", "foo3/bar/3"))),
               sort(c("foo", "foo/bar", "foo2", "foo3", "foo3/bar")))
})

test_that("in_r_build_ignore works", {
  tf <- tempfile()
  on.exit(unlink(tf))
  writeLines(
    c("^foo$",
      "^blah/xyz"
    ), tf)

  expect_equal(
    unname(
      in_r_build_ignore(c("foo/bar/baz", "R/test.R"), tf)
    ),
    c(TRUE, FALSE)
  )

  expect_equal(
    unname(
      in_r_build_ignore(c("foo", "blah", "blah/abc", "blah/xyz", "R/test.R"), tf)
    ),
    c(TRUE, FALSE, FALSE, TRUE, FALSE)
  )
})

test_that("dev_split_ref works", {
  expect_equal(dev_split_ref("DT")[["pkg"]], "DT")
  expect_equal(dev_split_ref("remotes")[["ref"]], "")
  expect_equal(dev_split_ref("with.dot")[["pkg"]], "with.dot")
  expect_equal(dev_split_ref("with2")[["pkg"]], "with2")
  expect_equal(dev_split_ref("with@v1.2.1")[["ref"]], "@v1.2.1")
  expect_equal(dev_split_ref("with@v1.0.0.999")[["ref"]], "@v1.0.0.999")
  expect_equal(dev_split_ref("with@v1.0.0.999")[["pkg"]], "with")
  expect_equal(dev_split_ref("with#279")[["ref"]], "#279")
  expect_equal(dev_split_ref("with#1")[["pkg"]], "with")
})
