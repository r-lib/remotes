
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

get_desc_from_url <- function(url) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  dir.create(tmp)
  tmp2 <- file.path(tmp, "DESCRIPTION")
  download(tmp2, url, auth_token = NULL)
  load_pkg_description(tmp)
}

test_that("is_bioconductor", {

  skip_on_cran()
  skip_if_offline()

  url <- "https://readonly:readonly@hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/Biobase/DESCRIPTION"
  D <- get_desc_from_url(url)
  expect_true(is_bioconductor(D))

  url <- "https://raw.githubusercontent.com/cran/MASS/master/DESCRIPTION"
  D <- get_desc_from_url(url)
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
