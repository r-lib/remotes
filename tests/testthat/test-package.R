context("package")

test_that("load_pkg_description", {
  pkg <- load_pkg_description("noremotes")
  expect_equal(pkg$package, "noremotes")
})

test_that("load_pkg_description tarball", {

  skip_on_cran()
  skip_if_offline()

  repos <- getOption("repos")
  if (length(repos) == 0) repos <- character()
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com"

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  out <- download.packages("pkgconfig", repos = repos, destdir = tmp_dir, quiet = TRUE)[[2]]

  pkg <- load_pkg_description(out)

  expect_equal(pkg$package, "pkgconfig")
})
