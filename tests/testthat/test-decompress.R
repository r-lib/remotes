
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
