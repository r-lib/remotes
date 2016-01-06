
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
