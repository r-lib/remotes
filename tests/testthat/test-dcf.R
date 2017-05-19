
context("DCF files")

test_that("read_dcf and write_dcf", {

  DESC <- "Package: foobar
Description: With continuation lines.
 Like
 .
 This one.
 .
 And this one.
Title: Foo Bar
"
  tmp <- tempfile()
  cat(DESC, file = tmp)
  dcf <- read_dcf(tmp)

  expect_equal(length(dcf), 3)
  expect_equal(names(dcf), c("Package", "Description", "Title"))

  tmp2 <- tempfile()
  on.exit(unlink(tmp2), add = TRUE)
  write_dcf(tmp2, dcf)
  
  DESC2 <- readChar(tmp2, nchars = file.info(tmp2)$size)

  ## This is a workaround for a write.dcf bug
  ## And also windows line ending characters
  DESC2 <- gsub("\r\n", "\n", DESC2)
  DESC2 <- gsub("\n .\n  ", "\n .\n ", DESC2)
  expect_equal(DESC, DESC2)
})
