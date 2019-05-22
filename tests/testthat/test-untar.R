
context("untar")

test_that("untar is standalone", {
  ## baseenv() makes sure that the remotes package env is not used
  env <- new.env(parent = baseenv())
  env$s1_untar <- s1_untar
  stenv <- env$s1_untar$.internal
  objs <- ls(stenv, all.names = TRUE)
  funs <- Filter(function(x) is.function(stenv[[x]]), objs)
  funobjs <- mget(funs, stenv)

  expect_message(
    mapply(codetools::checkUsage, funobjs, funs,
           MoreArgs = list(report = message, suppressLocalUnused = TRUE)),
    NA)
})

test_that("one-file", {
  ex <- data.frame(
      stringsAsFactors = FALSE,
      filename = "test.txt",
      size = 12L,
      mtime = .POSIXct(1387580181),
      permissions = I(as.octmode("644")),
      type = "file",
      uid = 501L,
      gid = 20L,
      uname = "maf",
      gname = "staff"
    )

  expect_identical(
    s1_untar$list(test_path("fixtures", "untar", "one-file.tar")),
    ex
  )

  tmp <- test_temp_dir()
  expect_identical(
    s1_untar$extract(test_path("fixtures", "untar", "one-file.tar"), tmp),
    ex
  )

  expect_true(file.exists(tmp))
  expect_true(file.exists(file.path(tmp, "test.txt")))
  expect_equal(readChar(file.path(tmp, "test.txt"), 100), "hello world\n")
})

test_that("multi-file", {
  ex <- data.frame(
      stringsAsFactors = FALSE,
      filename = c("file-1.txt", "file-2.txt"),
      size = c(12L, 12L),
      mtime = rep(.POSIXct(1387580181), 2),
      permissions = I(as.octmode(c("644", "644"))),
      type = c("file", "file"),
      uid = c(501L, 501L),
      gid = c(20L, 20L),
      uname = c("maf", "maf"),
      gname = c("staff", "staff")
    )

  expect_identical(
    s1_untar$list(test_path("fixtures", "untar", "multi-file.tar")),
    ex
  )

  tmp <- test_temp_dir()
  expect_identical(
    s1_untar$extract(test_path("fixtures", "untar", "multi-file.tar"), tmp),
    ex
  )

  expect_true(file.exists(tmp))
  expect_true(file.exists(file.path(tmp, "file-1.txt")))
  expect_true(file.exists(file.path(tmp, "file-2.txt")))
  expect_equal(readChar(file.path(tmp, "file-1.txt"), 100), "i am file-1\n")
  expect_equal(readChar(file.path(tmp, "file-2.txt"), 100), "i am file-2\n")
})

test_that("pax", {
  ex <- data.frame(
      stringsAsFactors = FALSE,
      filename = "pax.txt",
      size = 12L,
      mtime = .POSIXct(1387580181),
      permissions = I(as.octmode("644")),
      type = "file",
      uid = 501L,
      gid = 20L,
      uname = "maf",
      gname = "staff"
    )

  expect_identical(
    s1_untar$list(test_path("fixtures", "untar", "pax.tar")),
    ex
  )

  tmp <- test_temp_dir()
  expect_identical(
    s1_untar$extract(test_path("fixtures", "untar", "pax.tar"), tmp),
    ex
  )

  expect_true(file.exists(tmp))
  expect_true(file.exists(file.path(tmp, "pax.txt")))
  expect_equal(readChar(file.path(tmp, "pax.txt"), 100), "hello world\n")
})

test_that("types", {
  ex <- data.frame(
      stringsAsFactors = FALSE,
      filename = c("directory", "directory-link"),
      size = c(0L, 0L),
      mtime = .POSIXct(c(1387580181, 1387580181)),
      permissions = I(as.octmode(c("755", "755"))),
      type = c("directory", "symlink"),
      uid = c(501L, 501L),
      gid = c(20L, 20L),
      uname = c("maf", "maf"),
      gname = c("staff", "staff")
    )

  expect_identical(
    s1_untar$list(test_path("fixtures", "untar", "types.tar")),
    ex
  )

  tmp <- test_temp_dir()
  expect_identical(
    s1_untar$extract(test_path("fixtures", "untar", "types.tar"), tmp),
    ex
  )

  expect_true(file.exists(tmp))
  expect_true(file.exists(file.path(tmp, "directory")))
  expect_true(file.exists(file.path(tmp, "directory-link")))
  expect_true(file.info(file.path(tmp, "directory"))$isdir)
  expect_equal(Sys.readlink(file.path(tmp, "directory-link")), "directory")
})
