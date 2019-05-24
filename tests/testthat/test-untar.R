
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
    gname = "staff",
    extra = I(list(list()))
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
    gname = c("staff", "staff"),
    extra = I(list(list(), list()))
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
    gname = "staff",
    extra = I(list(list(path = "pax.txt", special = "sauce")))
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
    gname = c("staff", "staff"),
    extra = I(list(list(), list()))
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

test_that("long-name", {
  name <- paste0("my/file/is/longer/than/100/characters/and/should/use/",
                 "the/prefix/header/foobarbaz/foobarbaz/foobarbaz/",
                 "foobarbaz/foobarbaz/foobarbaz/filename.txt")
  ex <- data.frame(
    stringsAsFactors = FALSE,
    filename = name,
    size = 16L,
    mtime = .POSIXct(1387580181),
    permissions = I(as.octmode("644")),
    type = "file",
    uid = 501L,
    gid = 20L,
    uname = "maf",
    gname = "staff",
    extra = I(list(list()))
  )

  expect_identical(
    s1_untar$list(test_path("fixtures", "untar", "long-name.tar")),
    ex
  )

  tmp <- normalizePath(test_temp_dir(), winslash = "/")
  expect_identical(
    s1_untar$extract(test_path("fixtures", "untar", "long-name.tar"), tmp),
    ex
  )

  expect_true(file.exists(tmp))
  expect_true(file.exists(file.path(tmp, name)))
  expect_equal(readChar(file.path(tmp, name), 100), "hello long name\n")
})

test_that("gnu long path", {
  name <- paste0("node-v0.11.14/deps/npm/node_modules/init-package-json/",
                 "node_modules/promzard/example/npm-init/init-input.js")
  tl <- s1_untar$list(test_path("fixtures", "untar", "gnu-long-path.tar"))
  expect_identical(tl$filename, name)
})

test_that("name is 100", {
  tl <- s1_untar$list(test_path("fixtures", "untar", "name-is-100.tar"))
  expect_equal(nchar(tl$filename), 100L)
})

test_that("unicode bsd", {
  ex <- data.frame(
    stringsAsFactors = FALSE,
    filename = "h\u00f8ll\u00f8.txt",
    size = 4L,
    mtime = .POSIXct(1387588646),
    permissions = I(as.octmode("644")),
    type = "file",
    uid = 501L,
    gid = 20L,
    uname = "maf",
    gname = "staff",
    extra = I(list(list(
      path = "h\u00f8ll\u00f8.txt", ctime = "1387588646", atime = "1387589077",
      "SCHILY.dev" = "16777217", "SCHILY.ino" = "3599143",
      "SCHILY.nlink" = "1")))
  )

  expect_identical(
    s1_untar$list(test_path("fixtures", "untar", "unicode-bsd.tar")),
    ex
  )

  tmp <- test_temp_dir()
  expect_identical(
    s1_untar$extract(test_path("fixtures", "untar", "unicode-bsd.tar"), tmp),
    ex
  )

  expect_true(file.exists(tmp))
  expect_true(file.exists(file.path(tmp, "h\u00f8ll\u00f8.txt")))
  expect_equal(readChar(file.path(tmp, "h\u00f8ll\u00f8.txt"), 100), "hej\n")
})

test_that("unicode", {
  ex <- data.frame(
    stringsAsFactors = FALSE,
    filename = "h\u00f8st\u00e5l.txt",
    size = 8L,
    mtime = .POSIXct(1387580181),
    permissions = I(as.octmode("644")),
    type = "file",
    uid = 501L,
    gid = 20L,
    uname = "maf",
    gname = "staff",
    extra = I(list(list(path = "h\u00f8st\u00e5l.txt")))
  )

  expect_identical(
    s1_untar$list(test_path("fixtures", "untar", "unicode.tar")),
    ex
  )

  tmp <- test_temp_dir()
  expect_identical(
    s1_untar$extract(test_path("fixtures", "untar", "unicode.tar"), tmp),
    ex
  )

  expect_true(file.exists(tmp))
  expect_true(file.exists(file.path(tmp, "h\u00f8st\u00e5l.txt")))
  expect_equal(readChar(file.path(tmp, "h\u00f8st\u00e5l.txt"), 100),
               "h\u00f8ll\u00f8\n")
})

test_that("invalid file", {
  expect_error(
    s1_untar$list(test_path("fixtures", "untar", "invalid.tgz")),
    "Failed to decode tar header"
  )
})

test_that("space prefixed", {
  expect_error(
    s1_untar$list(test_path("fixtures", "untar", "space.tar")),
    NA
  )
})

test_that("base 256 uid and gid", {
  tl <- s1_untar$list(test_path("fixtures", "untar",
                                "base-256-uid-gid.tar"))
  expect_identical(tl$uid, 116435139L)
  expect_identical(tl$gid, 1876110778L)
})

test_that("base 256 size", {
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
    gname = "staff",
    extra = I(list(list()))
  )

  expect_identical(
    s1_untar$list(test_path("fixtures", "untar", "base-256-size.tar")),
    ex)

  tmp <- test_temp_dir()
  expect_identical(
    s1_untar$extract(test_path("fixtures", "untar", "base-256-size.tar"),
                     tmp),
    ex
  )

  expect_true(file.exists(tmp))
  expect_true(file.exists(file.path(tmp, "test.txt")))
  expect_equal(readChar(file.path(tmp, "test.txt"), 100), "hello world\n")
})

test_that("latin1", {
  name <- "En fran\xe7ais, s'il vous pla\xeet?.txt"
  Encoding(name) <- "latin1"
  ex <- data.frame(
    stringsAsFactors = FALSE,
    filename = name,
    size = 14L,
    mtime = .POSIXct(1495941034),
    permissions = I(as.octmode("644")),
    type = "file",
    uid = 0L,
    gid = 0L,
    uname = "root",
    gname = "root",
    extra = I(list(list()))
  )

  expect_identical(
    s1_untar$list(test_path("fixtures", "untar", "latin1.tar"),
                  options = list(filename_encoding = "latin1")),
    ex)
})

test_that("incomplete", {
  expect_error(
    s1_untar$list(test_path("fixtures", "untar", "incomplete.tar")),
    "Unexpected end of tar data")

  tmp <- test_temp_dir()
  expect_error(
    s1_untar$extract(test_path("fixtures", "untar", "incomplete.tar"), tmp),
    "Unexpected end of tar data")

  expect_true(file.exists(tmp))
  expect_false(file.exists(file.path(tmp, "file-1.txt")))
})
