context("rbuildignore")

get_all_combs <- function(x) {
  if (length(x) == 0) list(x)
  else {
    sub_combs <- get_all_combs(x[-1])
    c(sub_combs, lapply(sub_combs, function(sub) c(x[1], sub)))
  }
}

test_that("helper for deep copying", {
  N <- 4
  combs <- get_all_combs(seq_len(N))
  expect_length(combs, 2^N)
  expect_true(all(table(unlist(combs)) == 2 ^ (N - 1)))
  expect_equal(anyDuplicated(combs), 0)
  expect_true(all(vapply(combs, length, integer(1)) <= N))
})

create_tempdir_with_files <- function() {
  source <- tempfile()
  dir.create(source)
  withr::with_dir(
    source,
    {
      dir.create("a")
      dir.create("b/c", recursive = TRUE)
      writeLines("root", "root.txt")
      writeLines("a", "a/a.txt")
      writeLines("b", "b/b.txt")
      writeLines("c", "b/c/c.txt")
    }
  )
  source
}

test_that("deep copying", {
  source <- create_tempdir_with_files()
  source_files <- dir(source, recursive = TRUE)

  for (files in get_all_combs(source_files)) {
    target <- tempfile()
    dir.create(target)
    expect_true(all(copy_files_deep(source, target, !! files)))
    expect_setequal(dir(target, recursive = TRUE), !! files)
  }
})

test_that("compute inclusions", {
  source <- create_tempdir_with_files()
  source_files <- dir(source, recursive = TRUE)

  expect_setequal(
    get_files_from_exclude(source_files, character()),
    c("root.txt", "a/a.txt", "b/b.txt", "b/c/c.txt")
  )

  expect_setequal(
    get_files_from_exclude(source_files, "^root.txt$"),
    c("a/a.txt", "b/b.txt", "b/c/c.txt")
  )

  expect_setequal(
    get_files_from_exclude(source_files, c("^root.txt$", "^b/b\\.txt$")),
    c("a/a.txt", "b/c/c.txt")
  )

  expect_setequal(
    get_files_from_exclude(source_files, "^a/"),
    c("root.txt", "b/b.txt", "b/c/c.txt")
  )

  expect_setequal(
    get_files_from_exclude(source_files, "^b/"),
    c("root.txt", "a/a.txt")
  )
})
