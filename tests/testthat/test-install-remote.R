context("install-remotes.R")

test_that("different_sha returns TRUE if remote or local sha is NA not found", {
  expect_true(different_sha(remote_sha = NA, local_sha = "4a2ea2"))
  expect_true(different_sha(remote_sha = "4a2ea2", local_sha = NA))
  expect_true(different_sha(remote_sha = NA, local_sha = NA))
})

test_that("different_sha returns TRUE if remote_sha and local_sha are different", {
  expect_true(different_sha(remote_sha = "5b3fb3", local_sha = "4a2ea2"))
})

test_that("different_sha returns FALSE if remote_sha and local_sha are the same", {
  expect_false(different_sha(remote_sha = "4a2ea2", local_sha = "4a2ea2"))
})

test_that("local_sha returns NA if package is not installed", {
  expect_equal(local_sha("tsrtarst"), NA_character_)
})

test_that("package2remotes looks for the DESCRIPTION in .libPaths", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

   expect_equal(package2remote("noremotes")$sha, NA_character_)
   withr::with_temp_libpaths({
     expect_equal(package2remote("noremotes")$sha, NA_character_)
     install("noremotes", quiet = TRUE)
     expect_equal(package2remote("noremotes")$sha, "1.0.0")

     # Load the namespace, as packageDescription looks in loaded namespaces
     # first.
     loadNamespace("noremotes")
    })
  expect_equal(package2remote("noremotes")$sha, NA_character_)
})
