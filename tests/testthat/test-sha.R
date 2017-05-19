
context("SHA")

test_that("different_sha returns TRUE if remote or local sha is NA not found", {
  expect_true(different_sha(remote_sha = NA, local_sha = "4a2ea2"))
  expect_true(different_sha(remote_sha = "4a2ea2", local_sha = NA))
  expect_true(different_sha(remote_sha = NA, local_sha = NA))
})

test_that("different_sha returns TRUE if remote_sha
           and local_sha are different", {
  expect_true(different_sha(remote_sha = "5b3fb3", local_sha = "4a2ea2"))
})

test_that("different_sha returns FALSE if remote_sha and local_sha
           are the same", {
  expect_false(different_sha(remote_sha = "4a2ea2", local_sha = "4a2ea2"))
})

test_that("local_sha returns NA if package is not installed", {
  expect_equal(local_sha("tsrtarst"), NA)
})

test_that("remote_sha.github_remote returns NA if remote doesn't exist", {
  expect_equal(remote_sha(github_remote("arst/arst")), NA)
})

test_that("remote_sha.github_remote returns expected value if
           remote does exist", {
  expect_equal(
    remote_sha(github_remote("hadley/devtools@v1.8.0")),
    "ad9aac7b9a522354e1ff363a86f389e32cec181b"
  )
})
