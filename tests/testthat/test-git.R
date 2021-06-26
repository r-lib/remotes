
context("Git")

test_that("git_extract_sha1_tar", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  sha <- "fbae60ced0afee0e7c0f8dc3b5b1bb48d303f3dd"
  url <- build_url(
    "api.github.com/repos/hadley/devtools/tarball",
    sha
  )

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  download(tmp, url, auth_token = github_pat())

  expect_equal(
    git_extract_sha1_tar(tmp),
    sha
  )
})


test_that("git not quiet", {

  mockery::stub(git, "check_git_path", "/foo/git")
  mockery::stub(git, "system", "0")
  expect_message(
    git(args = c("arg1", "arg2"), quiet = FALSE),
    "['\"]/foo/git['\"] arg1arg2"
  )
})


test_that("git error", {

  mockery::stub(git, "check_git_path", "/foo/git")
  mockery::stub(git, "system", structure("foo", status = "1"))
  expect_error(git(args = "arg"), "Command failed")
})


test_that("git_path", {

  tmp <- tempfile()
  expect_error(
    git_path(tmp),
    "does not exist"
  )

  cat("Hello", file = tmp)
  expect_equal(git_path(tmp), tmp)

  mockery::stub(git_path, "Sys.which", "")
  mockery::stub(git_path, "os_type", "windows")
  mockery::stub(git_path, "file.exists", FALSE)
  expect_null(git_path())
})


test_that("check_git_path", {

  mockery::stub(check_git_path, "git_path", NULL)
  expect_error(
    check_git_path(),
    "Git does not seem to be installed on your system"
  )
})


test_that("git urls are properly parsed, anonymized and censored", {
  prot <- "http://"
  username <- "janedoe"
  password <- "12345"
  asterisks <- strrep("*", 8L)
  url <- "www.gitzone.com/namespace/repo.git"
  ref <- "HEAD"

  df <- expand.grid(
    prot = c("", prot),
    username = c("", username),
    password = c("", password),
    url = url,
    ref = c("", ref),
    stringsAsFactors = FALSE
  )

  # filter invalid urls with password but no username
  df <- df[!(!nchar(df$username) & nchar(df$password)),]

  # format url components and build permuted urls
  df$auth <- with(df, paste0(
    username,
    ifelse(nzchar(password), paste0(":", password), ""),
    ifelse(nzchar(username), "@", "")
  ))
  df$ref_str <- with(df, ifelse(nzchar(ref), paste0("@", ref), ""))
  df$full_url <- with(df, paste0(prot, auth, url, ref_str))

  for (i in seq_len(nrow(df))) {
    meta <- parse_git_url(df[i,"full_url"])
    expect_equal(meta$prot, df$prot[i])
    expect_equal(meta$auth, df$auth[i])
    expect_equal(meta$username, df$username[i])
    expect_equal(meta$password, df$password[i])
    expect_equal(meta$url, df$url[i])
    expect_equal(meta$ref, df$ref[i])
  }

  expect_true(!any(grepl(password, git_anon_url(df$full_url))))
  expect_true(!any(grepl(paste0(username, "|", password), git_anon_url(df$full_url))))

  expect_equal(git_anon_url(url), url)
  expect_equal(git_anon_url(i <- paste0(prot, url)), i)
  expect_equal(git_anon_url(paste0(prot, url, "@", ref)), paste0(prot, url))
  expect_equal(git_anon_url(paste0(url, "@", ref)), url)
  expect_equal(git_anon_url(paste0(username, "@", url, "@", ref)), url)
  expect_equal(git_anon_url(paste0(prot, username, "@", url, "@", ref)), paste0(prot, url))
  expect_equal(git_anon_url(paste0(username, ":", password, "@", url)), url)
  expect_equal(git_anon_url(paste0(username, ":", password, "@", url, "@", ref)), url)
  expect_equal(git_anon_url(paste0(prot, username, ":", password, "@", url, "@", ref)), paste0(prot, url))

  expect_true(!any(grepl(password, git_censored_url(df$full_url))))
  expect_equal(git_censored_url(df$full_url), gsub(password, asterisks, paste0(df$prot, df$auth, df$url)))

  expect_equal(git_censored_url(url), url)
  expect_equal(git_censored_url(i <- paste0(prot, url)), i)
  expect_equal(git_censored_url(paste0(prot, url, "@", ref)), paste0(prot, url))
  expect_equal(git_censored_url(paste0(url, "@", ref)), url)
  expect_equal(
    git_censored_url(paste0(username, "@", url, "@", ref)), 
    paste0(username, "@", url))
  expect_equal(
    git_censored_url(paste0(prot, username, "@", url, "@", ref)), 
    paste0(prot, username, "@", url))
  expect_equal(
    git_censored_url(paste0(username, ":", password, "@", url)), 
    paste0(username, ":", asterisks, "@", url))
  expect_equal(
    git_censored_url(paste0(username, ":", password, "@", url, "@", ref)), 
    paste0(username, ":", asterisks, "@", url))
  expect_equal(
    git_censored_url(paste0(prot, username, ":", password, "@", url, "@", ref)), 
    paste0(prot, username, ":", asterisks, "@", url))
})


test_that("parse_git_url handles ssh-style repo urls", {
  username <- "git"
  url <- "gitzone.com:namespace/repo.git"
  git_url <- paste0(username, "@", url)
  ref <- "HEAD"

  meta <- parse_git_url(git_url)
  expect_equal(meta$prot, "")
  expect_equal(meta$auth, "git@")
  expect_equal(meta$username, "git")
  expect_equal(meta$password, "")
  expect_equal(meta$url, url)
  expect_equal(meta$ref, "")

  meta <- parse_git_url(paste0(git_url, "@", ref))
  expect_equal(meta$prot, "")
  expect_equal(meta$auth, paste0(username, "@"))
  expect_equal(meta$username, username)
  expect_equal(meta$password, "")
  expect_equal(meta$url, url)
  expect_equal(meta$ref, ref)
})
