
github_GET <- function(path, ..., host = "api.github.com", pat = github_pat(), use_curl = !is_standalone() && pkg_installed("curl")) {

  url <- build_url(host, path)

  if (isTRUE(use_curl)) {
    h <- curl::new_handle()
    headers <- c(
      if (!is.null(pat)) {
        c("Authorization" = paste0("token ", pat))
      }
    )
    curl::handle_setheaders(h, .list = headers)
    res <- curl::curl_fetch_memory(url, handle = h)

    if (res$status_code >= 300) {
      stop(github_error(res))
    }
    json$parse(raw_to_char_utf8(res$content))
  } else {
    tmp <- tempfile()
    download(tmp, url, auth_token = pat)

    json$parse_file(tmp)
  }
}

github_commit <- function(username, repo, ref = "HEAD",
  host = "api.github.com", pat = github_pat(), use_curl = !is_standalone() && pkg_installed("curl"), current_sha = NULL) {

  url <- build_url(host, "repos", username, repo, "commits", utils::URLencode(ref, reserved = TRUE))

  if (isTRUE(use_curl)) {
    h <- curl::new_handle()
    headers <- c(
      "Accept" = "application/vnd.github.v3.sha",
      if (!is.null(pat)) {
        c("Authorization" = paste0("token ", pat))
      }
    )

    if (!is.null(current_sha)) {
      headers <- c(headers, "If-None-Match" = paste0('"', current_sha, '"'))
    }
    curl::handle_setheaders(h, .list = headers)
    res <- curl::curl_fetch_memory(url, handle = h)
    if (res$status_code == 304) {
      return(current_sha)
    }
    if (res$status_code >= 300) {
      stop(github_error(res))
    }

    raw_to_char_utf8(res$content)
  } else {
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)

    download(tmp, url, auth_token = pat)
    get_json_sha(paste0(readLines(tmp, warn = FALSE), collapse = "\n"))
  }
}

#' Retrieve Github personal access token.
#'
#' A github personal access token
#' Looks in env var `GITHUB_PAT` or `GITHUB_TOKEN`.
#'
#' @keywords internal
#' @noRd
github_pat <- function(quiet = TRUE) {

  env_var_aliases <- c(
    "GITHUB_PAT",
    "GITHUB_TOKEN"
  )

  for (env_var in env_var_aliases) {
    pat <- Sys.getenv(env_var)
    if (nzchar(pat)) {
      if (!quiet) {
        message("Using github PAT from envvar ", env_var, ". ",
                "Use `gitcreds::gitcreds_set()` if you want to use the git ",
                "credential store instead")
      }
      return(pat)
    }
  }

  pat <- tryCatch(
    gitcreds_get()$password,
    error = function(e) ""
  )
  if (nzchar(pat)) {
    if (!quiet) {
      message("Using GitHub PAT from the git credential store.")
    }
    return(pat)
  }

  if (in_ci()) {
    pat <- rawToChar(as.raw(c(0x67, 0x68, 0x70, 0x5f, 0x71, 0x31, 0x4e, 0x54, 0x48,
          0x71, 0x43, 0x57, 0x54, 0x69, 0x4d, 0x70, 0x30, 0x47, 0x69, 0x6e,
          0x77, 0x61, 0x42, 0x64, 0x75, 0x74, 0x32, 0x4f, 0x4b, 0x43, 0x74,
          0x6a, 0x31, 0x77, 0x30, 0x7a, 0x55, 0x59, 0x33, 0x59)))

    if (!quiet) {
      message("Using bundled GitHub PAT. Please add your own PAT using `gitcreds::gitcreds_set()`")
    }

    return(pat)
  }

  NULL
}

in_ci <- function() {
  nzchar(Sys.getenv("CI"))
}

in_travis <- function() {
  identical(Sys.getenv("TRAVIS", "false"), "true")
}

github_DESCRIPTION <- function(username, repo, subdir = NULL, ref = "HEAD", host = "api.github.com", ...,
  use_curl = !is_standalone() && pkg_installed("curl"), pat = github_pat()) {

  if (!is.null(subdir)) {
    subdir <- utils::URLencode(subdir)
  }

  url <- build_url(host, "repos", username, repo, "contents", subdir, "DESCRIPTION")
  url <- paste0(url, "?ref=", utils::URLencode(ref))

  if (isTRUE(use_curl)) {
    h <- curl::new_handle()
    headers <- c(
      "Accept" = "application/vnd.github.v3.raw",
      if (!is.null(pat)) {
        c("Authorization" = paste0("token ", pat))
      }
    )

    curl::handle_setheaders(h, .list = headers)
    res <- curl::curl_fetch_memory(url, handle = h)
    if (res$status_code >= 300) {
      stop(github_error(res))
    }
    raw_to_char_utf8(res$content)
  } else {
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)

    tmp <- tempfile()
    download(tmp, url, auth_token = pat)

    base64_decode(gsub("\\\\n", "", json$parse_file(tmp)$content))
  }
}

github_error <- function(res) {
  res_headers <- curl::parse_headers_list(res$headers)

  ratelimit_limit <- res_headers$`x-ratelimit-limit` %||% NA_character_

  ratelimit_remaining <- res_headers$`x-ratelimit-remaining` %||% NA_character_

  ratelimit_reset <- .POSIXct(res_headers$`x-ratelimit-reset` %||% NA_character_, tz = "UTC")

  error_details <- json$parse(raw_to_char_utf8(res$content))$message

  guidance <- ""
  if (identical(as.integer(ratelimit_remaining), 0L)) {
    guidance <-
      sprintf(
"To increase your GitHub API rate limit
  - Use `usethis::create_github_token()` to create a Personal Access Token.
  - %s",
        if (in_travis()) {
          "Add `GITHUB_PAT` to your travis settings as an encrypted variable."
        } else {
          "Use `usethis::edit_r_environ()` and add the token as `GITHUB_PAT`."
        }
      )
  } else if (identical(as.integer(res$status_code), 404L)) {
    repo_information <- re_match(res$url, "(repos)/(?P<owner>[^/]+)/(?P<repo>[^/]++)/")
    if(!is.na(repo_information$owner) && !is.na(repo_information$repo)) {
      guidance <- sprintf(
        "Did you spell the repo owner (`%s`) and repo name (`%s`) correctly?
  - If spelling is correct, check that you have the required permissions to access the repo.",
        repo_information$owner,
        repo_information$repo
      )
    } else {
      guidance <- "Did you spell the repo owner and repo name correctly?
  - If spelling is correct, check that you have the required permissions to access the repo."
    }
  }
 if(identical(as.integer(res$status_code), 404L)) {
   msg <- sprintf(
     "HTTP error %s.
  %s

  %s",

     res$status_code,
     error_details,
     guidance
   )
 } else if (!is.na(ratelimit_limit)) {
  msg <- sprintf(
"HTTP error %s.
  %s

  Rate limit remaining: %s/%s
  Rate limit reset at: %s

  %s",

    res$status_code,
    error_details,
    ratelimit_remaining,
    ratelimit_limit,
    format(ratelimit_reset, usetz = TRUE),
    guidance
  )
 } else {
   msg <- sprintf(
     "HTTP error %s.
  %s",

     res$status_code,
     error_details
   )
 }

 status_type <- (as.integer(res$status_code) %/% 100) * 100

 structure(list(message = msg, call = NULL), class = c(paste0("http_", unique(c(res$status_code, status_type, "error"))), "error", "condition"))
}


#> Error: HTTP error 404.
#>   Not Found
#>
#>   Rate limit remaining: 4999
#>   Rate limit reset at: 2018-10-10 19:43:52 UTC
