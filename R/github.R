github_auth <- function(token) {
  if (is.null(token)) {
    NULL
  } else {
    httr::authenticate(token, "x-oauth-basic", "basic")
  }
}

github_response <- function(req) {
  text <- httr::content(req, as = "text")
  parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE)

  if (httr::status_code(req) >= 400) {
    errors <- vapply(parsed$errors, `[[`, "message", FUN.VALUE = character(1))

    stop(
      parsed$message, " (", httr::status_code(req), ")\n",
      paste("* ", errors, collapse = "\n"),
      call. = FALSE
    )
  }

  parsed
}

github_GET <- function(path, ..., pat = github_pat()) {
  auth <- github_auth(pat)
  req <- httr::GET("https://api.github.com/", path = path, auth, ...)
  github_response(req)
}

github_commit <- function(username, repo, ref = "master") {
  github_GET(file.path("repos", username, repo, "commits", ref))
}

#' Retrieve Github personal access token.
#'
#' A github personal access token
#' Looks in env var \code{GITHUB_PAT}
#'
#' @keywords internal
#' @noRd
github_pat <- function() {
  pat <- Sys.getenv('GITHUB_PAT')
  if (identical(pat, "")) return(NULL)

  message("Using github PAT from envvar GITHUB_PAT")
  pat
}
