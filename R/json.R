
json_dict_get <- function(con, key) {
  text <- paste(readLines(con, warn = FALSE), collapse = " ")
  chr <- strsplit(text, "")[[1]]

  in_string <- FALSE
  in_topkey <- FALSE
  escaped <- FALSE
  level <- 0
  keyword <- ""
  for (i in seq_along(chr)) {
    c <- chr[[i]]

    ## Whatever is escaped, ignore it
    if (escaped) {
      escaped <- FALSE

    ## Ignore escape characters
    } else if (c == "\\") {
      escaped <- TRUE

    ## If in a top keyword that ends, maybe got it
    } else if (in_topkey  && c == '"') {
      if (keyword == key) { break }
      keyword <- ""
      in_topkey <- FALSE

    ## Otherwise if in top keyword, collect it
    } else if (in_topkey) {
      keyword <- paste0(keyword, c)

    ## If in a string, and ", the string is over
    } else if (in_string && c == '"') {
      in_string <- FALSE

    ## Otherwise ignore the string
    } else if (in_string) {

    ## We are not in a string
    ## Starting a string, maybe a top key
    } else if (c == '"') {
      if (level == 2) in_topkey <- TRUE else in_string <- TRUE

    ## We go one level deeper, proper levels are even numbers
    } else if (c == "{" || c == "[") {
      level <- level + 2

    ## We go one level higher
    } else if (c == "}" || c == "]") {
      level <- level - 2

    ## A top level value is considered as an odd level
    } else if (level == 2 && c == ":") {
      level <- level + 1

    ## We are in a top-level value
    } else if (level == 3 && c == ",") {
      level <- level - 1

    ## just ignore other stuff (whitespace, mostly, some commas)
    } else {

    }
  }

  if (keyword != "sha") {
    stop('Cannot get "sha" from GitHub', call. = FALSE)
  }

  ## Look for sha at position i
  atsha <- paste(chr[i:length(chr)], collapse = "")

  sub('^"\\s*:\\s*"([a-fA-F0-9]+)".*$', "\\1", atsha)
}
