tokenize_json <- function(text) {
  text <- paste(text, collapse = "\n")

  ESCAPE <- '(\\\\[^u[:cntrl:]]|\\\\u[0-9a-fA-F]{4})'
  CHAR <- '[^[:cntrl:]"\\\\]'

  STRING <- paste0('"', CHAR, '*(', ESCAPE, CHAR, '*)*"')
  NUMBER <- "-?(0|[1-9][0-9]*)([.][0-9]*)?([eE][+-]?[0-9]*)?"
  KEYWORD <- 'null|false|true'
  SPACE <- '[[:space:]]+'

  match <- gregexpr(
    pattern = paste0(
      STRING, "|", NUMBER, "|", KEYWORD, "|", SPACE, "|", "."
    ),
    text = text,
    perl = TRUE
  )

  grep("^\\s+$", regmatches(text, match)[[1]], value = TRUE, invert = TRUE)
}

throw <- function(...) {
  stop("JSON: ", ..., call. = FALSE)
}

fromJSONFile <- function(filename) {
  fromJSON(readLines(filename, warn = FALSE))
}

fromJSON <- function(text) {

  tokens <- tokenize_json(text)
  token <- NULL
  ptr <- 1

  read_token <- function() {
    if (ptr <= length(tokens)) {
      token <<- tokens[ptr]
      ptr <<- ptr + 1
    } else {
      token <<- 'EOF'
    }
  }

  parse_value <- function(name = "") {
    if (token == "{") {
      parse_object()
    } else if (token == "[") {
      parse_array()
    } else if (token == "EOF" || (nchar(token) == 1 && ! token %in% 0:9)) {
      throw("EXPECTED value GOT ", token)
    } else {
      j2r(token)
    }
  }

  parse_object <- function() {
    res <- structure(list(), names = character())

    read_token()

    ## Invariant: we are at the beginning of an element
    while (token != "}") {

      ## "key"
      if (grepl('^".*"$', token)) {
        key <- j2r(token)
      } else {
        throw("EXPECTED string GOT ", token)
      }

      ## :
      read_token()
      if (token != ":") { throw("EXPECTED : GOT ", token) }

      ## value
      read_token()
      res[key] <- list(parse_value())

      ## } or ,
      read_token()
      if (token == "}") {
        break
      } else if (token != ",") {
        throw("EXPECTED , or } GOT ", token)
      }
      read_token()
    }

    res
  }

  parse_array <- function() {
    res <- list()

    read_token()

    ## Invariant: we are at the beginning of an element
    while (token != "]") {
      ## value
      res <- c(res, list(parse_value()))

      ## ] or ,
      read_token()
      if (token == "]") {
        break
      } else if (token != ",") {
        throw("EXPECTED , GOT ", token)
      }
      read_token()
    }

    res
  }

  read_token()
  parse_value(tokens)
}

j2r <- function(token) {
  if (token == "null") {
    NULL
  } else if (token == "true") {
    TRUE
  } else if (token == "false") {
    FALSE
  } else if (grepl('^".*"$', token)) {
    trimq(token)
  } else {
    as.numeric(token)
  }
}

trimq <- function(x) {
  sub('^"(.*)"$', "\\1", x)
}

# Often we don't need to parse the whole json file, only extract a single
# record, which is faster and less error prone.
get_json_field <- function(text, field) {
  m <- regexpr(paste0('"', field, '"\\s*:\\s*"(\\w+)"'), text, perl = TRUE)
  if (all(m == -1)) {
    return(NA_character_)
  }

  start <- attr(m, "capture.start")
  end <- start + attr(m, "capture.length") - 1L
  substring(text, start, end)
}
