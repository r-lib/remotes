parse_submodules <- function(file) {
  if (grepl("\n", file)) {
    x <- strsplit(file, "\n")[[1]]
  } else {
    x <- readLines(file)
  }

  # https://git-scm.com/docs/git-config#_syntax
  # Subsection names are case sensitive and can contain any characters except
  # newline and the null byte. Doublequote " and backslash can be included by
  # escaping them as \" and \\
  double_quoted_string_with_escapes <- '(?:\\\\.|[^"])*'

  sections <- regexpr(
    sprintf('^[[:space:]]*\\[submodule "(%s)"\\][[:space:]]*$', double_quoted_string_with_escapes),
    x,
    perl = TRUE)

  # If no sections found return the empty list
  if (all(sections == -1)) {
    return(list())
  }

  # Otherwise extract section names
  section_names <- get_captures(x, sections)

  # Extract name = value
  # The variable names are case-insensitive, allow only alphanumeric characters
  # and -, and must start with an alphabetic character.
  variable_name <- "[[:alpha:]][[:alnum:]-]*"
  mappings <- regexpr(
    sprintf('^[[:space:]]*(%s)[[:space:]]*=[[:space:]]*(.*)[[:space:]]*$', variable_name),
    x,
    perl = TRUE
  )

  mapping_values <- get_captures(x, mappings)

  colnames(mapping_values) <- c("name", "value")

  values <- cbind(submodule = fill(section_names), mapping_values)
  values <- values[mappings != -1, ]
  values <- as.data.frame(values, stringsAsFactors = FALSE)

  # path and valid url are required
  if (!all(c("path", "url") %in% values$name)) {
    warning("Invalid submodule definition, skipping submodule installation", immediate. = TRUE, call. = FALSE)
    return(list())
  }

  # Roughly equivalent to tidyr::spread(values, name, value)
  res <- stats::reshape(values, idvar = "submodule", timevar = "name", v.name = "value", direction = "wide")

  # Set the column names, reshape prepends `value.` to path, url and branch
  colnames(res) <- gsub("value[.]", "", colnames(res))

  # path and valid url are required
  if (any(is.na(res$url), is.na(res$path))) {
    warning("Invalid submodule definition, skipping submodule installation", immediate. = TRUE, call. = FALSE)
    return(list())
  }

  # branch is optional
  if (!exists("branch", res)) {
    res$branch <- NA_character_
  }

  # Remove unneeded attribute
  attr(res, "reshapeWide") <- NULL

  # Remove rownames
  rownames(res) <- NULL

  res
}

# Adapted from https://stackoverflow.com/a/9517731/2055486
fill <- function(x, missing = "") {
  not_missing <- x != missing

  res <- x[not_missing]
  res[cumsum(not_missing)]
}

update_submodule <- function(url, path, branch, quiet) {
  args <- c('clone', '--depth', '1', '--no-hardlinks --recurse-submodules')
  if (length(branch) > 0 && !is.na(branch)) {
    args <- c(args, "--branch", branch)
  }
  args <- c(args, url, path)

  git(paste0(args, collapse = " "), quiet = quiet)
}

update_submodules <- function(source, quiet) {
  file <- file.path(source, ".gitmodules")
  if (!file.exists(file)) {
    return()
  }
  info <- parse_submodules(file)

  for (i in seq_len(NROW(info))) {
    update_submodule(info$url[[i]], file.path(source, info$path[[i]]), info$branch[[i]], quiet)
  }
}

get_captures <- function(str, match) {
  starts <- attr(match, "capture.start")
  ends <- starts + attr(match, "capture.length") - 1L
  matrix(substring(str, starts, ends), nrow = length(match))
}
