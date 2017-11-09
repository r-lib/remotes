copy_without_rbuildignore <- function(source, target) {
  exclude <- get_rbuildignore_exclusions(source)
  copy_without_excluded(source, target, exclude)
}

get_rbuildignore_exclusions <- function(path) {
  rbuildignore_path <- file.path(path, ".Rbuildignore")
  if (file.exists(rbuildignore_path)) readLines(rbuildignore_path, warn = FALSE)
  else character()
}

copy_without_excluded <- function(source, target, exclude) {
  files <- dir(source, recursive = TRUE)
  exclusions <- lapply(exclude, grepl, x = files, ignore.case = TRUE, perl = TRUE)
  excluded <- Reduce(`|`, exclusions)

  ret_included <- copy_files_deep(source, target, files[!excluded])

  ret <- rep(NA, length(files))
  ret[!excluded] <- ret_included
  ret
}

copy_files_deep <- function(source, target, files) {
  source_files <- file.path(source, files)
  target_files <- file.path(target, files)

  dirs <- unique(dirname(files))
  target_dirs <- file.path(target, dirs)
  lapply(target_dirs, dir.create, showWarnings = FALSE, recursive = TRUE)

  file.copy(source_files, target_files)
}
