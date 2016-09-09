
has_devel <- function() {
  tryCatch(
    has_devel2(),
    error = function(e) FALSE
  )
}

## This is similar to devtools:::has_devel(), with some
## very minor differences.

has_devel2 <- function() {
  foo_path <- file.path(tempfile(fileext = ".c"))

  cat("void foo(int *bar) { *bar=1; }\n", file = foo_path)
  on.exit(unlink(foo_path))

  R(c("CMD", "SHLIB", basename(foo_path)), dirname(foo_path))
  dylib <- sub("\\.c$", .Platform$dynlib.ext, foo_path)
  on.exit(unlink(dylib), add = TRUE)

  dll <- dyn.load(dylib)
  on.exit(dyn.unload(dylib), add = TRUE)

  stopifnot(.C(dll$foo, 0L)[[1]] == 1L)
  TRUE
}

missing_devel_warning <- function(pkgdir) {
  pkgname <- tryCatch(
    get_desc_field(file.path(pkgdir, "DESCRIPTION"), "Package"),
    error = function(e) NULL
  ) %||% "<unknown>"

  sys <- sys_type()

  warning(
    "Package ",
    pkgname,
    " has compiled code, but no suitable ",
    "compiler(s) were found. Installation will likely fail.\n  ",
    if (sys == "windows") "Install Rtools and make sure it is in the PATH.",
    if (sys == "macos") "Install XCode and make sure it works.",
    if (sys == "linux") "Install compilers via your Linux package manager."
  )
}

R <- function(args, path = tempdir()) {

  r <- file.path(R.home("bin"), "R")

  args <- c(
    "--no-site-file", "--no-environ", "--no-save",
    "--no-restore", "--quiet",
    args
  )

  system_check(r, args, path = path)
}
