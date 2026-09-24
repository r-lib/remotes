# Query the system requirements for a package (and its dependencies)

Returns a character vector of commands to run that will install system
requirements for the queried operating system.

## Usage

``` r
system_requirements(
  os,
  os_release = NULL,
  path = ".",
  package = NULL,
  curl = Sys.which("curl")
)
```

## Arguments

- os, os_release:

  The operating system and operating system release version, see
  <https://github.com/rstudio/r-system-requirements#operating-systems>
  for the list of supported operating systems.

  If `os_release` is `NULL`, `os` must consist of the operating system
  and the version separated by a dash, e.g. `"ubuntu-18.04"`.

- path:

  The path to the dev package's root directory.

- package:

  CRAN package name(s) to lookup system requirements for. If not `NULL`,
  this is used and `path` is ignored.

- curl:

  The location of the curl binary on your system.

## Value

A character vector of commands needed to install the system requirements
for the package.
