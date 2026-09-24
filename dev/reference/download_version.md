# Download a specified version of a CRAN package

It downloads the package to a temporary file, and returns the name of
the file.

## Usage

``` r
download_version(
  package,
  version = NULL,
  repos = getOption("repos"),
  type = getOption("pkgType"),
  ...
)
```

## Arguments

- package:

  Name of the package to install.

- version:

  Version of the package to install. Can either be a string giving the
  exact version required, or a specification in the same format as the
  parenthesized expressions used in package dependencies. One of the
  following formats:

  - An exact version required, as a string, e.g. `"0.1.13"`

  - A comparison operator and a version, e.g. `">= 0.1.12"`

  - Several criteria to satisfy, as a comma-separated string, e.g.
    `">= 1.12.0, < 1.14"`

  - Several criteria to satisfy, as elements of a character vector, e.g.
    `c(">= 1.12.0", "< 1.14")`

- repos:

  character vector, the base URL(s) of the repositories to use, e.g.,
  the URL of a CRAN mirror such as `"https://cloud.r-project.org"`. For
  more details on supported URL schemes see
  [`url`](https://rdrr.io/r/base/connections.html).

  Can be `NULL` to install from local files, directories or URLs: this
  will be inferred by extension from `pkgs` if of length one.

- type:

  character, indicating the type of package to download and install.
  Will be `"source"` except on Windows and some macOS builds: see the
  section on ‘Binary packages’ for those.

- ...:

  Other arguments passed on to
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).

## Value

Name of the downloaded file.
