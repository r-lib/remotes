# Attempts to install a package from CRAN.

This function is vectorised on `pkgs` so you can install multiple
packages in a single command.

## Usage

``` r
install_cran(
  pkgs,
  repos = getOption("repos"),
  type = getOption("pkgType"),
  dependencies = NA,
  upgrade = c("default", "ask", "always", "never"),
  force = FALSE,
  quiet = FALSE,
  build = TRUE,
  build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
  build_manual = FALSE,
  build_vignettes = FALSE,
  ...
)
```

## Arguments

- pkgs:

  A character vector of packages to install.

- repos:

  A character vector giving repositories to use.

- type:

  Type of package to `update`.

- dependencies:

  Which dependencies do you want to check? Can be a character vector
  (selecting from "Depends", "Imports", "LinkingTo", "Suggests", or
  "Enhances"), or a logical vector.

  `TRUE` is shorthand for "Depends", "Imports", "LinkingTo" and
  "Suggests". `NA` is shorthand for "Depends", "Imports" and "LinkingTo"
  and is the default. `FALSE` is shorthand for no dependencies (i.e.
  just check this package, not its dependencies).

  The value "soft" means the same as `TRUE`, "hard" means the same as
  `NA`.

  You can also specify dependencies from one or more additional fields,
  common ones include:

  - Config/Needs/website - for dependencies used in building the pkgdown
    site.

  - Config/Needs/coverage for dependencies used in calculating test
    coverage.

- upgrade:

  Should package dependencies be upgraded? One of "default", "ask",
  "always", or "never". "default" respects the value of the
  `R_REMOTES_UPGRADE` environment variable if set, and falls back to
  "ask" if unset. "ask" prompts the user for which out of date packages
  to upgrade. For non-interactive sessions "ask" is equivalent to
  "always". `TRUE` and `FALSE` are also accepted and correspond to
  "always" and "never" respectively.

- force:

  Force installation, even if the remote state has not changed since the
  previous install.

- quiet:

  If `TRUE`, suppress output.

- build:

  If `TRUE` build the package before installing.

- build_opts:

  Options to pass to `R CMD build`, only used when `build` is `TRUE`.

- build_manual:

  If `FALSE`, don't build PDF manual ('–no-manual').

- build_vignettes:

  If `FALSE`, don't build package vignettes ('–no-build-vignettes').

- ...:

  Other arguments passed on to
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).

## See also

Other package installation:
[`install_bioc()`](https://remotes.r-lib.org/dev/reference/install_bioc.md),
[`install_bitbucket()`](https://remotes.r-lib.org/dev/reference/install_bitbucket.md),
[`install_dev()`](https://remotes.r-lib.org/dev/reference/install_dev.md),
[`install_github()`](https://remotes.r-lib.org/dev/reference/install_github.md),
[`install_gitlab()`](https://remotes.r-lib.org/dev/reference/install_gitlab.md),
[`install_git()`](https://remotes.r-lib.org/dev/reference/install_git.md),
[`install_local()`](https://remotes.r-lib.org/dev/reference/install_local.md),
[`install_svn()`](https://remotes.r-lib.org/dev/reference/install_svn.md),
[`install_url()`](https://remotes.r-lib.org/dev/reference/install_url.md),
[`install_version()`](https://remotes.r-lib.org/dev/reference/install_version.md)

## Examples

``` r
if (FALSE) { # \dontrun{
install_cran("ggplot2")
install_cran(c("httpuv", "shiny"))
} # }
```
