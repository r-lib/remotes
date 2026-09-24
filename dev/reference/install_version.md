# Install specific version of a package.

This function knows how to look in multiple CRAN-like package
repositories, and in their `archive` directories, in order to find
specific versions of the requested package.

## Usage

``` r
install_version(
  package,
  version = NULL,
  dependencies = NA,
  upgrade = c("default", "ask", "always", "never"),
  force = FALSE,
  quiet = FALSE,
  build = FALSE,
  build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
  build_manual = FALSE,
  build_vignettes = FALSE,
  repos = getOption("repos"),
  type = "source",
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

- dependencies:

  logical indicating whether to also install uninstalled packages which
  these packages depend on/link to/import/suggest (and so on
  recursively). Not used if `repos = NULL`. Can also be a character
  vector, a subset of
  `c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")`.

  Only supported if `lib` is of length one (or missing), so it is
  unambiguous where to install the dependent packages. If this is not
  the case it is ignored, with a warning.

  The default, `NA`, means `c("Depends", "Imports", "LinkingTo")`.

  `TRUE` means to use `c("Depends", "Imports", "LinkingTo", "Suggests")`
  for `pkgs` and `c("Depends", "Imports", "LinkingTo")` for added
  dependencies: this installs all the packages needed to run `pkgs`,
  their examples, tests and vignettes (if the package author specified
  them correctly).

  In all of these, `"LinkingTo"` is omitted for binary packages.

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

  logical: if true, reduce the amount of output. This is *not* passed to
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)
  in case that is called, on purpose.

- build:

  If `TRUE` build the package before installing.

- build_opts:

  Options to pass to `R CMD build`, only used when `build` is `TRUE`.

- build_manual:

  If `FALSE`, don't build PDF manual ('–no-manual').

- build_vignettes:

  If `FALSE`, don't build package vignettes ('–no-build-vignettes').

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

## Details

The repositories are searched in the order specified by the `repos`
argument. This enables teams to maintain multiple in-house repositories
with different policies - for instance, one repo for development
snapshots and one for official releases. A common setup would be to
first search the official release repo, then the dev snapshot repo, then
a public CRAN mirror.

Older versions of packages on CRAN are usually only available in source
form. If your requested package contains compiled code, you will need to
have an R development environment installed. You can check if you do by
running `devtools::has_devel` (you need the `devtools` package for
this).

## See also

Other package installation:
[`install_bioc()`](https://remotes.r-lib.org/dev/reference/install_bioc.md),
[`install_bitbucket()`](https://remotes.r-lib.org/dev/reference/install_bitbucket.md),
[`install_cran()`](https://remotes.r-lib.org/dev/reference/install_cran.md),
[`install_dev()`](https://remotes.r-lib.org/dev/reference/install_dev.md),
[`install_github()`](https://remotes.r-lib.org/dev/reference/install_github.md),
[`install_gitlab()`](https://remotes.r-lib.org/dev/reference/install_gitlab.md),
[`install_git()`](https://remotes.r-lib.org/dev/reference/install_git.md),
[`install_local()`](https://remotes.r-lib.org/dev/reference/install_local.md),
[`install_svn()`](https://remotes.r-lib.org/dev/reference/install_svn.md),
[`install_url()`](https://remotes.r-lib.org/dev/reference/install_url.md)

## Examples

``` r
if (FALSE) { # \dontrun{
install_version("devtools", "1.11.0")
install_version("devtools", ">= 1.12.0, < 1.14")

## Specify search order (e.g. in ~/.Rprofile)
options(repos = c(
  prod = "http://mycompany.example.com/r-repo",
  dev = "http://mycompany.example.com/r-repo-dev",
  CRAN = "https://cran.revolutionanalytics.com"
))
install_version("mypackage", "1.15") # finds in 'prod'
install_version("mypackage", "1.16-39487") # finds in 'dev'
} # }
```
