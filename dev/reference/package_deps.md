# Find all dependencies of a CRAN or dev package.

Find all the dependencies of a package and determine whether they are
ahead or behind CRAN. A [`print()`](https://rdrr.io/r/base/print.html)
method identifies mismatches (if any) between local and CRAN versions of
each dependent package; an
[`update()`](https://rdrr.io/r/stats/update.html) method installs
outdated or missing packages from CRAN.

## Usage

``` r
package_deps(
  packages,
  dependencies = NA,
  repos = getOption("repos"),
  type = getOption("pkgType")
)

local_package_deps(pkgdir = ".", dependencies = NA)

dev_package_deps(
  pkgdir = ".",
  dependencies = NA,
  repos = getOption("repos"),
  type = getOption("pkgType"),
  remote_precedence = TRUE,
  additional_repositories = TRUE
)

# S3 method for class 'package_deps'
update(
  object,
  dependencies = NA,
  upgrade = c("default", "ask", "always", "never"),
  force = FALSE,
  quiet = FALSE,
  build = TRUE,
  build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"),
  build_manual = FALSE,
  build_vignettes = FALSE,
  repos = getOption("repos"),
  type = getOption("pkgType"),
  ...
)
```

## Arguments

- packages:

  A character vector of package names.

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

- repos:

  A character vector giving repositories to use.

- type:

  Type of package to `update`.

- pkgdir:

  Path to a package directory, or to a package tarball.

- remote_precedence:

  A logical flag specifying whether remote sources should take
  precedence over CRAN when both were found.

- additional_repositories:

  A logical flag specifying whether `Additional_repositories` should be
  extracted from the DESCRIPTION and appended to `repos`.

- object:

  A `package_deps` object.

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

  Additional arguments passed to `install_packages`.

## Value

A `data.frame` with columns:

|  |  |
|----|----|
| `package` | The dependent package's name, |
| `installed` | The currently installed version, |
| `available` | The version available on CRAN, |
| `diff` | An integer denoting whether the locally installed version of the package is newer (1), the same (0) or older (-1) than the version currently available on CRAN. |

## Examples

``` r
if (FALSE) { # \dontrun{
package_deps("devtools")
# Use update to update any out-of-date dependencies
update(package_deps("devtools"))
} # }
```
