# Install a package from a git repository

It is vectorised so you can install multiple packages with a single
command. You do not need to have the `git2r` package, or an external git
client installed.

## Usage

``` r
install_git(
  url,
  subdir = NULL,
  ref = NULL,
  branch = NULL,
  credentials = git_credentials(),
  git = c("auto", "git2r", "external"),
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

- url:

  Location of package. The url should point to a public or private
  repository.

- subdir:

  A sub-directory within a git repository that may contain the package
  we are interested in installing.

- ref:

  Name of branch, tag or SHA reference to use, if not HEAD.

- branch:

  Deprecated, synonym for ref.

- credentials:

  A git2r credentials object passed through to clone. Supplying this
  argument implies using `git2r` with `git`.

- git:

  Whether to use the `git2r` package, or an external git client via
  system. Default is `git2r` if it is installed, otherwise an external
  git installation.

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

- repos:

  A character vector giving repositories to use.

- type:

  Type of package to `update`.

- ...:

  Other arguments passed on to
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).

## Details

If you need to set git credentials for use in the `Remotes` field you
can do so by placing the credentials in the `remotes.git_credentials`
global option.

## See also

Other package installation:
[`install_bioc()`](https://remotes.r-lib.org/dev/reference/install_bioc.md),
[`install_bitbucket()`](https://remotes.r-lib.org/dev/reference/install_bitbucket.md),
[`install_cran()`](https://remotes.r-lib.org/dev/reference/install_cran.md),
[`install_dev()`](https://remotes.r-lib.org/dev/reference/install_dev.md),
[`install_github()`](https://remotes.r-lib.org/dev/reference/install_github.md),
[`install_gitlab()`](https://remotes.r-lib.org/dev/reference/install_gitlab.md),
[`install_local()`](https://remotes.r-lib.org/dev/reference/install_local.md),
[`install_svn()`](https://remotes.r-lib.org/dev/reference/install_svn.md),
[`install_url()`](https://remotes.r-lib.org/dev/reference/install_url.md),
[`install_version()`](https://remotes.r-lib.org/dev/reference/install_version.md)

## Examples

``` r
if (FALSE) { # \dontrun{
install_git("https://github.com/hadley/stringr.git")
install_git("https://github.com/hadley/stringr.git", ref = "stringr-0.2")
} # }
```
