# Attempts to install a package directly from GitHub.

This function is vectorised on `repo` so you can install multiple
packages in a single command.

## Usage

``` r
install_github(
  repo,
  ref = "HEAD",
  subdir = NULL,
  auth_token = github_pat(quiet),
  host = "api.github.com",
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

- repo:

  Repository address in the format
  `username/repo[/subdir][@ref|#pull|@*release]`. Alternatively, you can
  specify `subdir` and/or `ref` using the respective parameters (see
  below); if both are specified, the values in `repo` take precedence.

- ref:

  Desired git reference. Could be a commit, tag, or branch name, or a
  call to
  [`github_pull()`](https://remotes.r-lib.org/dev/reference/github_refs.md)
  or
  [`github_release()`](https://remotes.r-lib.org/dev/reference/github_refs.md).
  Defaults to `"HEAD"`, which means the default branch on GitHub and for
  git remotes. See
  [setting-the-default-branch](https://help.github.com/en/github/administering-a-repository/setting-the-default-branch)
  for more details.

- subdir:

  Subdirectory within repo that contains the R package.

- auth_token:

  To install from a private repo, generate a personal access token (PAT)
  with at least repo scope in <https://github.com/settings/tokens> and
  supply to this argument. This is safer than using a password because
  you can easily delete a PAT without affecting any others. Defaults to
  the `GITHUB_PAT` environment variable.

- host:

  GitHub API host to use. Override with your GitHub enterprise hostname,
  for example, `"github.hostname.com/api/v3"`.

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

If the repository uses submodules a command-line git client is required
to clone the submodules.

## See also

[`github_pull()`](https://remotes.r-lib.org/dev/reference/github_refs.md)

Other package installation:
[`install_bioc()`](https://remotes.r-lib.org/dev/reference/install_bioc.md),
[`install_bitbucket()`](https://remotes.r-lib.org/dev/reference/install_bitbucket.md),
[`install_cran()`](https://remotes.r-lib.org/dev/reference/install_cran.md),
[`install_dev()`](https://remotes.r-lib.org/dev/reference/install_dev.md),
[`install_gitlab()`](https://remotes.r-lib.org/dev/reference/install_gitlab.md),
[`install_git()`](https://remotes.r-lib.org/dev/reference/install_git.md),
[`install_local()`](https://remotes.r-lib.org/dev/reference/install_local.md),
[`install_svn()`](https://remotes.r-lib.org/dev/reference/install_svn.md),
[`install_url()`](https://remotes.r-lib.org/dev/reference/install_url.md),
[`install_version()`](https://remotes.r-lib.org/dev/reference/install_version.md)

## Examples
