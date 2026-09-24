# Install a package directly from Bitbucket

This function is vectorised so you can install multiple packages in a
single command.

## Usage

``` r
install_bitbucket(
  repo,
  ref = "HEAD",
  subdir = NULL,
  auth_user = bitbucket_user(),
  password = bitbucket_password(),
  host = "api.bitbucket.org/2.0",
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

  Repository address in the format `username/repo[/subdir][@ref]`.
  Alternatively, you can specify `subdir` and/or `ref` using the
  respective parameters (see below); if both are specified, the values
  in `repo` take precedence.

- ref:

  Desired git reference; could be a commit, tag, or branch name.
  Defaults to HEAD.

- subdir:

  Subdirectory within repo that contains the R package.

- auth_user:

  your account username if you're attempting to install a package hosted
  in a private repository (and your username is different to
  `username`). Defaults to the `BITBUCKET_USER` environment variable.

- password:

  your password. Defaults to the `BITBUCKET_PASSWORD` environment
  variable. See details for further information on setting up a
  password.

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

To install from a private repo, or more generally, access the Bitbucket
API with your own credentials, you will need to get an access token. You
can create an access token following the instructions found in the
[Bitbucket App Passwords
documentation](https://support.atlassian.com/bitbucket-cloud/docs/app-passwords/).
The App Password requires read-only access to your repositories and pull
requests. Then store your password in the environment variable
`BITBUCKET_PASSWORD` (e.g. `evelynwaugh:swordofhonour`)

Note that on Windows, authentication requires the "libcurl" download
method. You can set the default download method via the
`download.file.method` option:

    options(download.file.method = "libcurl")

In particular, if unset, RStudio sets the download method to "wininet".
To override this, you might want to set it to "libcurl" in your R
profile, see [base::Startup](https://rdrr.io/r/base/Startup.html). The
caveat of the "libcurl" method is that it does *not* set the system
proxies automatically, see "Setting Proxies" in
[`utils::download.file()`](https://rdrr.io/r/utils/download.file.html).

## See also

Bitbucket API docs:
<https://confluence.atlassian.com/bitbucket/use-the-bitbucket-cloud-rest-apis-222724129.html>

Other package installation:
[`install_bioc()`](https://remotes.r-lib.org/dev/reference/install_bioc.md),
[`install_cran()`](https://remotes.r-lib.org/dev/reference/install_cran.md),
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
install_bitbucket("sulab/mygene.r@default")
install_bitbucket("djnavarro/lsr")
} # }
```
