# Install the development version of a package

`install_dev()` retrieves the package DESCRIPTION from the CRAN mirror
and looks in the 'URL' and 'BugReports' fields for GitHub, GitLab or
Bitbucket URLs. It then calls the appropriate `install_()` function to
install the development package.

## Usage

``` r
install_dev(package, cran_url = getOption("repos")[["CRAN"]], ...)
```

## Arguments

- package:

  The package name to install.

- cran_url:

  The URL of the CRAN mirror to use, by default based on the 'repos'
  option. If unset uses 'https://cloud.r-project.org'.

- ...:

  Additional arguments passed to
  [`install_github()`](https://remotes.r-lib.org/dev/reference/install_github.md),
  [`install_gitlab()`](https://remotes.r-lib.org/dev/reference/install_gitlab.md),
  or
  [`install_bitbucket()`](https://remotes.r-lib.org/dev/reference/install_bitbucket.md)
  functions.

## See also

Other package installation:
[`install_bioc()`](https://remotes.r-lib.org/dev/reference/install_bioc.md),
[`install_bitbucket()`](https://remotes.r-lib.org/dev/reference/install_bitbucket.md),
[`install_cran()`](https://remotes.r-lib.org/dev/reference/install_cran.md),
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
# From GitHub
install_dev("dplyr")

# From GitLab
install_dev("iemiscdata")

# From Bitbucket
install_dev("argparser")
} # }
```
