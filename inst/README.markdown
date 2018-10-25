
# remotes

> Install R Packages from GitHub, BitBucket, or other local or remote
> repositories

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Linux Build Status](https://travis-ci.org/r-lib/remotes.svg?branch=master)](https://travis-ci.org/r-lib/remotes)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/r-lib/remotes?svg=true)](https://ci.appveyor.com/project/gaborcsardi/remotes)
[![](https://www.r-pkg.org/badges/version/remotes)](https://www.r-pkg.org/pkg/remotes)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/remotes)](https://www.r-pkg.org/pkg/remotes)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-lib/remotes/master.svg)](https://codecov.io/github/r-lib/remotes?branch=master)

Download and install R packages stored in GitHub,
BitBucket, or plain subversion or git repositories. This package
is a lightweight replacement of the `install_*` functions in
[`devtools`](https://github.com/r-lib/devtools).
Indeed most of the code was copied over from `devtools`.

## Features

* Installers:
    * Install packages with their dependencies.
    * Install from GitHub, GitLab, BitBucket.
	* Install from git and subversion repositories.
	* Install from local files or URLs.
	* Install the dependencies of a local package tree.
	* Install specific package versions from CRAN.
* Supports [BioConductor](https://bioconductor.org/) packages.
* Supports the `Remotes` field in `DESCRIPTION`. See more
  [here](https://github.com/r-lib/remotes/blob/master/vignettes/dependencies.Rmd).
* Supports the `Additional_repositories` field in `DESCRIPTION`.
* Can install itself from GitHub (see below).
* Does not depend on other R packages.
* Does not contain compiled code, so no compiler is needed.
* Does not need any external software (for most of the functionality
  at least).

## Installation

Install the relesed version of remotes from CRAN:

```r
install.packages("remotes")
```

You can also install remotes from GitHub. If you already have a previous
version of remotes installed, you can use that to install the
development version:

```r
remotes::install_github("r-lib/remotes")
```

Alternatively, you can also call the supplied `install-github.R` file
directly, from within R:

```r
source("https://raw.githubusercontent.com/r-lib/remotes/master/install-github.R")$value("r-lib/remotes")
```

The <https://install-github.me> service is also based on remotes.
You can use it to install any R package from GitHub via sourcing a URL.
E.g. to install remotes itself:

```r
source("https://install-github.me/r-lib/remotes")
```

## Usage

Note that most of the examples here use GitHub. See below for other
supported repository types.

To install the latest version of a package in the `master` branch from
GitHub, you can use the `user/repo` form. Note that `user` can also be
an organization:

```r
remotes::install_github("r-lib/conflicted")
```

If the R package is inside a subdirectory of the root directory,
then give this subdirectory as well:

```r
remotes::install_github("dmlc/xgboost/R-package")
```

To install a certain branch or commit or tag, append it to the
repo name, after an `@`:

```r
remotes::install_github("gaborcsardi/pkgconfig@v2.0.0")
```

To install the latest release, append `@*release` to the repo
name:

```r
remotes::install_github("gaborcsardi/pkgconfig@*release")
```

To install a pull request, append `#` and the id (an integer number)
of the pull request to the repo name:

```r
remotes::install_github("r-lib/pkgconfig#7")
```

### Dependencies

Dependencies are automatically installed from CRAN. By default,
outdated dependencies are automatically upgraded. In interactive sessions
you can select a subset of the dependencies to upgrade.

#### Dependencies on GitHub

It is also possible to install dependencies from GitHub or other
supported repositories. For this you need to add a `Remotes` field to the
`DESCRIPTION` file. Its format is:
```
Remotes: [remote::]repo_spec, [remote::]repo_spec, ...
```
where `repo_spec` is any repository specification the corresponding
`install_()` function can handle. If `remote::` is missing, `github::` is
assumed. Other possible values: `gitlab::`,`bitbucket::`, `git::`, `local::`,
`svn::`, `url::`, `version::`, `cran::`, `bioc::`.

See more about the `Remotes` field in this
[vignette](https://github.com/r-lib/remotes/blob/master/vignettes/dependencies.Rmd).

#### Additional repositories

remotes supports the `Additional_repositories` field in
`DESCRIPTION`. This is a way to specify dependencies from non-CRAN
package repositories. See the [Writing R extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies)
manual for details.

#### BioConductor packages

BioConductor packages are automatically detected and their
dependencies are installed from BioConductor. The BiocInstaller
package, which is needed to install them, is also automatically
installed temporarily.

#### Currently supported remote types

* GitHub repositories via `install_github`.
* Bitbucket repositories via `install_bitbucket`.
* Generic git repositories via `install_git`. They need either a
  system git installation, or the
  [git2r](https://github.com/ropensci/git2r) R package.
* Local directories or package archive files via `install_local`.
* Remote package archive files via `install_url`.
* Packages in subversion repositories via `install_svn`. They need
  a system subversion installation.
* Specific package versions from CRAN or other CRAN-like
  repositories via `install_version`. This includes outdated
  and archived packages as well.
* All dependencies of a package in a local directory via
  `install_deps`.

### Download methods

* For R older than 3.2, the curl package is required as remotes falls back
  to `curl::curl_download` in that case
* For R newer than 3.3, default `download.file()` method is used.
  (`method = "auto"`)
* For in between versions,
    * `method = "wininet"` is used on windows OS
    * `method = "libcurl"` is used on other OS, if available.

See `help("download.file")` for informations on these methods and for
setting proxies if needed.

### Standalone mode

remotes will use the curl, git2r and pkgbuild packages if they are
installed to provide faster implementations for some aspects of the install
process. However if you are using remotes to install or update these packages
(or their reverse dependencies) using them during installation may fail
(particularly on Windows).

If you set the environment variable `R_REMOTES_STANDALONE=true` (e.g.
in R `Sys.setenv(R_REMOTES_STANDALONE="true")`) you can force remotes to
operate in standalone mode and use only its internal R implementations. This
will allow successful installation of these packages.

### Environment variables

* Setting `R_REMOTES_STANDALONE=true` forces remotes to work in standalone
  mode and avoid loading its optional dependencies (curl, git2 and pkgbuild
  currently. See "Standalone mode" above.

* Setting `R_REMOTES_NO_ERRORS_FROM_WARNINGS=true` avoids stopping the
  installation for warning messages. Warnings usually mean installation
  errors, so by default remotes stops for a warning. However, sometimes
  other warnings might happen, that could be ignored by setting this
  environment variable. 

* Setting `_R_CHECK_FORCE_SUGGESTS_=false` while
  `R_REMOTES_NO_ERRORS_FROM_WARNINGS` is unset will also avoid stopping the
  installation for error messages. This is done because a warning is generated
  during installation when not all Suggested packages are not available.

## License

GPL (>= 2) © Mango Solutions, RStudio
