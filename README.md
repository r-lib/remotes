
# remotes

> Install R Packages from remote or local repositories, 
> including GitHub, GitLab, Bitbucket, and Bioconductor


<!-- badges: start -->
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Linux Build Status](https://travis-ci.org/r-lib/remotes.svg?branch=master)](https://travis-ci.org/r-lib/remotes)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/r-lib/remotes?svg=true)](https://ci.appveyor.com/project/gaborcsardi/remotes)
[![](https://www.r-pkg.org/badges/version/remotes)](https://www.r-pkg.org/pkg/remotes)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/remotes)](https://www.r-pkg.org/pkg/remotes)
[![Coverage Status](https://img.shields.io/codecov/c/github/r-lib/remotes/master.svg)](https://codecov.io/github/r-lib/remotes?branch=master)
<!-- badges: end -->

Download and install R packages stored in GitHub, GitLab, Bitbucket, 
Bioconductor, or plain subversion or git repositories. This package
is a lightweight replacement of the `install_*` functions in
[`devtools`](https://github.com/r-lib/devtools).
Indeed most of the code was copied over from `devtools`.

## Features

* Installers:
  * Install packages with their dependencies.
  * Install from GitHub, GitLab, Bitbucket.
  * Install from git and subversion repositories.
  * Install from local files or URLs.
  * Install the dependencies of a local package tree.
  * Install specific package versions from CRAN.
* Supports [Bioconductor](https://bioconductor.org/) packages.
* Supports the `Remotes` field in `DESCRIPTION`. See more in the
  [dependencies](https://github.com/r-lib/remotes/blob/master/vignettes/dependencies.Rmd) vignette.
* Supports the `Additional_repositories` field in `DESCRIPTION`.
* Can install itself from GitHub (see below).
* Does not depend on other R packages.
* Does not contain compiled code, so no compiler is needed.
* Does not need any external software (for most of the functionality
  at least).

## Installation

Install the released version of remotes from CRAN:

```r
install.packages("remotes")
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
# build = FALSE because of some specificities of XGBoost package
install_github("dmlc/xgboost/R-package", build = FALSE)
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
[vignette](https://remotes.r-lib.org/articles/dependencies.html).

#### Additional repositories

remotes supports the `Additional_repositories` field in
`DESCRIPTION`. This is a way to specify dependencies from non-CRAN
package repositories. See the [Writing R extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies)
manual for details.

#### Bioconductor packages

Bioconductor packages are automatically detected and their
dependencies are installed from Bioconductor.

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

If you set the environment variable `R_REMOTES_STANDALONE="true"` (e.g.
in R `Sys.setenv(R_REMOTES_STANDALONE="true")`) you can force remotes to
operate in standalone mode and use only its internal R implementations. This
will allow successful installation of these packages.

### Options

remotes uses the following standard R options, see `?options` for their
details:

* `download.file.method` for the default download method. See
  `?download.file`.

* `pkgType` for the package type (source or binary, see manual) to install,
  download or look up dependencies for.

* `repos` for the locations of the user's standard CRAN(-like) repositories.

It also uses some remotes specific options:

* `BioC_git` for the URL of the default Bioconductor git mirror.

* `BioC_mirror` for the URL of the Bioconductor mirror.

* `unzip` for the path of the external `unzip` program.

### Environment variables

* The `BITBUCKET_USER` and `BITBUCKET_PASSWORD` environment variables
  are used for the default Bitbucket  user name and password, in
  `install_bitbucket()`

* The `GITHUB_PAT` environment variable is used as the default GitHub
  personal access token for all GitHub API queries.

* The `R_BIOC_MIRROR` environment variable can be used to specify an
  alternative Bioconductor mirror. (The `BioC_mirror` option takes
  precedence over this.)

* The `R_BIOC_VERSION` environment variable can be used to force a
  Bioconductor version.

* The `R_REMOTES_UPGRADE` environment variable can be used to set a default
  preferred value for the `upgrade =` argument accepted by the various
  `install_*()` functions. For example, you can set `R_REMOTES_UPGRADE="always"`
  to upgrade dependent packages without asking the user.

* Setting `R_REMOTES_STANDALONE="true"` forces remotes to work in standalone
  mode and avoid loading its optional dependencies (curl, git2 and pkgbuild
  currently. See "Standalone mode" above.

* Setting `R_REMOTES_NO_ERRORS_FROM_WARNINGS="true"` avoids stopping the
  installation for warning messages. Warnings usually mean installation
  errors, so by default remotes stops for a warning. However, sometimes
  other warnings might happen, that could be ignored by setting this
  environment variable.

* Setting `_R_CHECK_FORCE_SUGGESTS_="false"` while
  `R_REMOTES_NO_ERRORS_FROM_WARNINGS` is unset will also avoid stopping the
  installation for error messages. This is done because a warning is generated
  during installation when not all Suggested packages are not available.

## License

GPL (>= 2) © Mango Solutions, RStudio
