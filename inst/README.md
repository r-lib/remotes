
# remotes

> Install R Packages from 'GitHub', 'BitBucket', or other local or remote repositories

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Linux Build Status](https://travis-ci.org/MangoTheCat/remotes.svg?branch=master)](https://travis-ci.org/MangoTheCat/remotes)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/MangoTheCat/remotes?svg=true)](https://ci.appveyor.com/project/gaborcsardi/remotes)
[![](http://www.r-pkg.org/badges/version/remotes)](http://www.r-pkg.org/pkg/remotes)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/remotes)](http://www.r-pkg.org/pkg/remotes)
[![Coverage Status](https://img.shields.io/codecov/c/github/MangoTheCat/remotes/master.svg)](https://codecov.io/github/MangoTheCat/remotes?branch=master)

Download and install R packages stored in 'GitHub',
'BitBucket', or plain 'subversion' or 'git' repositories. This package
is a lightweight replacement of the 'install_*' functions in
[devtools](https://github.com/hadley/devtools).
Indeed most of the code was copied over from 'devtools'.

`remotes` can install itself from GitHub, it does not depend on any R
package, it does not contain compiled code, and does not need any
external software (for most of the functionality at least).

## Installation

You can install `remotes` from GitHub. If you already have a previous
version of `remotes` installed, you can use that to install the new
versions:

```r
remotes::install_github("mangothecat/remotes")
```

Otherwise you can call the supplied `install-github.R`
file directly, from within R:

```r
source("https://raw.githubusercontent.com/MangoTheCat/remotes/master/install-github.R")$value("mangothecat/remotes")
```

## Usage

Note that most of the examples here use GitHub. See below for other
supported repository types.

To install the latest version of a package in the `master` branch from
GitHub, you can use the `user/repo` form. Note that `user` can also be
an organization:

```r
remotes::install_github("mangothecat/franc")
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
remotes::install_github("mangothecat/pkgsnap#10")
```

### Dependencies

Dependencies are automatically installed from CRAN. By default,
outdated dependencies are automatically upgraded.

#### Dependencies on GitHub

It is also possible to install dependencies from GitHub. For this
you need to add a `Remotes` field to the `DESCRIPTION` file.
Its format is:
```
Remotes: [remote::]repo_spec, [remote::]repo_spec, ...
```
where `repo_spec` is any repository specification `install_github`
can handle. If `remote::` is missing, `github::` is assumed.
Other possible values: `bitbucket::`, `git::`, `local::`,
`svn::`, `url::`, `version::`.

#### Additional repositories

`remotes` supports the `Additional_repositories` field in
`DESCRIPTION`. This is a way to specify dependencies from non-CRAN
package repositories. See the [Writing R extensions]
(https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-Dependencies)
manual for details.

#### BioConductor packages

BioConductor packages are automatically detected and their
dependencies are installed from BioConductor. Note that you
need to have the `BiocInstaller` package installed to
install BioConductor packages with `remotes`.

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

### Notes

If a package in a git repository has submodules, then the installation will
likely fail. Nevertheless, a warning is given in this case.

## License

GPL (>= 2) © Mango Solutions, RStudio
