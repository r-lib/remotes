# remotes 2.0.2

* `install_deps()` now installs un-installed remotes packages even when
  `upgrade = "never"` (@ankane, #227)

* `install_version()` now removes metadata added as a byproduct of using
  `install_url()` internally() (#224)

* `install()` now avoids converting warnings to errors if
  `R_REMOTES_NO_ERRORS_FROM_WARNINGS` is unset and
  `_R_CHECK_FORCE_SUGGESTS_=false`. This avoids failures due to Suggested
  packages potentially being missing.

* `install_bitbucket()` now works properly with packages in subdirectories
  (#220)

* `install_deps()` now installs un-installed packages even when `upgrade =
  "never"` (#218)

# remotes 2.0.1

* `install_github()` now bundles a GitHub PAT which is used on Travis to avoid
  hitting the rate limit too frequently. It also contains a more informative
  error message when the rate limit is hit on how to setup a GitHub personal
  access token.

* The dialog when `upgrade = "ask"` now has a 'CRAN only' option, to update
  only CRAN packages.

* No longer include project specific .Rprofile code in the temporary .Rprofile when
  `R_REMOTES_NO_ERRORS_FROM_WARNINGS=false` (the default).

* `update.package_deps()` no longer prompts to install uninstalled
  dependencies, they are always installed (#203).

* `available_packages()`, `available_packages_set()` and
  `available_packges_reset()` added to allow caching of the
  `available.packages()` database.

# remotes 2.0.0

## Breaking changes

* `install_github()`'s previously deprecated `username` argument has been
  removed. (#142)

* `install_deps()`'s `threads` argument has been removed, use the `Ncpus`
  argument instead (#153, #154)

* `install_git()`'s `branch` argument has been renamed to `ref` and `branch`
  has been deprecated.

## New features

* remotes now builds packages by default before installing them. This step
  uses the pkgbuild package, if avilable. If not, it calls `R CMD build`
  directly.

* New `install_dev()` to install the development version of a CRAN package,
  based on the URL and BugReports fields in the DESCRIPTION file (#144).

* `install_()*` functions now temporally put Rtools on the PATH when necessary,
  as long as the pkgbuild package is installed.

* remotes can be forced to use only its internal code by setting the
  environment variable `R_REMOTES_STANDALONE` = "true". This is useful when
  installing optional dependencies of remotes on Windows, such as curl or git2r
  (#147)

* When installing, remotes now errors on warnings, to catch cases
  where packages are only partially installed. This often happens on
  windows when the package dll is opened in another R process (#113).

* `install_()` functions now pass arguments, including authentication
  information and upgrade down to dependencies (#53, #86, #87).

* `install_()` functions allow the seclection of a subset of packages to
  upgrade, in interactive mode, when `upgrade = "ask"`.

* `install_git()` now supports passing credentials, when it is used with
  `git = "git2r"` (#106)

* `install_()` functions now return the name of the package(s) which were
  installed (#55).

* git submodules are now installed if they exist and a git client is
  available (#138, #133, #103, #82).

* New `install_gitlab()` and `install_bioc()` functions, to install
  `gitlab` and  `bioc` remote types.

* remotes now uses the same SHA updating logic for remotes as devtools,
  including checking if the SHA of the remote has changed since the last
  istallation. (#135)

* `install_url()` can now install package binaries on windows
  (r-lib/devtools#1765)

## Minor improvements and fixes

* `install_deps()` et al. now do not rewrite the `type` argument from `both`
  to `binary` to allow falling back to `source`. This fixes various
  installation failures.

* remotes now looks up GitHub package names locally, if possible, and
  uses the GitHub REST API (if the curl package is available, and not in
  standalone mode). This makes the remote lookup about 10x faster when the
  remote package has not changed since the last install.

* Using a GITHUB_PAT no longer prints diagnostic messages by
  default (r-lib/devtools#1752).

* remotes now always uses https URLs for R versions that support them
  (@ankane, #139)

* Do not include the BioCextra repository in versions after it was deprecated
  (R 3.5+, Bioc 3.6+).

* `install_()` functions now download tarballs (.tar.gz) files rather than zip
  archives (.zip). This results in generally smaller files and avoids issues
  with script permissions being lost and strange behavior of some external
  unzip programs on Windows (#96).

* Dependency parsing is now more robust to whitespace around the dependency
  specifications (#73).

* `standardise_dep()` exported, for use in devtools.

* `install_local()` now defaults to the current directory.

* `install_bitbucket()` now correctly supports authentication, and the
  `subdir` argument.

* `install_()` functions give a helpful warning when the package has long
  path names, on Windows. In this case building the package usually fails.
  (#84, #178).

* `install_()` functions have now a more robust way of handling various
  tar programs on Windows (#172).

* `install_()` functions now give a helpful warning on older R versions,
  on Windows, if `R.home()` contains a space character. Installation
  usually fails in this case.

* GitHub API errors now give better error messages, including data about
  the API rate limits.

# remotes 1.1.1

* Accept HTTPS, SSH, or various browser URLs in GitHub repo specification,
  @jennybc, #90, #109, #112

# remotes 1.1.0

* URL encode GitHub references, to allow installing from non-alphanumeric
  branch or tags, @krlmlr #38

* Better cooperation with proxy servers, and better download method
  selection on Windows, @cderv, #45, #46

* `install_deps()` supports tar balls, #47

* Allow training slash in GitHub repo specification, #54

* Work around on some Linux systems, where unzip is set to the empty
  string, @HenrikBengtsson, #57

* Check for circular dependencies while installing, #31

* Updated BioConductor repo URLs for newer BioC versions

# remotes 1.0.0

First public release.
