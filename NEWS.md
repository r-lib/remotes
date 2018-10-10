# remotes 2.0.0.9000
## Breaking changes

* `install_github()`'s previously deprecated `username` argument has been
  removed. (#142)

* `install_deps()`'s `threads` argument has been removed, use the `Ncpus`
  argument instead (#153, #154)

* `install_git()`'s `branch` argument has been renamed to `ref` and `branch`
  has been deprecated.

## Features and bugfixes

* `install_dev()` added to install the development version of a CRAN package,
  based on the URL and BugReports fields in the DESCRIPTION file (#144)

* `install_()*` functions now temporally put Rtools on the PATH when necessary,
  as long as the pkgbuild package is installed.

* `standardise_dep()` exported, for use in devtools.

* Remotes can be forced to use only its internal code by setting the
  environment variable `R_REMOTES_STANDALONE` = "true". This is useful when
  installing optional dependencies of remotes on Windows, such as curl or git2r
  (#147)

* When installing set warnings to be errors, to catch cases where packages are
  only partially installed. This often happens on windows when the package dll
  is opened in another R process (#113).
  
* `install_()` functions now pass arguments, including authentication
  information and upgrade down to dependencies (#53, #86, #87).

* `install_git()` now supports passing credentials, when it is used with `git =
  "git2r"` (#106)

* `install_()` functions now return the name of the package(s) which were
  installed (#55).

* Dependency parsing is now more robust to whitespace around the dependency
  specifications (#73).

* `install_()` functions now download tarballs (.tar.gz) files rather than zip
  archives (.zip). This results in generally smaller files and avoids issues
  with script permissions being lost and strange behavior of some external
  unzip programs on Windows (#96).

* Do not include the BioCextra repository in versions after it was deprecated
  (R 3.5+, Bioc 3.6+).

* `remote_package_name.github_remote` and `remote_sha.github_remote` will
  lookup package name locally if possible and use faster REST APIs if the curl
  package is installed. This makes the remote lookup about 10x faster when the
  remote package has not changed since the last install.

* Submodules are now installed if they exist and a git client is available
  (#138, #133, #103, #82).
* remotes now always uses https URLs for R versions that support them (@ankane,
  #139)

* `install_gitlab()` and `install_bioc()` remote types added (#135)

* Code in remotes now uses the same SHA updating logic for remotes as devtools,
including checking if the SHA of the remote has changed since the last
installation. (#135)

* `github_pat()` and `gitlab_pat()` no longer print diagnostic messages by
  default (r-lib/devtools#1752).

* Fix skipping when installing from a full SHA (r-lib/devtools#1624)

* `install_url()` can now install package binaries on windows (r-lib/devtools#1765)

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
