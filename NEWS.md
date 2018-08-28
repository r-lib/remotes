# Breaking Changes

* `install_github()`'s previously deprecated `username` argument has been
  removed. (#142)

# Development

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

# 1.1.1

* Accept HTTPS, SSH, or various browser URLs in GitHub repo specification,
  @jennybc, #90, #109, #112

# 1.1.0

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

# 1.0.0

First public release.
