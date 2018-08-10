
* Submodules are now installed if they exist and a git client is available
  (#138, #133, #103, #82).

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
