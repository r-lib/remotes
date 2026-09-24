# Changelog

## remotes (development version)

- Replaced `mockery::stub()` with
  [`testthat::local_mocked_bindings()`](https://testthat.r-lib.org/reference/local_mocked_bindings.html).
  Removed {mockery} as a suggested package
  ([\#804](https://github.com/r-lib/remotes/issues/804)).

## remotes 2.5.0

CRAN release: 2024-03-17

- [`install_github()`](https://remotes.r-lib.org/dev/reference/install_github.md)
  now uses credentials from the git credential store, if `GITHUB_PAT`
  and `GITHUB_TOKEN` are not set.
- The `Remotes` field in `DESCRIPTION` now accepts explicit package
  names: `<pkgname>=<type>::<username>/<repo>`
  ([\#719](https://github.com/r-lib/remotes/issues/719),
  [@heavywatal](https://github.com/heavywatal)).
- [`dev_package_deps()`](https://remotes.r-lib.org/dev/reference/package_deps.md)
  noq has a `remote_precedence` parameter which allows the user to
  choose whether remote sources should have a priority over CRAN sources
  ([\#732](https://github.com/r-lib/remotes/issues/732),
  [@maksymiuks](https://github.com/maksymiuks)).
- [`dev_package_deps()`](https://remotes.r-lib.org/dev/reference/package_deps.md)
  now works for packages with `Enhances` dependencies
  ([\#711](https://github.com/r-lib/remotes/issues/711),
  [@maksymiuks](https://github.com/maksymiuks)).
- [`dev_package_deps()`](https://remotes.r-lib.org/dev/reference/package_deps.md)
  now has an `additional_repositories` parameter which allows the user
  to choose whether `Additional_repositories` should be extracted from
  the `DESCRIPTION` file and appended to repos
  ([\#782](https://github.com/r-lib/remotes/issues/782),
  [@maksymiuks](https://github.com/maksymiuks)).
- [`install_dev()`](https://remotes.r-lib.org/dev/reference/install_dev.md)
  now ignores a trailing slash
  ([\#692](https://github.com/r-lib/remotes/issues/692),
  [@krlmlr](https://github.com/krlmlr)).
- System requirements now support Ubuntu 22.04.
- [`local_package_deps()`](https://remotes.r-lib.org/dev/reference/package_deps.md)
  now errors for non-existent directories
  ([\#772](https://github.com/r-lib/remotes/issues/772),
  [@MatthieuStigler](https://github.com/MatthieuStigler)).

## remotes 2.4.2

CRAN release: 2021-11-30

- Gábor Csárdi is now the maintainer.
- [`bioc_version()`](https://remotes.r-lib.org/dev/reference/bioc_install_repos.md)
  now points to the most recent (2021-10-27) Bioconductor release,
  `v3.14` ([@stufield](https://github.com/stufield),
  [\#664](https://github.com/r-lib/remotes/issues/664)).
- Fix regex to handle user names in URL in `git_remote`, add regression
  tests ([@achimgaedke](https://github.com/achimgaedke),
  [\#646](https://github.com/r-lib/remotes/issues/646)).

## remotes 2.4.1

CRAN release: 2021-09-29

- pkgbuild is no longer accidentally loaded even in standalone mode
  ([\#548](https://github.com/r-lib/remotes/issues/548))
- The internal GitHub token used to increase rate limits has been
  regenerated.
- Using `remote_package_name.git2r_remote` now passes credentials when
  looking up the package `DESCRIPTION`
  ([\#633](https://github.com/r-lib/remotes/issues/633),
  [@rnorberg](https://github.com/rnorberg))
- Using `remote_package_name.git2r_remote` and
  `remote_package_name.xgit_remote`, http responses returning an invalid
  `DESCRIPTION` or that redirect to another page will now fallback to
  return `NA` instead of throwing an error when trying to parse the
  unexpected content
  ([\#628](https://github.com/r-lib/remotes/issues/628),
  [@dgkf](https://github.com/dgkf)).
- Fix regex that breaks git protocol in `git_remote`
  ([@niheaven](https://github.com/niheaven)
  [\#630](https://github.com/r-lib/remotes/issues/630)).
- Clarify
  [`github_pull()`](https://remotes.r-lib.org/dev/reference/github_refs.md)
  documentation ([@ms609](https://github.com/ms609)
  [\#640](https://github.com/r-lib/remotes/issues/640)).

## remotes 2.4.0

CRAN release: 2021-06-02

- Re-license as MIT.
  ([\#551](https://github.com/r-lib/remotes/issues/551))

- Fix bug in install_bioc() when using version=‘devel’. The code will
  now pull from the git HEAD, not a branch named ‘HEAD’
  ([@bbimber](https://github.com/bbimber),
  [\#612](https://github.com/r-lib/remotes/issues/612)).

- skip tests for `download.file(method = "internal")`, on R \> 4.1,
  since that method is now defunct on those versions.

- [`system_requirements()`](https://remotes.r-lib.org/dev/reference/system_requirements.md)
  now works as intended if only the `os` argument is used
  ([@mdneuzerling](https://github.com/mdneuzerling),
  [\#609](https://github.com/r-lib/remotes/issues/609))

- `remote_package_name.git2r_remote` and
  `remote_package_name.xgit_remote` now get correct package name from
  HTTP(S) git repo’s `DESCRIPTION` file, and thus package’s
  `DESCRIPTION` file’s `Remotes` field could have
  `git::http(s)://<host>/<username>/<repo>[.git][@ref]` items that
  install remote packages using git via HTTP(S) protocol
  ([@niheaven](https://github.com/niheaven),
  [\#603](https://github.com/r-lib/remotes/issues/603)).

## remotes 2.3.0

CRAN release: 2021-04-01

### Major changes

- `install_*()` functions will no longer fail by default if there
  warnings from
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html).
  Concretely the default value of `R_REMOTES_NO_ERRORS_FROM_WARNINGS`
  has changed to `true` from the previous value of `false`.
  ([\#403](https://github.com/r-lib/remotes/issues/403))

### Minor improvements and fixes

- [`install_bioc()`](https://remotes.r-lib.org/dev/reference/install_bioc.md)
  now respects the environment variable R_BIOC_VERSION, and will use the
  git branch corresponding to this Bioconductor version
  ([@bbimber](https://github.com/bbimber),
  [\#580](https://github.com/r-lib/remotes/issues/580)).

- remotes without package names are now unconditionally installed
  ([\#532](https://github.com/r-lib/remotes/issues/532),
  [@jakubkovac](https://github.com/jakubkovac))

- It is now possible to specify a custom host for dependencies listed in
  the `DESCRIPTION` file with
  `Remotes: <type>[@host]::<username>/<repo>[@ref]`. The `ref` now
  supports `/` in it for `GitLab` repositories as it did for
  `GitHub`repositories. ([@dagola](https://github.com/dagola),
  [\#448](https://github.com/r-lib/remotes/issues/448))

- Internal `package2remote()` function now supports local remotes
  created by pak.

- `github_pat()` will now check if `GITHUB_TOKEN` is set if it cannot
  find `GITHUB_PAT`. ([@coatless](https://github.com/coatless))

- [`system_requirements()`](https://remotes.r-lib.org/dev/reference/system_requirements.md)
  now supports querying released packages as well as development
  dependencies ([\#545](https://github.com/r-lib/remotes/issues/545))

- [`system_requirements()`](https://remotes.r-lib.org/dev/reference/system_requirements.md)
  now supports OS name + version in the `os` argument
  ([\#549](https://github.com/r-lib/remotes/issues/549),
  [@krlmlr](https://github.com/krlmlr)).

## remotes 2.2.0

CRAN release: 2020-07-21

### New functions and features

- New
  [`system_requirements()`](https://remotes.r-lib.org/dev/reference/system_requirements.md)
  function to query the Public RStudio Package Manager for system
  requirements for a package (and its dependencies)

- Remotes functions can now install dependencies from additional
  DESCRIPTION fields, e.g. passing
  `dependencies = "Config/Needs/website"` will install the dependencies
  listed in the `Config/Needs/website:` field in the package’s
  DESCRIPTION. Prefixing fields with `Config/Needs` allows them to pass
  `R CMD check` without a NOTE, so it is the recommended format for
  these extra dependencies.

- `install_*()` family of functions now use the default branch in the
  repository, not the `master` branch
  ([@MyKo101](https://github.com/MyKo101),#508).

### Minor improvements and fixes

- Internal functions
  [`remote_download()`](https://remotes.r-lib.org/dev/reference/install_remote.md),
  [`remote_metadata()`](https://remotes.r-lib.org/dev/reference/install_remote.md),
  [`remote_package_name()`](https://remotes.r-lib.org/dev/reference/install_remote.md)
  and
  [`remote_sha()`](https://remotes.r-lib.org/dev/reference/install_remote.md)
  are now exported, so 3rd party packages could provide methods for new
  remote types ([\#509](https://github.com/r-lib/remotes/issues/509),
  [\#56](https://github.com/r-lib/remotes/issues/56))

- Internal functions
  [`add_metadata()`](https://remotes.r-lib.org/dev/reference/install_remote.md),
  [`github_remote()`](https://remotes.r-lib.org/dev/reference/github_remote.md)
  are now exported. They are mainly for 3rd party extensions and should
  not be used by most users
  ([\#485](https://github.com/r-lib/remotes/issues/485)).

- [`install_version()`](https://remotes.r-lib.org/dev/reference/install_version.md)
  now keeps searching subsequent repositories for the requested version,
  rather than failing if the version it finds in an early repository is
  unsuitable. ([\#305](https://github.com/r-lib/remotes/issues/305),
  [@kenahoo](https://github.com/kenahoo))

- [`install_version()`](https://remotes.r-lib.org/dev/reference/install_version.md)
  now understands specifications like ‘\>= 1.0’ or ‘\>= 1.12.0, \< 1.14’
  to install the first version of the package it can find that satisfies
  the criteria. ([\#305](https://github.com/r-lib/remotes/issues/305),
  [@kenahoo](https://github.com/kenahoo))

- [`install_version()`](https://remotes.r-lib.org/dev/reference/install_version.md)
  now avoids use of
  [`base::url()`](https://rdrr.io/r/base/connections.html), as prior to
  R 3.6.2 it had a bug when downloading large files
  ([\#463](https://github.com/r-lib/remotes/issues/463))

- `parse_submodules()` internal regular expression is now PCRE 2
  compatible ([\#502](https://github.com/r-lib/remotes/issues/502),
  [@jan-glx](https://github.com/jan-glx))

- [`update_packages()`](https://remotes.r-lib.org/dev/reference/update_packages.md)
  argument `force` has been deprecated and no longer has any effect
  ([\#521](https://github.com/r-lib/remotes/issues/521))

- Another fix for the mixed binary and source dependency issue, it
  should hopefully be fully squashed now
  ([\#296](https://github.com/r-lib/remotes/issues/296))

- The upgrade menu is now interruptible in RStudio
  ([\#489](https://github.com/r-lib/remotes/issues/489)).

- Internal GitHub functions now correctly handle cases when characters
  are not representable in the default locale, but are representable in
  UTF-8 ([\#492](https://github.com/r-lib/remotes/issues/492)).

## remotes 2.1.1

CRAN release: 2020-02-15

### Minor improvements and fixes

- Installing mixed binary and source dependencies when the latest
  versions of some packages do not have binaries yet should now install
  dependencies in the correct order to prevent load failures
  ([\#296](https://github.com/r-lib/remotes/issues/296))

- `github_error()` now also works when a GitHub (Enterprise) server does
  not return information about the rate limit
  ([@dpprdan](https://github.com/dpprdan),
  [\#396](https://github.com/r-lib/remotes/issues/396),
  [\#413](https://github.com/r-lib/remotes/issues/413)).

- `install_gitlab` passes the `quiet` argument on to `gitlab_pat`
  ([@michaelchirico](https://github.com/michaelchirico),
  [\#437](https://github.com/r-lib/remotes/issues/437))

- `remotes` is now resilient against installed packages that declare
  `RemoteType: standard` but do not include a `RemoteRepos` or
  `RemotePkgType` field. In such a case, the values for
  `getOption("repos")` and `getOption("pkgType")` will be used
  (respectively).

- [`install_gitlab()`](https://remotes.r-lib.org/dev/reference/install_gitlab.md)
  now installs from repositories in subgroups and with dots in their
  name. `subdir` is now an explicit argument instead of implicit in
  `repo` ([@robertdj](https://github.com/robertdj),
  [\#259](https://github.com/r-lib/remotes/issues/259),
  [\#420](https://github.com/r-lib/remotes/issues/420)).

- `install()` now passes the ellipsis `...` to
  [`install_deps()`](https://remotes.r-lib.org/dev/reference/install_deps.md)
  ([@Neil-Schneider](https://github.com/Neil-Schneider),
  [\#411](https://github.com/r-lib/remotes/issues/411))

- The tests have been updated to work with newer versions of callr and R
  4.0

## remotes 2.1.0

CRAN release: 2019-06-24

### New features

- `install_*()` functions gain `build_manual` and `build_vignette`
  arguments that previously existed in devtools versions \< 2.0
  ([\#353](https://github.com/r-lib/remotes/issues/353)).

- The interactive menu has been modified to provide more clear
  instructions on the skipping behavior
  ([\#207](https://github.com/r-lib/remotes/issues/207))

- Credentials are now passed via HTTP headers, to reduce exposure when
  requests fail ([\#391](https://github.com/r-lib/remotes/issues/391)).

### Minor improvements and fixes

- [`download()`](https://remotes.r-lib.org/dev/reference/download.md)
  with the external curl download method now always uses `-L` to follow
  redirects. ([\#350](https://github.com/r-lib/remotes/issues/350))

- [`update_packages()`](https://remotes.r-lib.org/dev/reference/update_packages.md)
  now has a more informative error message when the update fails
  ([\#223](https://github.com/r-lib/remotes/issues/223),
  [\#232](https://github.com/r-lib/remotes/issues/232))

- [`install_git()`](https://remotes.r-lib.org/dev/reference/install_git.md)
  now can take credentials from the global option
  `remotes.git_credentials`
  ([\#378](https://github.com/r-lib/remotes/issues/378)).

- [`install_git()`](https://remotes.r-lib.org/dev/reference/install_git.md)
  now works with SHA references and external git
  ([\#389](https://github.com/r-lib/remotes/issues/389)).

- GitHub remotes that point to branches no longer fail when the branch
  is later deleted
  ([\#274](https://github.com/r-lib/remotes/issues/274)).

- Local remotes whose original location has been moved will no longer
  error when updating
  ([\#370](https://github.com/r-lib/remotes/issues/370)).

- `update_deps()` no longer sorts the dependencies alphabetically
  ([\#296](https://github.com/r-lib/remotes/issues/296),
  [\#301](https://github.com/r-lib/remotes/issues/301))

- `github_resolve_ref()` now takes a `host` parameter
  ([\#284](https://github.com/r-lib/remotes/issues/284))

- Remotes specific environment variables now accept 0 and 1 as valid
  values ([\#238](https://github.com/r-lib/remotes/issues/238))

- remotes now uses locking by default when installing binary packages,
  which avoids issues when installing binaries that are already open in
  other R processes
  ([\#368](https://github.com/r-lib/remotes/issues/368))

- `update_deps()` no longer fails if a local package no longer exists
  ([\#289](https://github.com/r-lib/remotes/issues/289))

- [`install_version()`](https://remotes.r-lib.org/dev/reference/install_version.md)
  now errors with a more informative message when `type` is not ‘source’
  ([\#323](https://github.com/r-lib/remotes/issues/323))

- Bioc
  [`remote_sha()`](https://remotes.r-lib.org/dev/reference/install_remote.md)
  now always returns a character result
  ([\#379](https://github.com/r-lib/remotes/issues/379))

- Fix API call for private repositories in `install_gitlab`
  ([@aornugent](https://github.com/aornugent),
  [\#359](https://github.com/r-lib/remotes/issues/359),
  [\#363](https://github.com/r-lib/remotes/issues/363))

- git submodules now work if the submodule file is empty
  ([@muschellij2](https://github.com/muschellij2),
  [\#234](https://github.com/r-lib/remotes/issues/234))

- git submodules now work if the R package is stored in a subfolder
  ([@pommedeterresautee](https://github.com/pommedeterresautee),
  [\#233](https://github.com/r-lib/remotes/issues/233))

- [`install_gitlab()`](https://remotes.r-lib.org/dev/reference/install_gitlab.md)
  no longer adds the access token twice to the request
  ([@aornugent](https://github.com/aornugent),
  [\#363](https://github.com/r-lib/remotes/issues/363)).

- Bitbucket dependencies now actually use the `BITBUCKET_USER` and
  `BITBUCKET_PASSWORD` environment variables
  ([@antoine-sachet](https://github.com/antoine-sachet),
  [\#347](https://github.com/r-lib/remotes/issues/347)).

- `parse_deps()` now ignores trailing whitespaces around comparison
  operators in DESCRIPTION fields
  ([@LiNk-NY](https://github.com/LiNk-NY),
  [\#366](https://github.com/r-lib/remotes/issues/366))

## remotes 2.0.4

CRAN release: 2019-04-10

- `update.package_dependencies()` now uses the pkg_type for the cran
  remote rather than a global type attribute, fixing errors when this
  global attribute is lost
  ([\#291](https://github.com/r-lib/remotes/issues/291),
  [\#304](https://github.com/r-lib/remotes/issues/304)).

- Credentials are no longer passed to dependencies, as this breaks
  dependencies which use different credentials or hosts. If you have
  relied on this behavior a more robust way to provide the credentials
  is using the appropriate environment variables, e.g. `GITHUB_PAT`,
  `BITBUCKET_USER` etc.
  ([@antoine-sachet](https://github.com/antoine-sachet),
  [\#345](https://github.com/r-lib/remotes/issues/345)).

- The hash of bitbucket hosts is now correctly retrieved
  ([@antoine-sachet](https://github.com/antoine-sachet),
  [\#344](https://github.com/r-lib/remotes/issues/344))

- Fix parsing of Additional_Repositories which have a leading newline
  ([@tmelliott](https://github.com/tmelliott),
  [\#251](https://github.com/r-lib/remotes/issues/251)).

## remotes 2.0.3

CRAN release: 2019-04-09

- The order of choices for `upgrade = "ask"` now puts the stable ones
  ‘All’, ‘CRAN only’, ‘none’ first, so they always have the same numbers
  ([\#287](https://github.com/r-lib/remotes/issues/287)).

- `update_submodules()` now works with empty .gitmodules files
  ([@jsilve24](https://github.com/jsilve24),
  [\#329](https://github.com/r-lib/remotes/issues/329)).

- remotes now understands the “standard” remote type, as produced by
  packages installed from CRAN using `pak`
  ([\#309](https://github.com/r-lib/remotes/issues/309))

- [`install_dev()`](https://remotes.r-lib.org/dev/reference/install_dev.md)
  now supports ref/pull format, e.g. `install_dev('shiny@v1.2-rc')`
  ([@mkearney](https://github.com/mkearney),
  [\#279](https://github.com/r-lib/remotes/issues/279)).

- Fix return type of
  [`install_remote()`](https://remotes.r-lib.org/dev/reference/install_remote.md)
  when there is a circular dependency
  ([\#225](https://github.com/r-lib/remotes/issues/225))

- `remote_package_name.github_remote()` now works properly on Windows
  ([\#248](https://github.com/r-lib/remotes/issues/248))

- [`install_bioc()`](https://remotes.r-lib.org/dev/reference/install_bioc.md)
  repositories now updated for the Bioconductor 3.8 release.
  ([\#239](https://github.com/r-lib/remotes/issues/239))

- `install_*` functions now set the `R_LIBS*` environment variables for
  child processes correctly on Windows
  ([@HenrikBengtsson](https://github.com/HenrikBengtsson),
  [\#253](https://github.com/r-lib/remotes/issues/253))

- `install_*` functions now support the `R_REMOTES_UPGRADE` environment
  variable, to set the default for the `upgrade` argument. See README
  for details ([@kevinushey](https://github.com/kevinushey),
  [\#240](https://github.com/r-lib/remotes/issues/240)).

- `install_*` functions perform basic HTTP authentication using HTTP
  headers now. This fixes an issue with
  [`install_bitbucket()`](https://remotes.r-lib.org/dev/reference/install_bitbucket.md)
  and private repos
  ([\#255](https://github.com/r-lib/remotes/issues/255)).

- `install_*` functions now respect the `download.file.method` option,
  if `download_file()` is used for HTTP.

- `install_*` functions now use the *libcurl* method, if the
  `download.file.method` option is not set to a different one, and
  libcurl is available. Before, the *wininet* method was preferred on
  Windows. If you rely on the proxy configuration of *wininet*, then you
  might want to set the `download.file.method` option, or use another
  way to set up proxies, see
  [`?download.file`](https://rdrr.io/r/utils/download.file.html).

- Remotes without package names are now unconditionally installed
  ([\#246](https://github.com/r-lib/remotes/issues/246)).

- [`install_github()`](https://remotes.r-lib.org/dev/reference/install_github.md)
  now includes a more informative error message when the status code is
  404, asking the user to check that they have spelled the repo owner
  and repo correctly (included in the error message), and that they have
  the required permissions to access the repository.

- `install_*` functions (via the underlying private `install` function)
  now set `RGL_USE_NULL="TRUE"` in order to avoid errors when running
  headless and installing any package using `rgl`
  ([@jefferis](https://github.com/jefferis), \##333)

## remotes 2.0.2

CRAN release: 2018-10-30

- [`install_deps()`](https://remotes.r-lib.org/dev/reference/install_deps.md)
  now installs un-installed remotes packages even when
  `upgrade = "never"` ([@ankane](https://github.com/ankane),
  [\#227](https://github.com/r-lib/remotes/issues/227))

- [`install_version()`](https://remotes.r-lib.org/dev/reference/install_version.md)
  now removes metadata added as a byproduct of using
  [`install_url()`](https://remotes.r-lib.org/dev/reference/install_url.md)
  internally() ([\#224](https://github.com/r-lib/remotes/issues/224))

- `install()` now avoids converting warnings to errors if
  `R_REMOTES_NO_ERRORS_FROM_WARNINGS` is unset and
  `_R_CHECK_FORCE_SUGGESTS_=false`. This avoids failures due to
  Suggested packages potentially being missing.

- [`install_bitbucket()`](https://remotes.r-lib.org/dev/reference/install_bitbucket.md)
  now works properly with packages in subdirectories
  ([\#220](https://github.com/r-lib/remotes/issues/220))

- [`install_deps()`](https://remotes.r-lib.org/dev/reference/install_deps.md)
  now installs un-installed packages even when `upgrade = "never"`
  ([\#218](https://github.com/r-lib/remotes/issues/218))

## remotes 2.0.1

CRAN release: 2018-10-19

- [`install_github()`](https://remotes.r-lib.org/dev/reference/install_github.md)
  now bundles a GitHub PAT which is used on Travis to avoid hitting the
  rate limit too frequently. It also contains a more informative error
  message when the rate limit is hit on how to setup a GitHub personal
  access token.

- The dialog when `upgrade = "ask"` now has a ‘CRAN only’ option, to
  update only CRAN packages.

- No longer include project specific .Rprofile code in the temporary
  .Rprofile when `R_REMOTES_NO_ERRORS_FROM_WARNINGS=false` (the
  default).

- [`update.package_deps()`](https://remotes.r-lib.org/dev/reference/package_deps.md)
  no longer prompts to install uninstalled dependencies, they are always
  installed ([\#203](https://github.com/r-lib/remotes/issues/203)).

- [`available_packages()`](https://remotes.r-lib.org/dev/reference/available_packages.md),
  [`available_packages_set()`](https://remotes.r-lib.org/dev/reference/available_packages.md)
  and `available_packges_reset()` added to allow caching of the
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)
  database.

## remotes 2.0.0

CRAN release: 2018-10-11

### Breaking changes

- [`install_github()`](https://remotes.r-lib.org/dev/reference/install_github.md)’s
  previously deprecated `username` argument has been removed.
  ([\#142](https://github.com/r-lib/remotes/issues/142))

- [`install_deps()`](https://remotes.r-lib.org/dev/reference/install_deps.md)’s
  `threads` argument has been removed, use the `Ncpus` argument instead
  ([\#153](https://github.com/r-lib/remotes/issues/153),
  [\#154](https://github.com/r-lib/remotes/issues/154))

- [`install_git()`](https://remotes.r-lib.org/dev/reference/install_git.md)’s
  `branch` argument has been renamed to `ref` and `branch` has been
  deprecated.

### New features

- remotes now builds packages by default before installing them. This
  step uses the pkgbuild package, if available. If not, it calls
  `R CMD build` directly.

- New
  [`install_dev()`](https://remotes.r-lib.org/dev/reference/install_dev.md)
  to install the development version of a CRAN package, based on the URL
  and BugReports fields in the DESCRIPTION file
  ([\#144](https://github.com/r-lib/remotes/issues/144)).

- `install_()*` functions now temporally put Rtools on the PATH when
  necessary, as long as the pkgbuild package is installed.

- remotes can be forced to use only its internal code by setting the
  environment variable `R_REMOTES_STANDALONE` = “true”. This is useful
  when installing optional dependencies of remotes on Windows, such as
  curl or git2r ([\#147](https://github.com/r-lib/remotes/issues/147))

- When installing, remotes now errors on warnings, to catch cases where
  packages are only partially installed. This often happens on windows
  when the package dll is opened in another R process
  ([\#113](https://github.com/r-lib/remotes/issues/113)).

- `install_()` functions now pass arguments, including authentication
  information and upgrade down to dependencies
  ([\#53](https://github.com/r-lib/remotes/issues/53),
  [\#86](https://github.com/r-lib/remotes/issues/86),
  [\#87](https://github.com/r-lib/remotes/issues/87)).

- `install_()` functions allow the selection of a subset of packages to
  upgrade, in interactive mode, when `upgrade = "ask"`.

- [`install_git()`](https://remotes.r-lib.org/dev/reference/install_git.md)
  now supports passing credentials, when it is used with `git = "git2r"`
  ([\#106](https://github.com/r-lib/remotes/issues/106))

- `install_()` functions now return the name of the package(s) which
  were installed ([\#55](https://github.com/r-lib/remotes/issues/55)).

- git submodules are now installed if they exist and a git client is
  available ([\#138](https://github.com/r-lib/remotes/issues/138),
  [\#133](https://github.com/r-lib/remotes/issues/133),
  [\#103](https://github.com/r-lib/remotes/issues/103),
  [\#82](https://github.com/r-lib/remotes/issues/82)).

- New
  [`install_gitlab()`](https://remotes.r-lib.org/dev/reference/install_gitlab.md)
  and
  [`install_bioc()`](https://remotes.r-lib.org/dev/reference/install_bioc.md)
  functions, to install `gitlab` and `bioc` remote types.

- remotes now uses the same SHA updating logic for remotes as devtools,
  including checking if the SHA of the remote has changed since the last
  installation. ([\#135](https://github.com/r-lib/remotes/issues/135))

- [`install_url()`](https://remotes.r-lib.org/dev/reference/install_url.md)
  can now install package binaries on windows (r-lib/devtools#1765)

### Minor improvements and fixes

- [`install_deps()`](https://remotes.r-lib.org/dev/reference/install_deps.md)
  et al. now do not rewrite the `type` argument from `both` to `binary`
  to allow falling back to `source`. This fixes various installation
  failures.

- remotes now looks up GitHub package names locally, if possible, and
  uses the GitHub REST API (if the curl package is available, and not in
  standalone mode). This makes the remote lookup about 10x faster when
  the remote package has not changed since the last install.

- Using a GITHUB_PAT no longer prints diagnostic messages by default
  (r-lib/devtools#1752).

- remotes now always uses https URLs for R versions that support them
  ([@ankane](https://github.com/ankane),
  [\#139](https://github.com/r-lib/remotes/issues/139))

- Do not include the BioCextra repository in versions after it was
  deprecated (R 3.5+, Bioc 3.6+).

- `install_()` functions now download tarballs (.tar.gz) files rather
  than zip archives (.zip). This results in generally smaller files and
  avoids issues with script permissions being lost and strange behavior
  of some external unzip programs on Windows
  ([\#96](https://github.com/r-lib/remotes/issues/96)).

- Dependency parsing is now more robust to whitespace around the
  dependency specifications
  ([\#73](https://github.com/r-lib/remotes/issues/73)).

- [`standardise_dep()`](https://remotes.r-lib.org/dev/reference/standardise_dep.md)
  exported, for use in devtools.

- [`install_local()`](https://remotes.r-lib.org/dev/reference/install_local.md)
  now defaults to the current directory.

- [`install_bitbucket()`](https://remotes.r-lib.org/dev/reference/install_bitbucket.md)
  now correctly supports authentication, and the `subdir` argument.

- `install_()` functions give a helpful warning when the package has
  long path names, on Windows. In this case building the package usually
  fails. ([\#84](https://github.com/r-lib/remotes/issues/84),
  [\#178](https://github.com/r-lib/remotes/issues/178)).

- `install_()` functions have now a more robust way of handling various
  tar programs on Windows
  ([\#172](https://github.com/r-lib/remotes/issues/172)).

- `install_()` functions now give a helpful warning on older R versions,
  on Windows, if [`R.home()`](https://rdrr.io/r/base/Rhome.html)
  contains a space character. Installation usually fails in this case.

- GitHub API errors now give better error messages, including data about
  the API rate limits.

## remotes 1.1.1

CRAN release: 2017-12-20

- Accept HTTPS, SSH, or various browser URLs in GitHub repo
  specification, [@jennybc](https://github.com/jennybc),
  [\#90](https://github.com/r-lib/remotes/issues/90),
  [\#109](https://github.com/r-lib/remotes/issues/109),
  [\#112](https://github.com/r-lib/remotes/issues/112)

## remotes 1.1.0

CRAN release: 2017-07-09

- URL encode GitHub references, to allow installing from
  non-alphanumeric branch or tags, [@krlmlr](https://github.com/krlmlr)
  [\#38](https://github.com/r-lib/remotes/issues/38)

- Better cooperation with proxy servers, and better download method
  selection on Windows, [@cderv](https://github.com/cderv),
  [\#45](https://github.com/r-lib/remotes/issues/45),
  [\#46](https://github.com/r-lib/remotes/issues/46)

- [`install_deps()`](https://remotes.r-lib.org/dev/reference/install_deps.md)
  supports tar balls, [\#47](https://github.com/r-lib/remotes/issues/47)

- Allow training slash in GitHub repo specification,
  [\#54](https://github.com/r-lib/remotes/issues/54)

- Work around on some Linux systems, where unzip is set to the empty
  string, [@HenrikBengtsson](https://github.com/HenrikBengtsson),
  [\#57](https://github.com/r-lib/remotes/issues/57)

- Check for circular dependencies while installing,
  [\#31](https://github.com/r-lib/remotes/issues/31)

- Updated Bioconductor repo URLs for newer BioC versions

## remotes 1.0.0

CRAN release: 2016-09-10

First public release.
