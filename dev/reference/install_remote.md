# Install a remote package.

This:

1.  downloads source bundle

2.  decompresses & checks that it's a package

3.  adds metadata to DESCRIPTION

4.  calls install

## Usage

``` r
install_remote(
  remote,
  dependencies,
  upgrade,
  force,
  quiet,
  build,
  build_opts,
  build_manual,
  build_vignettes,
  repos,
  type,
  ...
)

add_metadata(pkg_path, meta)

remote_download(x, quiet = FALSE)

remote_metadata(x, bundle = NULL, source = NULL, sha = NULL)

remote_package_name(remote, ...)

remote_sha(remote, ...)
```

## Arguments

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

It uses the additional S3 generic functions to work. Writing methods for
these functions would allow 3rd party packages to define custom remotes.
