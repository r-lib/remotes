# Simpler available.packages

This is mostly equivalent to
[`utils::available.packages()`](https://rdrr.io/r/utils/available.packages.html)
however it also caches the full result. Additionally the cache can be
assigned explicitly with `available_packages_set()` and reset (cleared)
with `available_packages_reset()`.

## Usage

``` r
available_packages_set(repos, type, db)

available_packages_reset()

available_packages(repos = getOption("repos"), type = getOption("pkgType"))
```

## Arguments

- repos:

  character vector, the base URL(s) of the repositories to use.

- type:

  character string, indicate which type of packages: see
  [`install.packages`](https://rdrr.io/r/utils/install.packages.html).

  If `type = "both"` this will use the source repository.

## See also

[`utils::available.packages()`](https://rdrr.io/r/utils/available.packages.html)
for full documentation on the output format.
