# GitHub references

Use as `ref` parameter to
[`install_github()`](https://remotes.r-lib.org/dev/reference/install_github.md).
Allows installing a specific pull request or the latest release.

## Usage

``` r
github_pull(pull)

github_release()
```

## Arguments

- pull:

  Character string specifying the pull request to install

## See also

[`install_github()`](https://remotes.r-lib.org/dev/reference/install_github.md)

## Examples

``` r
github_pull("42")
#> [1] "42"
#> attr(,"class")
#> [1] "github_pull"
```
