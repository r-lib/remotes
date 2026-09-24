# Tools for Bioconductor repositories

`bioc_version()` returns the Bioconductor version for the current or the
specified R version.

## Usage

``` r
bioc_version(r_ver = getRversion())

bioc_install_repos(r_ver = getRversion(), bioc_ver = bioc_version(r_ver))
```

## Arguments

- r_ver:

  R version to use. For `bioc_install_repos()` it is ignored if
  `bioc_ver` is specified.

- bioc_ver:

  Bioconductor version to use. Defaults to the default one corresponding
  to `r_ver`.

## Value

`bioc_version()` returns a Bioconductor version, a `package_version`
object.

`bioc_install_repos()` returns a named character vector of the URLs of
the Bioconductor repositories, appropriate for the current or the
specified R version.

## Details

`bioc_install_repos()` deduces the URLs of the Bioconductor
repositories.

Both functions observe the `R_BIOC_VERSION` environment variable, which
can be set to force a Bioconductor version. If this is set, then the
`r_ver` and `bioc_ver` arguments are ignored.

`bioc_install_repos()` observes the `R_BIOC_MIRROR` environment variable
and also the `BioC_mirror` option, which can be set to the desired
Bioconductor mirror. The option takes precedence if both are set. Its
default value is `https://bioconductor.org`.

## Examples

``` r
bioc_version()
#> [1] ‘3.23’
bioc_version("3.4")
#> [1] ‘3.6’
bioc_install_repos()
#>                                                 BioCsoft 
#>            "https://bioconductor.org/packages/3.23/bioc" 
#>                                                  BioCann 
#> "https://bioconductor.org/packages/3.23/data/annotation" 
#>                                                  BioCexp 
#> "https://bioconductor.org/packages/3.23/data/experiment" 
#>                                            BioCworkflows 
#>       "https://bioconductor.org/packages/3.23/workflows" 
#>                                                BioCbooks 
#>           "https://bioconductor.org/packages/3.23/books" 
```
