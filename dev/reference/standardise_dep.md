# Standardise dependencies using the same logical as [install.packages](https://rdrr.io/r/utils/install.packages.html)

Standardise dependencies using the same logical as
[install.packages](https://rdrr.io/r/utils/install.packages.html)

## Usage

``` r
standardise_dep(x)
```

## Arguments

- x:

  The dependencies to standardise. A character vector (selecting from
  "Depends", "Imports", "LinkingTo", "Suggests", or "Enhances"), or a
  logical vector.

  `TRUE` is shorthand for "Depends", "Imports", "LinkingTo" and
  "Suggests". `NA` is shorthand for "Depends", "Imports" and "LinkingTo"
  and is the default. `FALSE` is shorthand for no dependencies.

  The value "soft" means the same as `TRUE`, "hard" means the same as
  `NA`.

  Any additional values that don't match one of the standard dependency
  types are filtered out.

## See also

<https://r-pkgs.org/description.html> for additional information on what
each dependency type means.
