# devtools

<details>

* Version: 2.3.0
* Source code: https://github.com/cran/devtools
* URL: https://devtools.r-lib.org/, https://github.com/r-lib/devtools
* BugReports: https://github.com/r-lib/devtools/issues
* Date/Publication: 2020-04-10 23:20:11 UTC
* Number of recursive dependencies: 101

Run `revdep_details(, "devtools")` for more info

</details>

## Newly broken

*   checking for code/documentation mismatches ... WARNING
    ```
    ...
      Mismatches in argument default values:
        Name: 'ref' Code: "HEAD" Docs: "master"
    install_github
      Code: function(repo, ref = "HEAD", subdir = NULL, auth_token =
                     github_pat(quiet), host = "api.github.com",
                     dependencies = NA, upgrade = c("default", "ask",
                     "always", "never"), force = FALSE, quiet = FALSE,
                     build = TRUE, build_opts = c("--no-resave-data",
                     "--no-manual", "--no-build-vignettes"), build_manual =
                     FALSE, build_vignettes = FALSE, repos =
                     getOption("repos"), type = getOption("pkgType"), ...)
      Docs: function(repo, ref = "master", subdir = NULL, auth_token =
                     github_pat(quiet), host = "api.github.com",
                     dependencies = NA, upgrade = c("default", "ask",
                     "always", "never"), force = FALSE, quiet = FALSE,
                     build = TRUE, build_opts = c("--no-resave-data",
                     "--no-manual", "--no-build-vignettes"), build_manual =
                     FALSE, build_vignettes = FALSE, repos =
                     getOption("repos"), type = getOption("pkgType"), ...)
      Mismatches in argument default values:
        Name: 'ref' Code: "HEAD" Docs: "master"
    ```

