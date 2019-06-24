# BiocManager

<details>

* Version: 1.30.4
* Source code: https://github.com/cran/BiocManager
* BugReports: https://github.com/Bioconductor/BiocManager/issues
* Date/Publication: 2018-11-13 08:40:10 UTC
* Number of recursive dependencies: 30

Run `revdep_details(,"BiocManager")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > 
      > test_check("BiocManager")
      ── 1. Error: .version_validity('devel') works (@test_version.R#87)  ────────────
      non-character object(s)
      1: expect_true(startsWith(.version_validity("devel"), test)) at testthat/test_version.R:87
      2: quasi_label(enquo(object), label, arg = "object")
      3: eval_bare(get_expr(quo), get_env(quo))
      4: startsWith(.version_validity("devel"), test)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 78 SKIPPED: 2 WARNINGS: 1 FAILED: 1
      1. Error: .version_validity('devel') works (@test_version.R#87) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# dynutils

<details>

* Version: 1.0.3
* Source code: https://github.com/cran/dynutils
* URL: http://github.com/dynverse/dynutils
* BugReports: https://github.com/dynverse/dynutils/issues
* Date/Publication: 2019-05-02 12:10:03 UTC
* Number of recursive dependencies: 67

Run `revdep_details(,"dynutils")` for more info

</details>

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘desc’
      All declared Imports should be used.
    ```

# icesTAF

<details>

* Version: 3.1-1
* Source code: https://github.com/cran/icesTAF
* URL: http://taf.ices.dk
* Date/Publication: 2019-05-24 16:11:33 UTC
* Number of recursive dependencies: 7

Run `revdep_details(,"icesTAF")` for more info

</details>

## In both

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘icesAdvice’
    ```

# opencpu

<details>

* Version: 2.1.2
* Source code: https://github.com/cran/opencpu
* URL: https://www.opencpu.org (website) https://github.com/opencpu/opencpu#readme (devel)
* BugReports: https://github.com/opencpu/opencpu/issues
* Date/Publication: 2019-05-12 17:10:10 UTC
* Number of recursive dependencies: 57

Run `revdep_details(,"opencpu")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# pct

<details>

* Version: 0.2.2
* Source code: https://github.com/cran/pct
* URL: https://itsleeds.github.io/pct/, https://github.com/ITSLeeds/pct
* BugReports: https://github.com/ITSLeeds/pct/issues
* Date/Publication: 2019-06-23 17:10:02 UTC
* Number of recursive dependencies: 105

Run `revdep_details(,"pct")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 11 marked UTF-8 strings
    ```

# reproducible

<details>

* Version: 0.2.8
* Source code: https://github.com/cran/reproducible
* URL: http://reproducible.predictiveecology.org, https://github.com/PredictiveEcology/reproducible
* BugReports: https://github.com/PredictiveEcology/reproducible/issues
* Date/Publication: 2019-03-18 18:20:03 UTC
* Number of recursive dependencies: 108

Run `revdep_details(,"reproducible")` for more info

</details>

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      4      readRDS Whole Cache call 0.1675601006  secs
        objectNames hashElements             hash objSize
      1        file         file c92a272d3d3f3873   24110
      2        .FUN         .FUN 7a8f2865ef4bc06d    1256
        functionName                            component  elapsedTime units
      1      readRDS                              Hashing 0.0008699894  secs
      2      readRDS Loading from memoise version of repo 0.1873638630  secs
      3      readRDS                     Whole Cache call 0.3113448620  secs
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 259 SKIPPED: 66 WARNINGS: 0 FAILED: 2
      1. Failure: test asPath (@test-cache.R#402) 
      2. Failure: test asPath (@test-cache.R#414) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RxODE

<details>

* Version: 0.9.0-8
* Source code: https://github.com/cran/RxODE
* URL: https://nlmixrdevelopment.github.io/RxODE/
* BugReports: https://github.com/nlmixrdevelopment/RxODE/issues
* Date/Publication: 2019-06-21 08:20:35 UTC
* Number of recursive dependencies: 114

Run `revdep_details(,"RxODE")` for more info

</details>

## In both

*   checking whether package ‘RxODE’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘.../revdep/checks.noindex/RxODE/new/RxODE.Rcheck/00install.out’ for details.
    ```

*   checking package dependencies ... NOTE
    ```
    Package suggested but not available for checking: ‘installr’
    ```

## Installation

### Devel

```
* installing *source* package ‘RxODE’ ...
** package ‘RxODE’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gcc... ccache /usr/local/Cellar/llvm/8.0.0/bin/clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `.../revdep/checks.noindex/RxODE/new/RxODE.Rcheck/00_pkg_src/RxODE':
configure: error: cannot run C compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘RxODE’
* removing ‘.../revdep/checks.noindex/RxODE/new/RxODE.Rcheck/RxODE’

```
### CRAN

```
* installing *source* package ‘RxODE’ ...
** package ‘RxODE’ successfully unpacked and MD5 sums checked
** using staged installation
checking for gcc... ccache /usr/local/Cellar/llvm/8.0.0/bin/clang
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... configure: error: in `.../revdep/checks.noindex/RxODE/old/RxODE.Rcheck/00_pkg_src/RxODE':
configure: error: cannot run C compiled programs.
If you meant to cross compile, use `--host'.
See `config.log' for more details
ERROR: configuration failed for package ‘RxODE’
* removing ‘.../revdep/checks.noindex/RxODE/old/RxODE.Rcheck/RxODE’

```
# sapfluxnetr

<details>

* Version: 0.0.7
* Source code: https://github.com/cran/sapfluxnetr
* URL: https://github.com/sapfluxnet/sapfluxnetr
* BugReports: https://github.com/sapfluxnet/sapfluxnetr/issues
* Date/Publication: 2019-05-01 10:40:03 UTC
* Number of recursive dependencies: 96

Run `revdep_details(,"sapfluxnetr")` for more info

</details>

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

