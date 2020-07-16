## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.6.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

I ran `R CMD check` on all 38 reverse dependencies
(https://github.com/r-lib/remotes/tree/HEAD/revdep).

devtools is affected due to a documentation mismatch with this release. I will release a new version of devtools shortly which addresses that issue.
