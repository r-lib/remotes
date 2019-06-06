The previous release had a major bug which effected a number of users, this release fixes that bug.

This release also changes the maintainer from Gábor Csárdi to Jim Hester.

## Test environments
* local OS X install, R 3.5.3
* ubuntu 14.04 (on travis-ci), R 3.5.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

I ran `R CMD check` on all 16 reverse dependencies
(https://github.com/r-lib/remotes/tree/master/revdep) there were no regressions
detected.
