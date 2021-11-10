## Current state

Remotes is mostly stable and works for the vast majority of people.
However it accumulates a steady stream of issues and pull requests.
Many of the issues are general installation problems unrelated to remotes itself, and are often better addressed to the community site.
Most of the pull requests are for extensions to other code hosting platforms, or tweaks to ones we don't use regularly (like GitLab).
This makes them somewhat challenging to review and accept, as if it is a platform we don't typically use it is easy for things to break in the future.
In addition testing in general is a challenge, many of the tests require internet access and use up the hourly rate limits, which makes them flaky and the test suite is pretty slow overall.

There is also a Makefile which needs to be run for any code change, which is easy to forget to do in practice.

## Known outstanding issues

remotes contains a bundled PAT to make using remotes on CI services avoid rate limits.
Unfortunately the bundled PAT somewhat frequently gets invalidated because a user commits it to somewhere on GitHub and GitHub automatically scans new commits for PATs and invalidates them.
When this happens we would have to release a new version of remotes with a new PAT.
The damage done is somewhat limited these days, as the GitHub Actions workflows by default use the automatic GITHUB_TOKEN as the PAT, so most of our builds are not affected.
This can affect builds on other CI systems however.
A workaround is for the user to setup their own PAT rather then relying on the built in one.

`install.packages()` has a known issue when there are a mix of binary and source packages, like when a package is recently released to CRAN and the binary is not yet built.
It can try to install the packages in the wrong order and there is little the user or remotes can do to fix this. https://bugs.r-project.org/show_bug.cgi?id=17864
Fixing it would require remotes to handle more of the installation ordering itself then it already does, which is what pak does.
Or `install.packages()` could potentially be patched to fix the problem, but getting a good reproducible example seems challenging and R core has been unresponsive to the issue.

There is a known incompatibility when you have multiple sets of credentials needed.
For instance if you have some repositories that need a private GitHub account and some that need the public one, but don't have the same PAT which has both access.
I don't believe there is an easy to implement solution to this problem given the current code.
A workaround is to use a URL remote for the private stuff instead of a GitHub remote.

There are also ongoing issues with the GitLab remotes and various scenarios with private repositories.
Unfortunately we don't use GitLab enough ourselves to really smooth out the rough edges.

## Future directions

Ideally remotes would be marked superseded and additional development around package installation would occur in either pak or renv.
