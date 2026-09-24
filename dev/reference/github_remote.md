# Create a new github_remote

This is an internal function to create a new github_remote, users should
generally have no need for it.

## Usage

``` r
github_remote(
  repo,
  ref = "HEAD",
  subdir = NULL,
  auth_token = github_pat(),
  sha = NULL,
  host = "api.github.com",
  ...
)
```

## Arguments

- repo:

  Repository address in the format
  `username/repo[/subdir][@ref|#pull|@*release]`. Alternatively, you can
  specify `subdir` and/or `ref` using the respective parameters (see
  below); if both are specified, the values in `repo` take precedence.

- ref:

  Desired git reference. Could be a commit, tag, or branch name, or a
  call to
  [`github_pull()`](https://remotes.r-lib.org/dev/reference/github_refs.md)
  or
  [`github_release()`](https://remotes.r-lib.org/dev/reference/github_refs.md).
  Defaults to `"HEAD"`, which means the default branch on GitHub and for
  git remotes. See
  [setting-the-default-branch](https://help.github.com/en/github/administering-a-repository/setting-the-default-branch)
  for more details.

- subdir:

  Subdirectory within repo that contains the R package.

- auth_token:

  To install from a private repo, generate a personal access token (PAT)
  with at least repo scope in <https://github.com/settings/tokens> and
  supply to this argument. This is safer than using a password because
  you can easily delete a PAT without affecting any others. Defaults to
  the `GITHUB_PAT` environment variable.

- host:

  GitHub API host to use. Override with your GitHub enterprise hostname,
  for example, `"github.hostname.com/api/v3"`.

- ...:

  Other arguments passed on to
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html).
