# Parse a remote git repo specification

A remote repo can be specified in two ways:

- as a URL:

  `parse_github_url()` handles HTTPS and SSH remote URLs and various
  GitHub browser URLs

- via a shorthand:

  `parse_repo_spec()` handles this concise form:
  `[username/]repo[/subdir][#pull|@ref|@*release]`

## Usage

``` r
parse_repo_spec(repo)

parse_github_repo_spec(repo)

parse_github_url(repo)
```

## Arguments

- repo:

  Character scalar, the repo specification.

## Value

List with members: `username`, `repo`, `subdir` `ref`, `pull`,
`release`, some which will be empty.

## Examples

``` r
parse_repo_spec("metacran/crandb")
#> $package
#> [1] ""
#> 
#> $username
#> [1] "metacran"
#> 
#> $repo
#> [1] "crandb"
#> 
#> $subdir
#> [1] ""
#> 
#> $ref
#> [1] ""
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 
parse_repo_spec("jimhester/covr#47")        ## pull request
#> $package
#> [1] ""
#> 
#> $username
#> [1] "jimhester"
#> 
#> $repo
#> [1] "covr"
#> 
#> $subdir
#> [1] ""
#> 
#> $ref
#> [1] ""
#> 
#> $pull
#> [1] "47"
#> 
#> $release
#> [1] ""
#> 
parse_repo_spec("jeroen/curl@v0.9.3")       ## specific tag
#> $package
#> [1] ""
#> 
#> $username
#> [1] "jeroen"
#> 
#> $repo
#> [1] "curl"
#> 
#> $subdir
#> [1] ""
#> 
#> $ref
#> [1] "v0.9.3"
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 
parse_repo_spec("tidyverse/dplyr@*release") ## shorthand for latest release
#> $package
#> [1] ""
#> 
#> $username
#> [1] "tidyverse"
#> 
#> $repo
#> [1] "dplyr"
#> 
#> $subdir
#> [1] ""
#> 
#> $ref
#> [1] ""
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] "*release"
#> 
parse_repo_spec("r-lib/remotes@550a3c7d3f9e1493a2ba") ## commit SHA
#> $package
#> [1] ""
#> 
#> $username
#> [1] "r-lib"
#> 
#> $repo
#> [1] "remotes"
#> 
#> $subdir
#> [1] ""
#> 
#> $ref
#> [1] "550a3c7d3f9e1493a2ba"
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 
parse_repo_spec("igraph=igraph/rigraph") ## Different package name from repo name
#> $package
#> [1] "igraph"
#> 
#> $username
#> [1] "igraph"
#> 
#> $repo
#> [1] "rigraph"
#> 
#> $subdir
#> [1] ""
#> 
#> $ref
#> [1] ""
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 

parse_github_url("https://github.com/jeroen/curl.git")
#> $username
#> [1] "jeroen"
#> 
#> $repo
#> [1] "curl"
#> 
#> $ref
#> [1] ""
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 
parse_github_url("git@github.com:metacran/crandb.git")
#> $username
#> [1] "metacran"
#> 
#> $repo
#> [1] "crandb"
#> 
#> $ref
#> [1] ""
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 
parse_github_url("https://github.com/jimhester/covr")
#> $username
#> [1] "jimhester"
#> 
#> $repo
#> [1] "covr"
#> 
#> $ref
#> [1] ""
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 
parse_github_url("https://github.example.com/user/repo.git")
#> $username
#> [1] "user"
#> 
#> $repo
#> [1] "repo"
#> 
#> $ref
#> [1] ""
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 
parse_github_url("git@github.example.com:user/repo.git")
#> $username
#> [1] "user"
#> 
#> $repo
#> [1] "repo"
#> 
#> $ref
#> [1] ""
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 

parse_github_url("https://github.com/r-lib/remotes/pull/108")
#> $username
#> [1] "r-lib"
#> 
#> $repo
#> [1] "remotes"
#> 
#> $ref
#> [1] ""
#> 
#> $pull
#> [1] "108"
#> 
#> $release
#> [1] ""
#> 
parse_github_url("https://github.com/r-lib/remotes/tree/name-of-branch")
#> $username
#> [1] "r-lib"
#> 
#> $repo
#> [1] "remotes"
#> 
#> $ref
#> [1] "name-of-branch"
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 
parse_github_url("https://github.com/r-lib/remotes/commit/1234567")
#> $username
#> [1] "r-lib"
#> 
#> $repo
#> [1] "remotes"
#> 
#> $ref
#> [1] "1234567"
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 
parse_github_url("https://github.com/r-lib/remotes/releases/latest")
#> $username
#> [1] "r-lib"
#> 
#> $repo
#> [1] "remotes"
#> 
#> $ref
#> [1] ""
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] "*release"
#> 
parse_github_url("https://github.com/r-lib/remotes/releases/tag/1.0.0")
#> $username
#> [1] "r-lib"
#> 
#> $repo
#> [1] "remotes"
#> 
#> $ref
#> [1] "1.0.0"
#> 
#> $pull
#> [1] ""
#> 
#> $release
#> [1] ""
#> 
```
