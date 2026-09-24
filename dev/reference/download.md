# Download a file

Uses either the curl package for R versions older than 3.2.0, otherwise
a wrapper around
[`download.file()`](https://rdrr.io/r/utils/download.file.html).

## Usage

``` r
download(
  path,
  url,
  auth_token = NULL,
  basic_auth = NULL,
  quiet = TRUE,
  headers = NULL
)
```

## Arguments

- path:

  Path to download to. `dirname(path)` must exist.

- url:

  URL.

- auth_token:

  Token for token-based authentication or `NULL`.

- basic_auth:

  List with `user` and `password` for basic HTTP authentication, or
  `NULL`.

- quiet:

  Passed to
  [`download.file()`](https://rdrr.io/r/utils/download.file.html) or
  [`curl::curl_download()`](https://jeroen.r-universe.dev/curl/reference/curl_download.html).

- headers:

  Named character vector of HTTP headers to use.

## Value

`path`, if the download was successful.

## Details

We respect the `download.file.method` setting of the user. If it is not
set, then see `download_method()` for choosing a method.

Authentication can be supplied three ways:

- By setting `auth_token`. This will append an HTTP `Authorization`
  header: `Authorization: token {auth_token}`.

- By setting `basic_auth` to a list with elements `user` and `password`.
  This will append a proper `Authorization: Basic {encoded_password}`
  HTTP header.

- By specifying the proper `headers` directly.

If both `auth_token` and `basic_auth` are specified, that's an error.
`auth_token` and `basic_auth` are *appended* to `headers`, so they take
precedence over an `Authorization` header that is specified directly in
`headers`.
