test_that("system_requirements error if no package exists", {
  pkg <- tempfile()

  expect_error(
    system_requirements("ubuntu", "16.04", pkg),
    "must contain a package"
  )

  dir.create(pkg)
  on.exit(unlink(pkg, recursive = TRUE))

  expect_error(
    system_requirements("ubuntu", "16.04", pkg),
    "must contain a package"
  )
})

test_that("system_requirements errors if an invalid os or os_release is given", {
  expect_error(
    system_requirements("foo", "1"),
    "should be one of"
  )
  expect_error(
    system_requirements("ubuntu", "1"),
    "should be one of"
  )
})
test_that("system_requirements errors curl is not found", {
  expect_error(
    system_requirements("foo", "1"),
    "should be one of"
  )
  expect_error(
    system_requirements("ubuntu", "16.04", curl = ""),
    "`curl` must be on the `PATH`"
  )
})

test_that("system_requirements returns an empty character vector if a package has no system requirements", {
  skip_on_cran()
  skip_if_offline()

  pkg <- tempfile()
  dir.create(pkg)
  on.exit(unlink(pkg, recursive = TRUE))

  writeLines("Package: testPkg", file.path(pkg, "DESCRIPTION"))

  expect_equal(
    system_requirements("ubuntu", "16.04", pkg),
    character()
  )
})

test_that("system requirements return the system requirements if it has a direct system requirement", {
  skip_on_cran()
  skip_if_offline()

  pkg <- tempfile()
  dir.create(pkg)
  on.exit(unlink(pkg, recursive = TRUE))

  writeLines("Package: testPkg\nSystemRequirements: libcurl", file.path(pkg, "DESCRIPTION"))

  expect_equal(
    system_requirements("ubuntu", "16.04", pkg),
    "apt-get install -y libcurl4-openssl-dev"
  )

  expect_equal(
    system_requirements("redhat", "7", pkg),
    "yum install -y libcurl-devel"
  )
})

test_that("system_requirements return the system requirements if it has a hard dependency with a system requirement", {
  skip_on_cran()
  skip_if_offline()

  pkg <- tempfile()
  dir.create(pkg)
  on.exit(unlink(pkg, recursive = TRUE))

  writeLines("Package: testPkg\nImports: curl", file.path(pkg, "DESCRIPTION"))

  expect_equal(
    system_requirements("ubuntu", "16.04", pkg),
    c("apt-get install -y libcurl4-openssl-dev",
      "apt-get install -y libssl-dev"
    )
  )

  expect_equal(
    system_requirements("redhat", "7", pkg),
    c("yum install -y libcurl-devel",
      "yum install -y openssl-devel"
    )
  )
})

test_that("system_requirements return the system requirements if it has a soft dependency with a system requirement", {
  skip_on_cran()
  skip_if_offline()

  pkg <- tempfile()
  dir.create(pkg)
  on.exit(unlink(pkg, recursive = TRUE))

  writeLines("Package: testPkg\nSuggests: curl", file.path(pkg, "DESCRIPTION"))

  expect_equal(
    system_requirements("ubuntu", "16.04", pkg),
    c("apt-get install -y libcurl4-openssl-dev",
      "apt-get install -y libssl-dev"
    )
  )

  expect_equal(
    system_requirements("redhat", "7", pkg),
    c("yum install -y libcurl-devel",
      "yum install -y openssl-devel"
    )
  )
})

test_that("system_requirements return the system requirements if 2nd order dependencies have system requirements", {
  skip_on_cran()
  skip_if_offline()

  pkg <- tempfile()
  dir.create(pkg)
  on.exit(unlink(pkg, recursive = TRUE))

  # secret has no system requirements, but imports curl and openssl
  writeLines("Package: testPkg\nSuggests: secret", file.path(pkg, "DESCRIPTION"))

  expect_equal(
    system_requirements("ubuntu", "16.04", pkg),
    c("apt-get install -y libcurl4-openssl-dev",
      "apt-get install -y libssl-dev"
    )
  )

  expect_equal(
    system_requirements("redhat", "7", pkg),
    c("yum install -y libcurl-devel",
      "yum install -y openssl-devel"
    )
  )
})

test_that("system_requirements errors if DESCRIPTION cannot be parsed", {
  skip_on_cran()
  skip_if_offline()

  pkg <- tempfile()
  dir.create(pkg)
  on.exit(unlink(pkg, recursive = TRUE))

  writeLines("Not parsable", file.path(pkg, "DESCRIPTION"))

  expect_error(
    system_requirements("ubuntu", "16.04", pkg),
    "Could not parse DESCRIPTION", fixed = TRUE
  )
})

test_that("system_requirements work with package arguments", {
  skip_on_cran()
  skip_if_offline()

  expect_equal(
    system_requirements("ubuntu", "16.04", package = "curl"),
    c("apt-get install -y libcurl4-openssl-dev",
      "apt-get install -y libssl-dev"
    )
  )
})

test_that("system_requirements work with vector package arguments", {
  skip_on_cran()
  skip_if_offline()

  expect_equal(
    sort(system_requirements("ubuntu", "16.04", package = c("curl", "git2r"))),

    sort(c("apt-get install -y software-properties-common",
      "add-apt-repository -y ppa:cran/libgit2",  "apt-get update",
      "apt-get install -y libgit2-dev", "apt-get install -y libssh2-1-dev",
      "apt-get install -y libssl-dev", "apt-get install -y zlib1g-dev",
      "apt-get install -y libcurl4-openssl-dev"
    ))
  )
})

test_that("system_requirements errors for a single non-existing CRAN package", {
  skip_on_cran()
  skip_if_offline()

  expect_error(
    system_requirements("ubuntu", "16.04", package = c("iDontExist")),
    "Could not locate package 'iDontExist'", fixed = TRUE
  )
})

test_that("system_requirements errors for multiple packages when one does not exist", {
  skip_on_cran()
  skip_if_offline()

  expect_error(
    system_requirements("ubuntu", "16.04", package = c("curl", "iDontExist")),
    "Could not locate package 'iDontExist'", fixed = TRUE
  )
})

test_that("system_requirements allow specifying os_release within os", {
  skip_on_cran()
  skip_if_offline()

  expect_equal(
    system_requirements("ubuntu-16.04", package = "curl"),
    c("apt-get install -y libcurl4-openssl-dev",
      "apt-get install -y libssl-dev"
    )
  )
})

