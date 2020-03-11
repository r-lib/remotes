
context("Deps")

test_that("standardise_dep", {

  expect_equal(
    standardise_dep(NA),
    c("Depends", "Imports", "LinkingTo")
  )

  expect_equal(
    standardise_dep(TRUE),
    c("Depends", "Imports", "LinkingTo", "Suggests")
  )

  expect_equal(
    standardise_dep(FALSE),
    character(0)
  )

  expect_equal(
    standardise_dep(c("Imports", "Suggests")),
    c("Imports", "Suggests")
  )

  expect_error(
    standardise_dep(1:10),
    "Dependencies must be a boolean or a character vector"
  )

})


test_that("compare_versions", {

  expect_equal(
    compare_versions(
      c("1.0.0", "1.0.0", "1.0.0"),
      c("1.0.1", "0.9.0", "1.0.0"),
      c(TRUE, TRUE, TRUE)),
    c(-1L, 1L, 0L)
  )

  expect_equal(
    compare_versions(
      c(NA, "1.0.0"),
      c("1.0.0", NA),
      c(TRUE, TRUE)),
    c(-2L, 2L)
  )

})


test_that("remote_deps", {
  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  pkg <- list(
    package = "foo",
    remotes = "github::hadley/testthat@v2.0.0,klutometis/roxygen@v6.0.1"
  )

  res <- remote_deps(pkg)

  expect_equal(res$package, c("testthat", "roxygen2"))

  # There are the shas for the v2.0.0 and v6.0.1 tags respectivly
  expect_equal(
    res$available,
    c("b0c0d5dcd78c5f97790c4b6ddb5babbce4c63a9d",
      "c4081879d943ad31eacbbf47410dca0ff88d6460"))
})


test_that("printing package_deps", {

  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "magrittr"),
    installed = c("1.0", "1.0", "1.0"),
    available = c("1.0", NA, "1.0"),
    diff = c(0L, 2L, 0L)
  )
  object$remote <- list(cran_remote("dotenv", NULL, NULL),
                        cran_remote("falsy", NULL, NULL),
                        cran_remote("magrittr", NULL, NULL))

  class(object) <- c("package_deps", "data.frame")

  expect_output(
    print(object),
    "Not on CRAN.*\n.*package.*\n.*falsy.*"
  )

  expect_output(
    print(object, show_ok = TRUE),
    paste(
      sep = "\n",
      "Not on CRAN.*",
      ".*package.*",
      ".*falsy.*",
      "OK.*",
      ".*package.*",
      " dotenv.*",
      " magrittr"
    )
  )

  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "magrittr"),
    installed = c("1.0", "1.0", NA),
    available = c("1.0", "1.1", "1.0"),
    diff = c(0L, -1L, -2L)
  )
  object$remote <- list(cran_remote("dotenv", NULL, NULL),
                        cran_remote("falsy", NULL, NULL),
                        cran_remote("magrittr", NULL, NULL))

  class(object) <- c("package_deps", "data.frame")

  expect_output(
    print(object),
    paste(
      sep = "\n",
      "Needs update ------.*",
      " package .*",
      " falsy .*",
      " magrittr .*"
    )
  )
})


test_that("update_packages", {

  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "magrittr"),
    installed = c("1.0", "1.0", NA),
    available = c("1.0", "1.1", "1.0"),
    diff = c(0L, -1L, -2L)
  )
  class(object) <- c("package_deps", "data.frame")

  mockery::stub(update_packages, "package_deps", object)
  mockery::stub(
    update_packages,
    "update.package_deps",
    function(x, ...) x$package)

  expect_equal(
    update_packages("dotenv"),
    c("dotenv", "falsy", "magrittr")
  )
})

test_that("Additional_repositories field", {

  pkg <- list(
    additional_repositories =
      "http://packages.ropensci.org, http://foo.bar.com"
  )

  expect_equal(
    parse_additional_repositories(pkg),
    c("http://packages.ropensci.org", "http://foo.bar.com")
  )

  pkg <- list(
    additional_repositories =
      "http://packages.ropensci.org, \nhttp://foo.bar.com"
  )

  expect_equal(
    parse_additional_repositories(pkg),
    c("http://packages.ropensci.org", "http://foo.bar.com")
  )

  pkg <- list(
    additional_repositories = 
      "\n  http://packages.ropensci.org, \nhttp://foo.bar.com"
  )

  expect_equal(
    parse_additional_repositories(pkg),
    c("http://packages.ropensci.org", "http://foo.bar.com")
  )

})

test_that("update.package_deps", {

  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "magrittr"),
    installed = c("1.0", "1.0", "1.0"),
    available = c("1.0", NA, "1.0"),
    diff = c(CURRENT, UNAVAILABLE, CURRENT),
    is_cran = c(TRUE, TRUE, TRUE)
  )
  object$remote <- list(
    cran_remote("dotenv", getOption("repos"), getOption("type")),
    cran_remote("falsy", getOption("repos"), getOption("type")),
    cran_remote("magrittr", getOption("repos"), getOption("type"))
  )

  class(object) <- c("package_deps", "data.frame")

  mockery::stub(update, "install_packages", NULL)
  expect_message(
    update(object, upgrade = TRUE, quiet = FALSE),
    "Skipping 1 packages? not available: falsy"
  )
})

test_that("update.package_deps 2", {

  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "magrittr"),
    installed = c("1.0", "1.1", "1.0"),
    available = c("1.0", "1.0", "1.0"),
    diff = c(CURRENT, AHEAD, CURRENT),
    is_cran = c(TRUE, TRUE, TRUE)
  )
  object$remote <- list(
    cran_remote("dotenv", getOption("repos"), getOption("type")),
    cran_remote("falsy", getOption("repos"), getOption("type")),
    cran_remote("magrittr", getOption("repos"), getOption("type"))
  )
  class(object) <- c("package_deps", "data.frame")

  mockery::stub(update, "install_packages", NULL)
  expect_message(
    update(object, upgrade = TRUE, quiet = FALSE),
    "Skipping 1 packages? ahead of CRAN: falsy"
  )
})

test_that("update.package_deps 3", {

  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "magrittr"),
    installed = c("1.0", "1.0", NA),
    available = c("1.0", "1.1", "1.0"),
    diff = c(CURRENT, BEHIND, UNINSTALLED),
    is_cran = c(TRUE, TRUE, TRUE)
  )
  object$remote <- list(
    cran_remote("dotenv", getOption("repos"), getOption("type")),
    cran_remote("falsy", getOption("repos"), getOption("type")),
    cran_remote("magrittr", getOption("repos"), getOption("type"))
  )

  class(object) <- c("package_deps", "data.frame")

  mockery::stub(
    update.package_deps,
    "install_packages",
    function(packages, ...) packages)
  expect_equal(
    update(object, upgrade = FALSE),
    NULL
  )
})

context("Remotes")

test_that("remote_deps returns an empty data frame if no remotes specified", {

  pkg <- list(
    package = "foo"
  )

  expect_equal(remote_deps(pkg), package_deps_new())
})

test_that("remote_deps works with implicit types", {

  expect_equal(
    parse_one_remote("hadley/testthat"),
    github_remote("hadley/testthat")
  )

  expect_equal(split_remotes("hadley/testthat,klutometis/roxygen"),
      c("hadley/testthat", "klutometis/roxygen"))

  expect_equal(split_remotes("hadley/testthat,\n  klutometis/roxygen"),
    c("hadley/testthat", "klutometis/roxygen"))

  expect_equal(split_remotes("hadley/testthat,\n\t klutometis/roxygen"),
    c("hadley/testthat", "klutometis/roxygen"))
})

test_that("split_remotes errors with missing commas", {
  expect_error(split_remotes("hadley/testthat hadley/ggplot2"), "Missing commas")
  expect_error(split_remotes("hadley/testthat\n  hadley/ggplot2"), "Missing commas")
  expect_error(split_remotes("hadley/testthat, hadley/ggplot2, klutometis/roxygen r-lib/devtools"), "Missing commas.*'klutometis")
})

test_that("parse_one_remote errors", {
  expect_error(parse_one_remote(""),
    "Malformed remote specification ''")

  expect_error(parse_one_remote("git::testthat::blah"),
    "Malformed remote specification 'git::testthat::blah'")
  expect_error(parse_one_remote("hadley::testthat"),
    "Unknown remote type: hadley")
  expect_error(parse_one_remote("SVN2::testthat"),
    "Unknown remote type: SVN2")

  expect_error(
    parse_one_remote("git::testthat::blah"),
    "Malformed remote specification 'git::testthat::blah'"
  )
  expect_error(
    parse_one_remote("hadley::testthat"),
    "Unknown remote type: hadley"
  )
  expect_error(
    parse_one_remote("SVN2::testthat"),
    "Unknown remote type: SVN2"
  )
})

test_that("remotes are parsed with explicit types", {

  expect_equal(
    parse_one_remote("github::hadley/testthat"),
    github_remote("hadley/testthat"))

  expect_equal(split_remotes("github::hadley/testthat,klutometis/roxygen"),
    c("github::hadley/testthat", "klutometis/roxygen"))

  expect_equal(split_remotes("hadley/testthat,github::klutometis/roxygen"),
    c("hadley/testthat", "github::klutometis/roxygen"))

  expect_equal(split_remotes("github::hadley/testthat,github::klutometis/roxygen"),
    c("github::hadley/testthat", "github::klutometis/roxygen"))

  expect_equal(split_remotes("bioc::user:password@release/Biobase#12345,github::klutometis/roxygen"),
    c("bioc::user:password@release/Biobase#12345", "github::klutometis/roxygen"))

})

test_that("type = 'both' works well", {

  skip_on_cran()
  skip_if_offline()
  skip_if_over_rate_limit()

  # -6 is the remote column, which includes the pkg_type
  # as an attribute, so we remove it from the comparison
  expect_equal(
    package_deps("falsy", type = "both")[-6],
    package_deps("falsy", type = "binary")[-6]
  )

})

test_that("resolve_upgrade works", {
  # returns ask by default when used interactively
  expect_equal(resolve_upgrade(c("default", "ask", "always", "never"), is_interactive = TRUE), "ask")

  # returns always by default when used non-interactively
  expect_equal(resolve_upgrade(c("default", "ask", "always", "never"), is_interactive = FALSE), "always")

  # returns always when given TRUE or always input
  expect_equal(resolve_upgrade(TRUE, is_interactive = FALSE), "always")
  expect_equal(resolve_upgrade("always", is_interactive = FALSE), "always")

  # returns never when given FALSE or never input
  expect_equal(resolve_upgrade(FALSE, is_interactive = FALSE), "never")
  expect_equal(resolve_upgrade("never", is_interactive = FALSE), "never")

  # errors on unexpected inputs
  expect_error(resolve_upgrade("sometimes"), "'arg' should be one of")
})

test_that("upgradeable_packages works", {
  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "rlang", "magrittr"),
    installed = c("1.0", "1.0", "abc123", NA),
    available = c("1.0", "1.1", "zyx456", "1.0"),
    diff = c(CURRENT, BEHIND, BEHIND, UNINSTALLED),
    is_cran = c(TRUE, TRUE, FALSE, TRUE)
  )
  object$remote <- list(
    cran_remote("dotenv", getOption("repos"), getOption("type")),
    cran_remote("falsy", getOption("repos"), getOption("type")),
    github_remote("rlib/rlang"),
    cran_remote("magrittr", getOption("repos"), getOption("type"))
  )
  class(object) <- c("package_deps", "data.frame")

  # returns full object if "always"
  expect_equal(upgradable_packages(object, "always", TRUE),
               object)

  # returns only uninstalled packages if "never"
  expect_equal(upgradable_packages(object, "never", TRUE),
               object[which(object$package == "magrittr"), ])

  # returns full object if "ask" and not is_interactive
  expect_equal(upgradable_packages(object, "ask", is_interactive = FALSE, TRUE),
               object)

  # returns selected row to update if "ask" and is_interactive
  mockery::stub(upgradable_packages, "select_menu", function(...) "falsy (1.0    -> 1.1   ) [CRAN]", TRUE)
  expect_equal(upgradable_packages(object, "ask", TRUE, is_interactive = TRUE),
               object[c(
                 which(object$package == "falsy"),
                 which(object$package == "magrittr")
               ), ]
              )

  # returns selected rows to update if "ask" and is_interactive
  mockery::stub(upgradable_packages, "select_menu", function(...) c("falsy (1.0    -> 1.1   ) [CRAN]", "rlang (abc123 -> zyx456) [GitHub]"))
  expect_equal(upgradable_packages(object, "ask", TRUE, is_interactive = TRUE),
               object[c(
                 which(object$package == "falsy"),
                 which(object$package == "rlang"),
                 which(object$package == "magrittr")
               ), ]
              )

  # All should be the whole object
  mockery::stub(upgradable_packages, "select_menu", function(...) "All")
  expect_equal(upgradable_packages(object, "ask", TRUE, is_interactive = TRUE),
               object)

  # None should be only un-installed packages
  mockery::stub(upgradable_packages, "select_menu", function(...) "None")
  expect_equal(upgradable_packages(object, "ask", TRUE, is_interactive = TRUE),
               object[which(object$package == "magrittr"), ])

  # CRAN should be only the CRAN packages
  mockery::stub(upgradable_packages, "select_menu", function(...) "CRAN packages only")
  expect_equal(upgradable_packages(object, "ask", TRUE, is_interactive = TRUE),
               object[c(
                 which(object$package == "falsy"),
                 which(object$package == "magrittr")
               ), ]
              )

  # empty vector should be the 0 row object (you get this when canceling the selection)
  mockery::stub(upgradable_packages, "select_menu", function(...) character(0))
  expect_equal(upgradable_packages(object, "ask", TRUE, is_interactive = TRUE),
               object[which(object$package == "magrittr"), ])

  # If only given current or ahead packages (which dotenv is), just return that
  expect_equal(upgradable_packages(object[object$package == "dotenv", ], "ask", TRUE, is_interactive = TRUE),
               object[object$package == "dotenv", ])
})

test_that("format_upgrades works", {
  object <- data.frame(
    stringsAsFactors = FALSE,
    package = c("dotenv", "falsy", "rlang", "magrittr"),
    installed = c("1.0", "1.0", "abc123", NA),
    available = c("1.0", "1.1", "zyx456", "1.0"),
    diff = c(CURRENT, BEHIND, BEHIND, UNINSTALLED),
    is_cran = c(TRUE, TRUE, FALSE, TRUE)
  )
  object$remote <- list(
    cran_remote("dotenv", getOption("repos"), getOption("type")),
    cran_remote("falsy", getOption("repos"), getOption("type")),
    github_remote("rlib/rlang"),
    cran_remote("magrittr", getOption("repos"), getOption("type"))
  )
  class(object) <- c("package_deps", "data.frame")

  expect_equal(
    format_upgrades(object[0, ]),
    character(0)
  )

  expect_equal(
    format_upgrades(object[object$diff < BEHIND, ]),
    "magrittr (NA -> 1.0) [CRAN]"
  )

  expect_equal(
    format_upgrades(object[object$diff <= BEHIND, ]),
    c(
      "falsy    (1.0    -> 1.1   ) [CRAN]",
      "rlang    (abc123 -> zyx456) [GitHub]",
      "magrittr (NA     -> 1.0   ) [CRAN]"
    )
  )

  expect_equal(
    format_upgrades(object),
    c(
      "dotenv   (1.0    -> 1.0   ) [CRAN]",
      "falsy    (1.0    -> 1.1   ) [CRAN]",
      "rlang    (abc123 -> zyx456) [GitHub]",
      "magrittr (NA     -> 1.0   ) [CRAN]"
    )
  )
})

test_that("dev_package_deps works with package using remotes", {
  skip_on_cran()
  skip_if_offline()

  res <- dev_package_deps(test_path("withremotes"), dependencies = TRUE)

  is_falsy <- "falsy" == res$package
  expect_true(any(is_falsy))
  expect_is(res$remote[is_falsy][[1]], "github_remote")

  is_testthat <- "testthat" == res$package
  expect_true(any(is_testthat))
  expect_is(res$remote[is_testthat][[1]], "cran_remote")
})
