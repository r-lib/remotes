
context("glob")

test_that("glob", {

  glob_match <- function(g, str) expect_true(glob$test(g, str))
  glob_not_match <- function(g, str) expect_false(glob$test(g, str))

  # Match everything
  glob_match("*", "foo")

  # Match the end
  glob_match("f*", "foo")

  # Match the start
  glob_match("*o", "foo")

  # Match the middle
  glob_match("fi*uck", "firetruck")
  glob_not_match("uc", "firetruck")

  # Match zero characters
  glob_match("fi*retruck", "firetruck")

  # More complex matches
  glob_match("*.min.js", "http://example.com/jquery.min.js")
  glob_match("*.min.*", "http://example.com/jquery.min.js")
  glob_match("*/js/*.js", "http://example.com/js/jquery.min.js")

  glob_not_match(".min.", "http://example.com/jquery.min.js")
  glob_match("*.min.*", "http://example.com/jquery.min.js")

  glob_not_match("http:", "http://example.com/jquery.min.js")
  glob_match("http:*", "http://example.com/jquery.min.js")

  glob_not_match("min.js", "http://example.com/jquery.min.js")
  glob_match("*.min.js", "http://example.com/jquery.min.js")

  glob_not_match("/js*jq*.js", "http://example.com/js/jquery.min.js")

  # ?: Match one character, no more and no less
  glob_match("f?o", "foo")
  glob_not_match("f?o", "fooo")
  glob_not_match("f?oo", "foo")

  # []: Match a character range
  glob_match("fo[oz]", "foo")
  glob_match("fo[oz]", "foz")
  glob_not_match("fo[oz]", "fog")

  # {}: Match a choice of different substrings
  glob_match("foo{bar,baaz}", "foobaaz")
  glob_match("foo{bar,baaz}", "foobar")
  glob_not_match("foo{bar,baaz}", "foobuzz")
  glob_match("foo{bar,b*z}", "foobuzz")

  # More complex extended matches
  glob_match("http://?o[oz].b*z.com/{*.js,*.html}",
              "http://foo.baaz.com/jquery.min.js")
  glob_match("http://?o[oz].b*z.com/{*.js,*.html}",
              "http://moz.buzz.com/index.html")
  glob_not_match("http://?o[oz].b*z.com/{*.js,*.html}",
                 "http://moz.buzz.com/index.htm")
  glob_not_match("http://?o[oz].b*z.com/{*.js,*.html}",
                 "http://moz.bar.com/index.html")
  glob_not_match("http://?o[oz].b*z.com/{*.js,*.html}",
                 "http://flozz.buzz.com/index.html")

  # Remaining special chars should still match themselves
  # Test string  "\\\\/$^+.()=!|,.*"  represents  <glob>\\/$^+.()=!|,.*</glob>
  # The equivalent regex is:  /^\\\/\$\^\+\.\(\)\=\!\|\,\..*$/
  # Both glob and regex match:  \/$^+.()=!|,.*
  test_ext_str = "\\\\/$^+.()=!|,.*"
  target_ext_str = "\\/$^+.()=!|,.*"
  glob_match(test_ext_str, target_ext_str)
})
