if (requireNamespace("tinytest", quietly = TRUE)) {
  home <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
  tinytest::test_package("evoland.plus", at_home = home)
}
