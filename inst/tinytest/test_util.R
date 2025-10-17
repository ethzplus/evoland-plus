library(tinytest)

# Test validate generic and methods
expect_error(
  validate(list()),
  "No validate method defined for class list"
)

# Test validate.evoland_t dispatch
dt <- data.table::data.table(x = 1:3, y = letters[1:3])
class(dt) <- c("test_class", "evoland_t", class(dt))
expect_silent(validate(dt))

# Test with non-data.table object
non_dt <- data.frame(x = 1:3)
class(non_dt) <- c("test_class", "evoland_t", class(non_dt))
expect_error(validate(non_dt), "inherits.*data.table.*is not TRUE")

# Test check_missing_names
test_list <- list(a = 1, b = 2, c = 3)
expect_silent(evoland:::check_missing_names(test_list, c("a", "b")))
expect_error(
  evoland:::check_missing_names(test_list, c("a", "d", "e")),
  "missing required names: d, e"
)

# Test empty requirements
expect_silent(evoland:::check_missing_names(test_list, character(0)))

# Test %||% operator
expect_equal(NULL %||% "default", "default")
expect_equal("value" %||% "default", "value")
expect_equal(0 %||% "default", 0)
expect_equal(FALSE %||% "default", FALSE)
expect_equal("" %||% "default", "")

# Test evoland:::pluck_wildcard
test_list <- list(
  a = list(
    x = 1,
    y = 2
  ),
  b = list(
    x = 3,
    y = 4
  ),
  c = list(
    x = 5,
    y = 6
  ),
  group1 = list(
    item1 = list(value = 10),
    item2 = list(value = 20)
  ),
  group2 = list(
    item1 = list(value = 30),
    item2 = list(value = 40)
  )
)

# Normal, wildcard, no, nonexistent indexing
expect_equal(evoland:::pluck_wildcard(test_list, "a", "x"), 1)
expect_equal(
  evoland:::pluck_wildcard(test_list, NA, "x"),
  list(a = 1, b = 3, c = 5, group1 = NULL, group2 = NULL)
)
expect_equal(evoland:::pluck_wildcard(test_list), test_list)
expect_null(evoland:::pluck_wildcard(test_list, "nonexistent"))

# Nested wildcards
nested_result <- evoland:::pluck_wildcard(
  test_list[c("group1", "group2")],
  NA,
  NA,
  "value"
)
expected_nested <- list(
  group1 = list(item1 = 10, item2 = 20),
  group2 = list(item1 = 30, item2 = 40)
)
expect_equal(nested_result, expected_nested)

# Test evoland:::ensure_dir
temp_dir <- tempfile()
expect_equal(evoland:::ensure_dir(temp_dir), temp_dir)
expect_true(dir.exists(temp_dir))

# Test with existing directory (should not error)
expect_silent(evoland:::ensure_dir(temp_dir))
expect_true(dir.exists(temp_dir))

# Test with nested directory creation
unlink(temp_dir, recursive = TRUE)

# ensure_dir should fail with read-only filesystem on macOS post-catalina 10.15
if (
  Sys.info()[["sysname"]] == "Darwin" &&
    numeric_version(Sys.info()[["release"]]) >= "19.0.0"
) {
  expect_error(evoland:::ensure_dir("/tests"), "Read-only file system")
}
