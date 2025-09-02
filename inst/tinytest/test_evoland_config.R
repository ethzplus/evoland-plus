# Test configuration reading functionality
require(tinytest)

# Test that the default configuration can be read correctly
config_path <- system.file("config.yml", package = "evoland")
config <- read_evoland_config(config_path)
expect_true(inherits(config, "evoland_config"))

# Test that validation passes (no errors thrown)
expect_silent(validate(config))

expect_error(
  config_t(list(config)),
  pattern = "is not TRUE"
)
new_conf_t <- config_t(list(
  default = config
))
expect_inherits(new_conf_t, "data.table")
expect_inherits(new_conf_t, "config_t")

test_db_path <- tempfile(fileext = ".duckdb")
db <- evoland_db$new(test_db_path)

expect_message(
  db$commit(new_conf_t, mode = "overwrite"),
  pattern = "converting list col"
)
expect_message(
  # should pass exactly the same a second time
  db$commit(new_conf_t, mode = "overwrite"),
  pattern = "converting list col"
)
expect_error(
  # should not be able to insert duplicate
  db$commit(new_conf_t, mode = "append"),
  pattern = "Duplicate key"
)

# should pass fine
db$commit(new_conf_t, mode = "upsert")

# should pass fine
db$commit(config_t(list(nondefault = config)), mode = "append")
