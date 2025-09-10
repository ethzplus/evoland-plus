# Test configuration reading functionality
require(tinytest)

# Test that the default configuration can be read correctly
config_path <- system.file("config.yaml", package = "evoland")
config <- read_evoland_config(config_path)
expect_true(inherits(config, "evoland_config"))

# Test that validation passes (no errors thrown)
expect_silent(validate(config))

db <- evoland_db$new(":memory:")

expect_silent(db$config <- config)
expect_error(db$config <- config, "DB already has a config")
expect_equal(db$delete_from("config_t"), 1L)
expect_silent(db$config <- config)

expect_equal(db$config, config)
