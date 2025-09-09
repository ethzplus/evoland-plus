# Test configuration reading functionality
require(tinytest)

# Test that the default configuration can be read correctly
config_path <- system.file("config.json", package = "evoland")
config <- read_evoland_config(config_path)
expect_true(inherits(config, "evoland_config"))

# Test that validation passes (no errors thrown)
expect_silent(validate(config))

db <- evoland_db$new(":memory:")

expect_silent(ingest_evoland_config(db, config_path))
expect_error(ingest_evoland_config(db, config_path), "DB already has a config")
expect_silent(ingest_evoland_config(db, config_path, force = TRUE))

expect_equal(
  retrieve_evoland_config(db),
  config
)
