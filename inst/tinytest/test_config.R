# Test configuration reading functionality

# Test that the default configuration can be read correctly
config_path <- system.file("config.yml", package = "evoland")
config <- read_evoland_config(config_path, name = "test_config")

# Test that the config object has the correct class
expect_true(inherits(config, "evoland_config"))

# Test that the config object has the correct name attribute
expect_identical(attr(config, "name"), "test_config")

# Test that all required sections are present
required_sections <- c("reporting", "coords", "lulc_data", "lulc_classes", "periods")
expect_true(all(required_sections %in% names(config)))

# Test that required reporting fields are present
required_reporting <- c("scenario_name", "shortname")
expect_true(all(required_reporting %in% names(config[["reporting"]])))

# Test that required coords fields are present
required_coords <- c("type", "epsg", "extent", "resolution")
expect_true(all(required_coords %in% names(config[["coords"]])))

# Test that lulc_classes section has at least 2 classes
expect_true(length(config[["lulc_classes"]]) >= 2)

# Test that each LULC class has a pretty_name
for (class_name in names(config[["lulc_classes"]])) {
  expect_true("pretty_name" %in% names(config[["lulc_classes"]][[class_name]]))
}

# Test that required periods fields are present
required_periods <- c("period_length", "start_observed", "end_observed")
expect_true(all(required_periods %in% names(config[["periods"]])))

# Test that validation passes (no errors thrown)
expect_silent(validate(config))
