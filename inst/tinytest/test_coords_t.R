library(tinytest)

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
db$config <- read_evoland_config(config_path)

# expecting the validator to complain that fields are all-na
expect_warning(populate_coords_t(db), "is all NA, populate using")
expect_equal(db$row_count("coords_t"), 100L)
