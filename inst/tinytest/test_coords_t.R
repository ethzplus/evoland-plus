library(tinytest)
devtools::load_all()

config_path <- system.file("config.yaml", package = "evoland")
db <- evoland_db$new(":memory:")
ingest_evoland_config(db, config_path)

# expecting the validator to complain that fields are all-na
expect_warning(populate_coords_t(db), "is all NA, populate using")
expect_equal(db$row_count("coords_t"), 100L)
