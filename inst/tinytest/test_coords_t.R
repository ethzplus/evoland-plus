library(tinytest)
devtools::load_all()

config_path <- system.file("config.yml", package = "evoland")
config <- read_evoland_config(config_path)

# expecting the validator to complain that fields are all-na
expect_warning(
  square_coords_t <- create_coords_t(config),
  "is all NA, populate using"
)

expect_equal(nrow(square_coords_t), 8280000L)

expect_stdout(print(square_coords_t), "CRS: EPSG:2056")
