library(tinytest)

db <- evoland_db$new(":memory:")

# Test creation and validation
intrv_masks_t <-
  data.table::data.table(
    id_coord = 1:99,
    id_intrv = 1:3
  ) |>
  as_intrv_masks_t()

expect_silent(print(intrv_masks_t))
expect_error(
  db$intrv_masks_t <- intrv_masks_t,
  "Violates foreign key constraint"
)

# should work once intrv_meta_t is populated
config_path <- system.file("config.yaml", package = "evoland")
config <- read_evoland_config(config_path)
expect_silent(db$intrv_meta_t <- create_intrv_meta_t(config))
expect_silent(db$coords_t <- create_coords_t(config))
expect_silent(db$intrv_masks_t <- intrv_masks_t)

expect_identical(db$intrv_masks_t, intrv_masks_t)

expect_equal(db$row_count("intrv_masks_t"), nrow(intrv_masks_t))
