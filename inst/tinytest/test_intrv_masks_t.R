library(tinytest)

intrv_masks_t <-
  data.table::data.table(
    id_coord = 1:99,
    id_intrv = 1:3
  ) |>
  as_intrv_masks_t()

expect_silent(print(intrv_masks_t))
