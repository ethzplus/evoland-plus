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
expect_silent(
  db$intrv_meta_t <- create_intrv_meta_t(list(
    protected_areas = list(
      pre_allocation = TRUE,
      pretty_name = "Nature protection areas",
      description = "This intervention introduces additional protected areas (PAs); urbanisation or agricultural expansion is prohibited within a PA once established.",
      periods = c(7, 8),
      transitions = c(1, 2),
      sources = list(
        list(
          url = "file:///somedir/protected_areas.gpkg",
          md5sum = "something"
        )
      )
    ),
    glaciers = list(
      pre_allocation = FALSE,
      pretty_name = "Deglaciation",
      description = "Deterministic transitions based on GloGEM",
      sources = list(
        list(
          url = "file:///somedir/glacier_extent_2030.tif",
          md5sum = "something"
        ),
        list(
          url = "file:///somedir/glacier_extent_2040.tif",
          md5sum = "something"
        )
      )
    ),
    hydro_predictors = list(
      pre_allocation = TRUE,
      pretty_name = "Hydrological predictor variables",
      description = "Provide refined predictor variables based on the rsplash hydrological model.",
      params = list(
        tmpdir = "/mnt/ramdisk"
      )
    )
  ))
)
expect_silent(
  db$coords_t <- create_coords_t_square(
    epsg = 2056,
    extent = terra::ext(c(
      xmin = 2697000,
      xmax = 2698000,
      ymin = 1252000,
      ymax = 1253000
    )),
    resolution = 100
  )
)
expect_silent(db$intrv_masks_t <- intrv_masks_t)

expect_identical(db$intrv_masks_t, intrv_masks_t)

expect_equal(db$row_count("intrv_masks_t"), nrow(intrv_masks_t))
