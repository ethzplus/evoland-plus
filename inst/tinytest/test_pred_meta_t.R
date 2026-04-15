library(tinytest)

# nolint start
pred_spec <- list(
  noise = list(
    unit = "dBa",
    pretty_name = "Maximum noise exposure",
    orig_format = "10m*10m raster",
    description = "The maximum across all sonBASE noise exposure categories, i.e. daytime & nighttime road & rail exposure",
    sources = list(
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-strassenlaerm_tag/laerm-strassenlaerm_tag/laerm-strassenlaerm_tag_2056.tif",
        md5sum = "a4b9f1c04ee63824f18852bfd1eecbdd"
      ),
      list(
        url = "https://data.geo.admin.ch/ch.bafu.laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht/laerm-bahnlaerm_nacht_2056.tif",
        md5sum = "4b782128495b5af8467e2259bd57def2"
      )
    ),
    data_type = "float",
    fill_value = NA_real_
  ),
  distance_to_lake = list(
    unit = "m",
    pretty_name = "Distance to closest lake",
    orig_format = "vector",
    description = "Derived from swissTLM3D",
    sources = list(list(
      url = "https://data.geo.admin.ch/ch.swisstopo.swisstlm3d/swisstlm3d_2025-03/swisstlm3d_2025-03_2056_5728.gpkg.zip",
      md5sum = "ecb3bcfbf6316c6e7542e20de24f61b7"
    )),
    data_type = "factor",
    fill_value = "1000+m",
    factor_levels = c("0-100m", "100-500m", "500-1000m", "1000+m")
  )
)
# nolint end

# Test creation and validation
pred_meta_t <- create_pred_meta_t(pred_spec)
expect_true(nrow(pred_meta_t) == 2L)
expect_inherits(pred_meta_t, "pred_meta_t")
expect_true(all(
  c(
    "name",
    "pretty_name",
    "description",
    "orig_format",
    "sources",
    "unit",
    "factor_levels"
  ) %in%
    names(pred_meta_t)
))
expect_stdout(print(pred_meta_t), "Number of predictors")

expect_error(
  # should fail with unpopulated data fields and missing data_type
  create_pred_meta_t(list(test_pred = list()))[["pretty_name"]],
  "data_type must be set"
)

expect_equal(
  # should fail with unpopulated data fields and missing data_type
  create_pred_meta_t(
    list(test_pred = list(data_type = "float"))
  )[["pretty_name"]],
  "test_pred"
)
