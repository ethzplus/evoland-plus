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
    )
  ),
  distance_to_lake = list(
    unit = "m",
    pretty_name = "Distance to closest lake",
    orig_format = "vector",
    description = "Derived from swissTLM3D",
    sources = list(list(
      url = "https://data.geo.admin.ch/ch.swisstopo.swisstlm3d/swisstlm3d_2025-03/swisstlm3d_2025-03_2056_5728.gpkg.zip",
      md5sum = "ecb3bcfbf6316c6e7542e20de24f61b7"
    ))
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

expect_equal(
  # should also work with unpopulated data fields
  create_pred_meta_t(list(test_pred = list()))[["pretty_name"]],
  "test_pred"
)
