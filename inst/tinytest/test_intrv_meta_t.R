library(tinytest)

# construct list from yaml - could drop the yaml dependency?
intrv_spec <- list(
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
)

# Test creation and validation
intrv_meta_t_correct <- create_intrv_meta_t(intrv_spec)
expect_silent(print(intrv_meta_t_correct))
expect_equal(nrow(intrv_meta_t_correct), 3L)
expect_equal(intrv_meta_t_correct$pre_allocation, c(TRUE, FALSE, TRUE))
expect_equal(intrv_meta_t_correct$id_period_list, list(c(7, 8), NULL, NULL))
