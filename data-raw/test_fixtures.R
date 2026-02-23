# Creates canonical test fixture data for use in tests, examples, and vignettes.
library("evoland")
library("data.table")

# Coordinates: 30×30 grid in Swiss LV95
test_coords_t <- create_coords_t_square(
  epsg = 2056,
  extent = terra::ext(c(xmin = 2697000, xmax = 2700000, ymin = 1252000, ymax = 1255000)),
  resolution = 100
)

# Time periods
test_periods_t <- create_periods_t(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2005-01-01",
  end_extrapolated = "2015-01-01"
)

# LULC classes
test_lulc_meta_t <- create_lulc_meta_t(list(
  forest = list(pretty_name = "Forest", src_classes = 1L),
  urban = list(pretty_name = "Urban", src_classes = 2L),
  agriculture = list(pretty_name = "Agriculture", src_classes = 3L)
))

# Synthetic LULC observations with known transition signal
set.seed(42)
n_coords <- nrow(test_coords_t)
test_lulc_data_t <- as_lulc_data_t(rbindlist(list(
  data.table(
    id_run = 0L,
    id_coord = 1:n_coords,
    id_lulc = sample(1:2, n_coords, TRUE, c(0.7, 0.3)),
    id_period = 1L
  ),
  data.table(
    id_run = 0L,
    id_coord = 1:n_coords,
    id_lulc = sample(1:2, n_coords, TRUE, c(0.5, 0.5)),
    id_period = 2L
  ),
  data.table(
    id_run = 0L,
    id_coord = 1:n_coords,
    id_lulc = sample(1:2, n_coords, TRUE, c(0.4, 0.6)),
    id_period = 3L
  )
)))

# Predictor metadata (one for each data type)
test_pred_spec <- list(
  elevation = list(
    unit = "m",
    pretty_name = "Elevation",
    description = "Elevation above sea level",
    data_type = "float",
    sources = list(list(url = "https://example.com/elevation.tif", md5sum = "abc123"))
  ),
  population = list(
    unit = "count",
    pretty_name = "Population",
    description = "Population count",
    data_type = "int",
    sources = list(list(url = "https://example.com/pop.tif", md5sum = "def456"))
  ),
  is_protected = list(
    unit = "boolean",
    pretty_name = "Protected Area",
    description = "Is the area protected",
    data_type = "bool",
    sources = list(list(url = "https://example.com/protected.gpkg", md5sum = "ghi789"))
  ),
  soil_type = list(
    unit = "category",
    pretty_name = "Soil Type",
    description = "Type of soil",
    data_type = "factor",
    factor_levels = c("Sandy", "Loamy", "Clay", "Peaty", "Chalky"),
    sources = list(list(url = "https://example.com/soil.tif", md5sum = "jkl012"))
  )
)
test_pred_meta_t <- create_pred_meta_t(test_pred_spec, with_id_pred = TRUE)

# Predictor data (static + time-varying)
set.seed(43)
test_pred_data_t <- as_pred_data_t(
  rbind(
    # Static predictors (period 0)
    data.table(
      id_run = 0L,
      id_pred = 1L,
      id_coord = 1:n_coords,
      id_period = 0L,
      value = runif(n_coords, 400, 800)
    ),
    data.table(
      id_run = 0L,
      id_pred = 2L,
      id_coord = 1:n_coords,
      id_period = 0L,
      value = sample(0:1000, n_coords, replace = TRUE)
    ),
    data.table(
      id_run = 0L,
      id_pred = 3L,
      id_coord = 1:n_coords,
      id_period = 0L,
      value = sample(c(0, 1), n_coords, replace = TRUE)
    ),
    data.table(
      id_run = 0L,
      id_pred = 4L,
      id_coord = 1:n_coords,
      id_period = 0L,
      value = sample(1:5, n_coords, replace = TRUE)
    )
  )
)

# Save all fixtures
usethis::use_data(
  test_coords_t,
  test_periods_t,
  test_lulc_meta_t,
  test_lulc_data_t,
  test_pred_spec,
  test_pred_meta_t,
  test_pred_data_t,
  overwrite = TRUE,
  compress = "xz",
  internal = TRUE
)
