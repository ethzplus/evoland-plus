library(tinytest)

db <- evoland_db$new(":memory:")

expect_silent(
  lulc_data_t <- as_lulc_data_t(data.frame(
    id_coord = integer(),
    id_lulc = integer(),
    id_period = integer(),
    date = as.Date(numeric())
  ))
)
expect_silent(print(lulc_data_t))
expect_equal(nrow(lulc_data_t), 0L)

# First populate the database with reference tables following the pattern from other tests
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

expect_silent(
  db$periods_t <- create_periods_t(
    period_length_str = "P10Y",
    start_observed = "1985-01-01",
    end_observed = "2020-01-01",
    end_extrapolated = "2060-01-01"
  )
)

expect_silent(
  db$lulc_meta_t <- create_lulc_meta_t(list(
    closed_forest = list(
      pretty_name = "Dense Forest",
      description = "Normal forest; Forest strips; Afforestations",
      src_classes = c(50:53, 57L)
    ),
    arable = list(pretty_name = "Arable Land", src_classes = 41L),
    urban = list(
      pretty_name = "Urban areas",
      description = "Industrial and camping areas; Garden allotments; Cemeteries",
      src_classes = c(1:14, 19L, 29:36)
    ),
    static = list(
      pretty_name = "Static / immutable classes",
      description = "Airports; Airfields; Dumps; Quarries, mines, et cetera",
      src_classes = c(15:18, 20:28, 61:63, 66:71)
    )
  ))
)

expect_silent(db$lulc_data_t <- lulc_data_t)
expect_equal(db$row_count("lulc_data_t"), 0L)

synthetic_lulc_data_t <- data.table::data.table(
  id_coord = c(1L, 2L, 1L),
  id_lulc = c(1L, 2L, 3L),
  id_period = c(1L, 1L, 2L),
  date = as.Date(c("2000-01-01", "2000-01-01", "2010-01-01"))
)

expect_error(
  db$lulc_data_t <- synthetic_lulc_data_t,
  r"(^inherits.* is not TRUE$)"
)
expect_silent(
  db$lulc_data_t <- as_lulc_data_t(synthetic_lulc_data_t)
)
expect_silent(print(db$lulc_data_t))
expect_equal(db$row_count("lulc_data_t"), 3L)

invalid_lulc_data_t <- as_lulc_data_t(data.table::data.table(
  id_coord = c(1L, 2L, 1L, 1L),
  id_lulc = c(1L, 2L, 3L, 20L),
  id_period = c(1L, 1L, 2L, 2L),
  date = as.Date(c("2000-01-01", "2000-01-01", "2010-01-01", "2010-01-01"))
))

expect_error(
  db$lulc_data_t <- invalid_lulc_data_t,
  '"id_lulc: 20" does not exist in the referenced table'
)
