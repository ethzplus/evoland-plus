library(tinytest)

lulc_class_spec <- list(
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
)

# expecting the validator to complain that fields are all-na
expect_silent(lulc_meta_t <- create_lulc_meta_t(lulc_class_spec))
expect_stdout(print(lulc_meta_t), "LULC Metadata")


db <- evoland_db$new(":memory:")
expect_stdout(print(db$lulc_meta_t), "(0 rows and 5 cols)")
expect_silent(db$lulc_meta_t <- lulc_meta_t)
expect_equal(lulc_meta_t, db$lulc_meta_t)
expect_equal(db$row_count("lulc_meta_t"), 4L)
