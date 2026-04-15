library(tinytest)

# Test creation and validation
pred_data_t <- as_pred_data_t(
  data.table::data.table(
    id_run = 1L,
    id_pred = 1:2,
    id_coord = 3:50,
    id_period = 1L,
    value = 3.14
  )
)
expect_equal(nrow(pred_data_t), 48L)
expect_true(typeof(pred_data_t$value) == "double")
expect_stdout(print(pred_data_t), r"{Raw Predictor Data Table}")

# Test set_pred_coltypes
# nolint start
wide_dt <- data.table::rowwiseDT(
  id_coord= , id_pred_1=, id_pred_2=, id_pred_3=, id_pred_4=,
  1         , 1.1       , 10        , 1         , 1         ,
  2         , 2.2       , 20        , 0         , 2         ,
  3         , 3.3       , 30        , 1         , 3         ,
  4         , 4.4       , 40        , 0         , 4         ,
  5         , NA        , NA        , NA        , NA
)
wide_dt_expected <- data.table::rowwiseDT(
  id_coord= , id_pred_1=, id_pred_2= , id_pred_3=, id_pred_4=,
  1         , 1.1       , 10L        , TRUE      , "Sandy"   ,
  2         , 2.2       , 20L        , FALSE     , "Loamy"   ,
  3         , 3.3       , 30L        , TRUE      , "Clay"    ,
  4         , 4.4       , 40L        , FALSE     , "Peaty"   ,
  5         , NA_real_  , NA_integer_, NA        , NA
)
# nolint end
data.table::set(
  wide_dt_expected,
  j = "id_pred_4",
  value = factor(
    wide_dt_expected[["id_pred_4"]],
    levels = c("Sandy", "Loamy", "Clay", "Peaty", "Chalky")
  )
)


evoland:::set_pred_coltypes(
  result = wide_dt,
  pred_meta_t = evoland:::test_pred_meta_t
)

expect_equal(wide_dt, wide_dt_expected)
expect_equal(
  levels(wide_dt$id_pred_4),
  c("Sandy", "Loamy", "Clay", "Peaty", "Chalky")
)
