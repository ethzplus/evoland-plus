library(tinytest)

trans_meta_t <- create_trans_meta_t()
expect_silent(trans_meta_t)
expect_equal(nrow(trans_meta_t), 0L)
expect_inherits(trans_meta_t, "trans_meta_t")
expect_stdout(print(trans_meta_t), "empty")

# nolint start
synthetic_transitions <- data.table::rowwiseDT(
  id_coord=, id_lulc_anterior=, id_lulc_posterior=, id_period=,
  3        , 1                , 1                 , 4         ,
  3        , 1                , 1                 , 4         ,
  2        , 1                , 2                 , 3         ,
  1        , 1                , 2                 , 2         ,
  3        , 1                , 3                 , 4         ,
  2        , 2                , 1                 , 3         ,
  3        , 2                , 1                 , 4         ,
  1        , 2                , 2                 , 2         ,
  3        , 2                , 2                 , 4         ,
  3        , 2                , 2                 , 4         ,
  1        , 2                , 2                 , 2         ,
  2        , 2                , 2                 , 3         ,
  1        , 2                , 3                 , 2         ,
  3        , 2                , 4                 , 4         ,
  1        , 3                , 1                 , 2         ,
  2        , 3                , 1                 , 3         ,
  2        , 3                , 1                 , 3         ,
  1        , 3                , 2                 , 2         ,
  1        , 3                , 3                 , 2         ,
  2        , 3                , 4                 , 3         ,
  3        , 4                , 1                 , 4         ,
  3        , 4                , 1                 , 4         ,
  3        , 4                , 2                 , 4         ,
  2        , 4                , 2                 , 3         ,
  2        , 4                , 2                 , 3         ,
  2        , 4                , 3                 , 3         ,
  1        , 4                , 3                 , 2         ,
  1        , 4                , 3                 , 2         ,
  1        , 4                , 4                 , 2         ,
  2        , 4                , 4                 , 3
) |>
  evoland:::cast_dt_col("id_coord", "int") |>
  evoland:::cast_dt_col("id_lulc_anterior", "int") |>
  evoland:::cast_dt_col("id_lulc_posterior", "int") |>
  evoland:::cast_dt_col("id_period", "int")

# nolint end
expect_equal(
  create_trans_meta_t(synthetic_transitions)[["frequency_rel"]],
  c(0.10, 0.05, 0.10, 0.05, 0.05, 0.15, 0.05, 0.05, 0.10, 0.15, 0.15)
)

# `transitions` normally comes from trans_v, an unordered DuckDB query whose row order is
# not guaranteed. id_trans is a key referenced from other tables, so it must depend only
# on the observed transitions, not on the order they arrive in.
trans_meta_ordered <- create_trans_meta_t(synthetic_transitions)

expect_equal(
  trans_meta_ordered[["id_trans"]],
  trans_meta_ordered[order(id_lulc_anterior, id_lulc_posterior), id_trans]
)

set.seed(11)
for (i in 1:5) {
  shuffled <- synthetic_transitions[sample(.N)]
  expect_equal(create_trans_meta_t(shuffled), trans_meta_ordered)
}
