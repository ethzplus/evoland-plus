library(tinytest)

# expecting the validator to complain that fields are all-na
expect_silent(
  periods_t <- create_periods_t(
    period_length_str = "P10Y",
    start_observed = "1985-01-01",
    end_observed = "2020-01-01",
    end_extrapolated = "2060-01-01"
  )
)
expect_stdout(print(periods_t), "Date range")

db <- evoland_db$new(":memory:")
expect_stdout(print(db$periods_t), "(0 rows and 4 cols)")
expect_silent(db$periods_t <- periods_t)
expect_equal(periods_t, db$periods_t)
expect_equal(db$row_count("periods_t"), 8L)
