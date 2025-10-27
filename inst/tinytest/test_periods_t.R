library(tinytest)

expect_silent(
  periods_t <- create_periods_t(
    period_length_str = "P10Y",
    start_observed = "1985-01-01",
    end_observed = "2020-01-01",
    end_extrapolated = "2060-01-01"
  )
)
expect_stdout(print(periods_t), "Date range")
