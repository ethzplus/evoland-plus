require(tinytest)

# Test empty trans_rates_t creation
trans_rates_t <- as_trans_rates_t()
expect_stdout(print(trans_rates_t), "empty")

# Test extrapolation
periods <-
  data.table::rowwiseDT(
    id_period=,  start_date=,  end_date=,   is_extrapolated=, # nolint
    0,          "2000-01-01", "2000-12-31", FALSE,
    1,          "2001-01-01", "2001-12-31", FALSE,
    2,          "2002-01-01", "2002-12-31", FALSE,
    3,          "2003-01-01", "2003-12-31", TRUE,
    4,          "2004-01-01", "2004-12-31", TRUE
  ) |>
  as_periods_t()

obs_rates <-
  data.table::rowwiseDT(
    id_run=, id_period=, id_trans=, count=,      rate=, # nolint
    # expecting 0.3, 0.4
    0,       1,          1,         NA_integer_, 0.1,
    0,       2,          1,         NA_integer_, 0.2,
    # negative trend: 0.1, 0
    0,       1,          2,         NA_integer_, 0.3,
    0,       2,          2,         NA_integer_, 0.2,
    # single observation: expecting constant 0.4
    1,       1,          1,         NA_integer_, 0.4
  ) |>
  as_trans_rates_t()

# Test extrapolate_trans_rates
extrap_rates <- extrapolate_trans_rates(obs_rates, periods)

expected_extrap_rates <-
  data.table::rowwiseDT(
    id_run=, id_period=, id_trans=, count=,      rate=, # nolint
    # expecting 0.3, 0.4
    0,       3,          1,         NA_integer_, 0.3,
    0,       4,          1,         NA_integer_, 0.4,
    # negative trend: 0.1, 0
    0,       3,          2,         NA_integer_, 0.1,
    0,       4,          2,         NA_integer_, 0.0,
    # single observation: expecting constant 0.4
    1,       3,          1,         NA_integer_, 0.4,
    1,       4,          1,         NA_integer_, 0.4
  ) |>
  as_trans_rates_t()


expect_equal(extrap_rates, expected_extrap_rates)
