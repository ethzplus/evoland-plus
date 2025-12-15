library(tinytest)

# Test empty trans_rates_t creation
trans_rates_t <- as_trans_rates_t()
expect_stdout(print(trans_rates_t), "empty")

# Create synthetic transition (meta) data
# Period 1: 100 cells, 80 stay as class 1, 20 transition to class 2
# Period 2: 100 cells, 70 stay as class 1, 30 transition to class 2
trans_v <-
  data.table::data.table(
    id_period = rep(1:2, each = 100),
    id_lulc_anterior = 1L,
    id_lulc_posterior = c(
      rep(1L, 80),
      rep(2L, 20),
      rep(1L, 70),
      rep(2L, 30)
    ),
    id_coord = rep(1:100, times = 2)
  )
trans_meta <-
  create_trans_meta_t(trans_v)[,
    id_trans := seq_len(.N)
  ]
periods <-
  create_periods_t(
    period_length_str = "P10Y",
    start_observed = "2010-01-01",
    end_observed = "2020-01-01",
    end_extrapolated = "2060-01-01"
  )

# Create observed rates
obs_rates <- create_obs_trans_rates_t(trans_v, trans_meta)
expect_equal(obs_rates[["rate"]], c(0.2, 0.3))

# Test create_extr_trans_rates_t
extrap_rates <- create_extr_trans_rates_t(obs_rates, periods)
expect_true(inherits(extrap_rates, "trans_rates_t"))
expect_equal(extrap_rates[["rate"]], c(0.4, 0.5, 0.6, 0.7)) # linear trend

# concat on DB, then retrieve for dinamica
test_dir_rates <- tempfile("evoland_rates")
on.exit(unlink(test_dir_rates, recursive = TRUE), add = TRUE)
db <- evoland_db$new(test_dir_rates)
expect_warning(db$trans_meta_t <- trans_meta, "Overriding existing IDs")
db$trans_rates_t <- obs_rates
db$trans_rates_t <- extrap_rates
expect_equal(
  db$trans_rates_dinamica_v(1),
  data.table::data.table(`From*` = 1L, `To*` = 2L, Rate = 0.2)
)

# Test that negative rates are set to 0
# Create data that would extrapolate to negative
declining_rate <-
  data.table::data.table(
    id_period = 1:2,
    id_trans = 1L,
    rate = c(0.15, 0.1)
  ) |>
  as_trans_rates_t() |>
  create_extr_trans_rates_t(periods)
expect_equal(declining_rate[["rate"]], c(.05, 0, 0, 0)) # No negative rates

# Test validation
expect_error(
  as_trans_rates_t(data.table::data.table(
    id_period = 1L,
    id_trans = 1L,
    rate = -0.1 # Invalid negative rate
  )),
  "rate is negative"
)

# Test with only one observation for transition (edge case)
single_extrap <-
  data.table::data.table(
    id_period = 1L,
    id_trans = 1L,
    rate = 0.5
  ) |>
  as_trans_rates_t() |>
  create_extr_trans_rates_t(periods)
expect_true(all(single_extrap[["rate"]] == 0.5)) # Should use mean rate
