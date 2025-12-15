# Create Transition Rates Table

Creates a trans_rates_t table that stores transition rates
(probabilities) for each transition type in each time period. Historical
rates are calculated from observed transitions, and future rates are
extrapolated using linear regression.

## Usage

``` r
as_trans_rates_t(x)

create_obs_trans_rates_t(trans_v, trans_meta)

create_extr_trans_rates_t(obs_rates, periods)

# S3 method for class 'trans_rates_t'
print(x, nrow = 10, ...)
```

## Arguments

- x:

  A list or data.frame coercible to a data.table

- trans_v:

  A transitions view table with columns: id_period, id_lulc_anterior,
  id_lulc_posterior, id_coord

- trans_meta:

  A trans_meta_t table with transition metadata including id_trans,
  id_lulc_anterior, and id_lulc_posterior

- obs_rates:

  A trans_rates_t table with observed historical rates

- periods:

  A periods_t table defining the time periods

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

## Value

A data.table of class "trans_rates_t" with columns:

- `id_period`: Foreign key to periods_t

- `id_trans`: Foreign key to trans_meta_t

- `rate`: Transition rate (0 to 1)

## Methods (by generic)

- `print(trans_rates_t)`: Print a trans_rates_t object, passing params
  to data.table print

## Functions

- `create_obs_trans_rates_t()`: Calculate observed transition rates from
  historical data. For each period and transition type, calculates the
  rate as the proportion of id_lulc_anterior cells that transitioned to
  id_lulc_posterior.

- `create_extr_trans_rates_t()`: Extrapolate future transition rates
  using linear regression. For each transition type (id_trans), fits a
  linear model of rate vs period number and extrapolates to future
  periods. Negative predicted rates are set to 0.
