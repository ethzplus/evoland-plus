# Create Transition Rates Table

Creates a trans_rates_t table that stores transition rates
(probabilities) for each transition type in each time period. Historical
rates are calculated from observed transitions, and future rates are
extrapolated using linear regression.

## Usage

``` r
as_trans_rates_t(x)

get_obs_trans_rates(self)

extrapolate_trans_rates(obs_rates, periods, coord_count = NA_integer_)

# S3 method for class 'trans_rates_t'
print(x, nrow = 10, ...)
```

## Arguments

- x:

  A list or data.frame coercible to a data.table

- self:

  a DB instance

- obs_rates:

  A trans_rates_t table of observed transition rates for historical
  periods

- periods:

  A periods_t table with is_extrapolated = TRUE for future periods

- coord_count:

  Optional integer specifying the number of coordinates (cells) for
  normalization

- nrow:

  see
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

## Value

A data.table of class "trans_rates_t" with columns:

- `id_run`: Foreign key to runs_t

- `id_period`: Foreign key to periods_t

- `id_trans`: Foreign key to trans_meta_t

- `count`: Absolute count of transitions

- `rate`: Transition rate: count of transitions in (id_trans, id_period)
  over (id_period)

## Methods (by generic)

- `print(trans_rates_t)`: Print a trans_rates_t object, passing params
  to data.table print

## Functions

- `get_obs_trans_rates()`: Calculate observed transition rates from
  historical data. For each period and transition type, calculates the
  rate as the proportion of id_lulc_anterior cells that transitioned to
  id_lulc_posterior.

- `extrapolate_trans_rates()`: Return future transition rates using
  linear regression. For each id_run + id_trans, fits a linear model of
  rate vs period number and extrapolates to future periods. Negative
  predicted rates are set to 0.
