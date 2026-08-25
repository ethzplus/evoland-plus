# Create Period Table

Creates a `periods_t` table, i.e. a description of discrete, regular
periods during which land use can transition. This is a precondition for
pattern based land use change models. Periods outside the observed range
are designated `is_extrapolated`. The special period with ID 0 is used
for static phenomena, coded as instantaneous at the end of the observed
period.

## Usage

``` r
as_periods_t(x)

create_periods_t(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2059-12-31"
)

# S3 method for class 'periods_t'
print(x, nrow = 10, ...)
```

## Arguments

- x:

  A list or data.frame coercible to a data.table

- period_length_str:

  ISO 8601 duration string specifying the length of each period
  (currently only accepting years, e.g., "P5Y" for 5 years)

- start_observed:

  Start date of the observed data (YYYY-MM-DD)

- end_observed:

  End date of the observed data (YYYY-MM-DD); periods that start after
  `end_observed` are marked `is_extrapolated`.

- end_extrapolated:

  End date for extrapolation time range (YYYY-MM-DD); only full periods
  *before* this date are taken into account.

- nrow:

  see
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdrr.io/pkg/data.table/man/print.data.table.html)

## Value

A data.table of class "periods_t" with columns:

- `id_period`: Unique ID for each tperiod

- `start_date`: Start date for period

- `end_date`: End date for period

- `period_length_d`: Days between this and the preceding period's
  midpoints, derived from start and end date.

- `is_extrapolated`: bool, are observations matched to this period, or
  is it used for extrapolation?

## Methods (by generic)

- `print(periods_t)`: Print a periods_t object, passing params to
  data.table print

## Functions

- `create_periods_t()`: Creates a regular `periods_t` table from
  specifications.
