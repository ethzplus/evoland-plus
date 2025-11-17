# Create Period Table

Creates a period table, i.e. a description of discrete periods during
which land use can transition. This is necessary because a) land use
data may not be available as regular time series and need to be assigned
to such a form, and b) because this normalization helps consistency.

## Usage

``` r
as_periods_t(x)

create_periods_t(
  period_length_str = "P10Y",
  start_observed = "1985-01-01",
  end_observed = "2020-01-01",
  end_extrapolated = "2060-01-01"
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

  End date of the observed data (YYYY-MM-DD)

- end_extrapolated:

  End date for extrapolation time range (YYYY-MM-DD)

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

## Value

A data.table of class "periods_t" with columns:

- `id_period`: Unique ID for each tperiod

- `start_date`: Start date for period

- `end_date`: End date for period

- `is_extrapolated`: bool, are observations matched to this period, or
  is it used for extrapolation?

## Methods (by generic)

- `print(periods_t)`: Print a periods_t object, passing params to
  data.table print

## Functions

- `create_periods_t()`: Creates a periods_t table from specifications;
  periods that start after `end_observed` are marked as extrapolated.
  The special period with ID 0 is used for static phenomena that are
  presumed to be instantaneous at the end of the observed period.
