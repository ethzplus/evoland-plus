# Create Reporting Table

The reporting table holds information handy for writing out reports
(tables, graphs...)

## Usage

``` r
as_reporting_t(x)

db_set_report(self, ...)
```

## Arguments

- ...:

  Each named argument is entered into the table with the argument name
  as its key.

## Value

A data.table of class "reporting_t" with columns:

- `key`: Unique character key

- `value`: Value as character

## Functions

- `db_set_report()`: Set or update reporting metadata in the database.
