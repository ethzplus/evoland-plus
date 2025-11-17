# Create Intervention Masks Table

Construct and validate (part of) an `intrv_masks_t` object. This is used
to positively link interventions \[\]

## Usage

``` r
as_intrv_masks_t(x)

# S3 method for class 'intrv_masks_t'
print(x, nrow = 10, ...)
```

## Arguments

- x:

  An object that is accepted by
  [`data.table::setDT()`](https://rdatatable.gitlab.io/data.table/reference/setDT.html)

- nrow:

  see
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

- ...:

  passed to
  [data.table::print.data.table](https://rdatatable.gitlab.io/data.table/reference/print.data.table.html)

## Value

A data.table-inheriting object with the columns `id_intrv` and
`id_coord`, creating a relation between coordinates to apply a

## Methods (by generic)

- `print(intrv_masks_t)`: Print an intrv_masks_t object, passing params
  to data.table print
