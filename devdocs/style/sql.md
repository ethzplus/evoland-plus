# SQL style

SQL runs on DuckDB, against tables in a DuckLake catalog.
Formatting follows sql-formatter with `.sql-formatter.json` (DuckDB dialect, lowercase keywords): `npx sql-formatter --config .sql-formatter.json inst/<file>.sql`.
sql-formatter does not understand glue placeholders in every position; if it mangles one, format by hand in the same style.

## Where SQL lives

- A query longer than a few lines, or one with CTEs, goes in `inst/<name>.sql`, named after what it returns, and is loaded with `read_sql("<name>.sql")`.
- Short statements may be inline R strings passed to `$execute()` or `$get_query()`.

## Interpolation

`$execute()` and `$get_query()` interpolate statements with [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html) against the live connection, so values are quoted safely:

| Placeholder    | Inserts                                                      |
| -------------- | ------------------------------------------------------------ |
| `{value}`      | a quoted literal: `'abc'`, `42`                              |
| `{value*}`     | a comma-separated list of literals, for `in ({ids*})`        |
| ``{`name`}``   | a quoted identifier                                          |
| ``{`names`*}`` | a comma-separated list of identifiers                        |
| `{read_expr}`  | a `DBI::SQL()` object verbatim, e.g. from `$get_read_expr()` |

- Don't build SQL from values with `paste()`, `sprintf()` or plain `glue::glue()`: that bypasses quoting. Compose fragments with `glue::glue_sql()` / `glue::glue_sql_collapse()` and the connection, as `R/validation.R` does.
- Read tables through `{<table>_read_expr}` placeholders filled by `self$get_read_expr("<table>")`, not by naming the table. The read expression resolves the active run's lineage; a bare table name silently ignores it.
- Cast interpolated values whose type DuckDB can't infer: `{id_run}::integer`.
- If the query itself needs braces (struct or map literals), pass `.open`/`.close` to use other delimiters.

## File header

Every `inst/*.sql` file starts with a block comment saying what the query returns and listing every placeholder, so the R call site can be checked against it without reading the query:

```sql
/*
Pontius' figure of merit of simulated against observed land use change, per simulated run.

Interpolated by `ducklake_db$get_query()`, requiring
Filters:
- {id_period_anterior}
- {id_period_post}
Data sources:
- {reference_read_expr}: lulc_data_t of the reference run
- {simulated_read_expr}: lulc_data_t rows (id_run, id_coord, id_lulc) of each simulated run
Output:
- {`result`}: "overall" or "per_transition"
*/
```

## Queries

- One CTE per logical step, named for what it holds (`observed_flows`), not how it was computed (`joined`).
- A result that will be committed must already have the target table's column names.
