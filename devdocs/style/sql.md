# SQL style

SQL runs on DuckDB, against tables in a DuckLake catalog.
Formatting follows sql-formatter with `.sql-formatter.json` (DuckDB dialect, 2-space indent, lowercase keywords): `npx sql-formatter --config .sql-formatter.json inst/<file>.sql`.
sql-formatter does not understand glue placeholders in every position; if it mangles one, format by hand in the same style rather than restructuring the query.

## Where SQL lives

- A query longer than a few lines, or one with CTEs, goes in `inst/<name>.sql` and is loaded with `read_sql("<name>.sql")` (`R/ducklake_db_utils.R`).
- Short statements may be inline R strings passed to `$execute()` or `$get_query()`.
- Name the file after what it returns (`pred_data_wide.sql`, `figure_of_merit.sql`), in snake_case.

## Interpolation

Statements are interpolated by `$execute()` / `$get_query()` with [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html) against the live connection, so values are quoted safely:

| Placeholder        | Inserts                                                       |
| ------------------ | ------------------------------------------------------------- |
| `{value}`          | a quoted literal: `'abc'`, `42`                               |
| `{value*}`         | a comma-separated list of literals, for `in ({ids*})`         |
| ``{`name`}``       | a quoted identifier                                           |
| ``{`names`*}``     | a comma-separated list of identifiers                         |
| `{read_expr}`      | a `DBI::SQL()` object verbatim, e.g. from `$get_read_expr()`  |

- Never build SQL with `paste()`, `sprintf()` or plain `glue::glue()` from values: that bypasses quoting. Compose fragments with `glue::glue_sql()` / `glue::glue_sql_collapse()` and the connection, as `R/validation.R` does.
- Read tables through `{<table>_read_expr}` placeholders filled by `self$get_read_expr("<table>")`, not by naming the table directly. The read expression resolves the active run's lineage; a bare table name silently ignores it.
- Cast interpolated R values whose type DuckDB can't infer: `{id_run}::integer`. Pass ids from R as integers (`as.integer()`).
- If the query itself needs braces (struct or map literals), pass `.open`/`.close` to use other delimiters.

## File header

Every `inst/*.sql` file starts with a block comment saying what the query returns and what it needs:

```sql
/*
Pontius' figure of merit of simulated against observed land use change, per simulated run.
Cells missing from any of the three maps are ignored.

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

List every placeholder. A reader of the R call site should be able to check the arguments against this header without reading the query.

## Query style

- Lowercase keywords and function names: `select`, `count(*)`, `coalesce()`.
- Structure with CTEs (`with ... as (...)`), one logical step each, named for what they hold (`observed_flows`, `period_select`), not for how they're computed (`tmp1`, `joined`).
- One column per line in `select` lists; explicit `as` for every alias.
- Explicit join types (`inner join`, `left join`), never comma joins. Short table aliases are fine when the CTE name is long, but a self-join needs aliases that say which role each side plays (`a` for anterior, `o` for observed).
- Qualify columns with the table alias whenever more than one table is in scope.
- Use DuckDB features where they make the query clearer: `qualify`, `filter (where ...)`, `exclude`, `group by all`, `union all by name`.
- `--` comments explain why a filter or join exists, as in R.
- Column names and types follow the schema (`id_<entity>`, see [r.md](r.md#naming)); a query result that will be committed must already have the target table's column names.
- Keep result row order out of SQL unless the caller depends on it; sort in R with `data.table::setorder()` where needed.
