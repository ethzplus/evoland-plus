# R style

Base: the [tidyverse style guide](https://style.tidyverse.org/), as enforced by air (`air.toml`, line width 100).
This file covers what those don't decide, and where this package deliberately differs.

## Dependencies

- `glue` is the one tidyverse-adjacent package in use; it is stable and used throughout.
- Packages needed by one optional feature go in `Suggests` and are checked with `require_suggested(package, purpose)` (`R/util.R`) at the start of the feature.
- Call imported functions as `pkg::fun()`. `@importFrom` (in `R/init.R`) is only for operators and data.table specials such as `:=` and `%chin%`.

## Naming

| Thing                            | Pattern                                     | Example                                  |
| -------------------------------- | ------------------------------------------- | ---------------------------------------- |
| Table class                      | `<entity>_t`                                | `periods_t`, `trans_meta_t`              |
| Coercing constructor             | `as_<entity>_t(x)`                          | `as_periods_t()`                         |
| Constructor from a specification | `create_<entity>_t(...)`                    | `create_periods_t(period_length_str =)`  |
| View (computed, not stored)      | `<name>_v`                                  | `trans_v`, `adjusted_trans_pot_v`        |
| Rcpp export                      | `<name>_cpp`                                | `distance_neighbors_cpp()`               |
| Function bound as an R6 method   | takes `self` (and `private`, `super`) first | `set_full_trans_preds(self, overwrite)`  |
| Constant                         | `SCREAMING_SNAKE_CASE`                      | `TRANSIENT_CATALOG_ERRORS`               |

Column names are part of the database schema and therefore of the API; see [Schema names](#schema-names).

- Identifiers are `id_<entity>`, where `<entity>_t`, `<entity>s_t` or `<entity>_meta_t` is the table they identify: `id_run`, `id_coord`, `id_lulc`. `devdocs/er_diagram.R` relies on this to find foreign keys.
- A second reference to the same table adds a role: `parent_id_run`, `id_coord_origin`, `id_lulc_anterior`/`id_lulc_posterior`. Reuse the role suffix already established for that column; don't introduce new abbreviations (`_ant`, `_prev`).
- Booleans are `is_*` or `has_*`; counts are `n_*`; quantities with a fixed unit carry it as a suffix (`period_length_d`).

## Syntax

- Pipe with the native `|>`, using the `_` placeholder for non-first arguments: `grep(pattern = "_t$", x = _)`.
- Don't name an intermediate result that is only used on the next line; continue the pipe instead, and give the rationale in a short comment where it isn't obvious. The flow of data should be visible in the shape of the code:

  ```r
  named_table_out <-
    named_table_in |>
    transformation() |>
    # keep only the cells that can still change
    filter_step() |>
    ...
  ```
- `\(x)` for one-line anonymous functions, `function(x) { ... }` when the body spans lines.
- Subset lists and data.frame columns with `[["name"]]`, not `$name`, which partially matches. `$` is for R6 members (`self$connection`, `db$runs_t`).

## data.table

- Work by reference: `setDT()`, `setorder()`, `setcolorder()`, `setattr()`, `:=`, `set()`. Copy (`data.table::copy()`) only when the caller's object must not change, and say so in a comment.
- Columns used in non-standard evaluation (`i`, `j`, `by`, `on`) must be declared in `utils::globalVariables()` in `R/init.R`, or R CMD check reports a NOTE. Regenerate the list with `tools:::.check_code_usage_in_package("evoland")`.
- As with pipes, chained `data.table[column == "a", column := "b"][,.(column, column2)]` statements should be visually structured like so:
  
  ```r
  assignment <- 
    data.table[
      column == "a", 
      column := "b"
    ][,
      .(
        column, 
        column2
      )
    ]
  ```
  

## Errors and messages

These use R's own `message()`, `warning()` and `stop()` for now; a logging tool with levels and timestamps is planned in ethzplus/evoland-plus#75.


- Assertions use the named `stopifnot()` form; the name is the message the user sees:

  ```r
  stopifnot(
    "id_run must be scalar integerish or NULL" = is_scalar_integerish(y),
    "periods must not overlap" = nrow(overlapping) == 0
  )
  ```

- Check argument types with the predicates in `R/util.R` instead of spelling the test out: `is_integerish()`, `is_scalar_integerish()`, `is_scalar_numeric()`, `is_scalar_character()`, `is_scalar_logical()`. None of them accepts `NA`.

- Other errors use `stop(glue::glue(...), call. = FALSE)`, saying what was expected and what was found.
- Progress goes through `message()`, so callers can silence it with `suppressMessages()`; `cat()` is only for `print.*` methods. Indent sub-steps by two spaces.

## Table classes (`R/*_t.R`)

Every table follows the shape of `R/periods_t.R`:

1. `as_<entity>_t(x)`: if `x` is missing, build an empty table with the full column set and types (the ER diagram is generated from it); `setDT()`; cast each column with `cast_dt_col()`; compute derived columns; finish with `as_ducklake_db_t(x, class_name =, key_cols =, ...)`, which calls `validate()`.
2. Optionally `create_<entity>_t(...)`, building the table from a specification and ending with `as_<entity>_t()`.
3. `validate.<entity>_t(x, ...)`: `NextMethod()`, set the canonical column order with `setcolorder()`, check invariants in one named `stopifnot()`, return `x`.
4. `print.<entity>_t(x, nrow = 10, ...)`: `cat()` a short summary, then `NextMethod(nrow = nrow, ...)`, then `invisible(x)`.

## R6 classes and database methods

- The class body in `R/evoland_db.R` (or `R/ducklake_db.R`) holds only the method signature, its roxygen documentation and a one-line delegation:

  ```r
  set_full_trans_preds = function(overwrite = FALSE) {
    create_method_binding(set_full_trans_preds)
  },
  ```

  The implementation is a plain function in the domain file, taking `self` (plus `private` or `super` via `with_private = TRUE` / `with_super = TRUE`) as its first arguments. This is the canonical way to bind methods (ethzplus/evoland-plus#28):
  - all R6 documentation stays in one file, which roxygen requires;
  - the logic sits next to the table it works on;
  - the implementation is a named function that receives `self` explicitly, so linters see no undefined symbols and language servers can jump to it.
- Don't add methods with `$set()`. `R/evoland_db_views.R` still does; it predates this rule and is to be migrated (ethzplus/evoland-plus#28).
- Group methods with section comments (`## Public Methods ----`), which editors show in their outline.
- Parameterless views are active bindings; views with parameters are methods. A view longer than a few lines of SQL keeps its query in `inst/<name>.sql`, see [sql.md](sql.md).

## Schema names

Table names, column names, column types and key columns are read by external tools through the DuckLake catalog.
Don't rename or retype a column, or change a table's keys, as a side effect of other work; make it its own change.
Check the existing `R/*_t.R` files for a column that already means the same thing before adding one.

## Documentation (roxygen)

- Run `roxygen2::roxygenize()` after changing any `#'` block and commit the resulting `man/` and `NAMESPACE` changes.
- For tables, `@return` lists every column with its meaning, in the canonical column order.
- Group a table's functions on one help page with `@name <entity>_t`. Much of the existing documentation uses `@describeIn` for the other functions; treat that as legacy and prefer a manual structure with sections that fit the content, see [prose.md](prose.md#roxygen).
- Documented internal helpers get `@keywords internal`.

## Tests

See `AGENTS.md` for where tests go. In addition:

- Prefer `expect_equal()` against a literal table (`data.table::rowwiseDT()` keeps it readable) over many scalar checks.
- Fixtures are generated by `data-raw/test_fixtures.R` into `R/sysdata.rda`. Add a test that regenerates a fixture and compares it, so the stored copy cannot go stale.
