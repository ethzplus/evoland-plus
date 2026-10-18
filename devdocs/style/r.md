# R style

Base: the [tidyverse style guide](https://style.tidyverse.org/), as enforced by air (`air.toml`, line width 100) and lintr (`.lintr`).
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
- `\(x)` for one-line anonymous functions, `function(x) { ... }` when the body spans lines.
- Subset lists and data.frame columns with `[["name"]]`, not `$name`, which partially matches. `$` is for R6 members (`self$connection`, `db$runs_t`).

## data.table

- Work by reference: `setDT()`, `setorder()`, `setcolorder()`, `setattr()`, `:=`, `set()`. Copy (`data.table::copy()`) only when the caller's object must not change, and say so in a comment.
- Columns used in non-standard evaluation (`i`, `j`, `by`, `on`) must be declared in `utils::globalVariables()` in `R/init.R`, or R CMD check reports a NOTE. Regenerate the list with `tools:::.check_code_usage_in_package("evoland")`.

## Errors and messages

- Assertions use the named `stopifnot()` form; the name is the message the user sees:

  ```r
  stopifnot(
    "id_run must be scalar integerish or NULL" = length(y) == 1L && as.integer(y) == y,
    "periods must not overlap" = nrow(overlapping) == 0
  )
  ```

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

  The implementation is a plain function in the domain file, taking `self` (plus `private` or `super` via `with_private = TRUE` / `with_super = TRUE`) as its first arguments. This keeps all R6 documentation in one file for roxygen, while the logic sits next to the table it works on.
- Don't add methods with `$set()`. (`R/evoland_db_views.R` still does; it predates this rule and should be migrated.)
- Group methods with section comments (`## Public Methods ----`), which editors show in their outline.
- Parameterless views are active bindings; views with parameters are methods.

## Schema names

Table names, column names, column types and key columns are read by external tools through the DuckLake catalog.
Don't rename or retype a column, or change a table's keys, as a side effect of other work; make it its own change and say so in the pull request.
Check the existing `R/*_t.R` files for a column that already means the same thing before adding one.

## Documentation (roxygen)

- Run `roxygen2::roxygenize()` after changing any `#'` block and commit the resulting `man/` and `NAMESPACE` changes.
- For tables, `@return` lists every column with its meaning, in the canonical column order.
- Group a table's functions on one help page with `@name <entity>_t` on the first and `@describeIn <entity>_t <one line>` on the rest.
- Documented internal helpers get `@keywords internal`.

## Tests

See `AGENTS.md` for where tests go. In addition:

- Prefer `expect_equal()` against a literal table (`data.table::rowwiseDT()` keeps it readable) over many scalar checks.
- Fixtures are generated by `data-raw/test_fixtures.R` into `R/sysdata.rda`. Add a test that regenerates a fixture and compares it, so the stored copy cannot go stale.
