# R style

Base: the [tidyverse style guide](https://style.tidyverse.org/), as enforced by air (`air.toml`, line width 100) and lintr (`.lintr`).
This file covers what those don't decide, and the places where this package deliberately differs.

## Dependencies

- Prefer base R where it is equally readable. Every new entry in `Imports` needs a reason a reviewer would accept; a few lines of utility code are cheaper than a dependency.
- Avoid the tidyverse (dplyr, purrr, tidyr, stringr, ...): its APIs change too often for a package that must keep running for years. `glue` is the exception; it is stable and used throughout.
- Avoid niche, rarely maintained packages. If the functionality is small, write a non-exported helper in `R/util*.R`.
- Packages needed by one optional feature go in `Suggests` and are checked with `require_suggested(package, purpose)` (`R/util.R`) at the start of the feature.
- Call imported functions with `pkg::fun()`. Use `@importFrom` (in `R/init.R`) only for operators and data.table specials such as `:=` and `%chin%`.

## Naming

Everything is `snake_case`, except constants (`SCREAMING_SNAKE_CASE`, e.g. `TRANSIENT_CATALOG_ERRORS`) and R6 class names, which are snake_case too (`evoland_db`).

Names are part of the database schema and therefore of the API; see [Schema names](#schema-names).

| Thing                              | Pattern                         | Example                                  |
| ---------------------------------- | ------------------------------- | ---------------------------------------- |
| Table class                        | `<entity>_t`                    | `periods_t`, `trans_meta_t`              |
| Coercing constructor               | `as_<entity>_t(x)`              | `as_periods_t()`                         |
| Constructor from a specification   | `create_<entity>_t(...)`        | `create_periods_t(period_length_str =)` |
| S3 methods on a table class        | `validate.<class>`, `print.<class>` | `validate.periods_t()`              |
| View (computed, not stored)        | `<name>_v`                      | `trans_v`, `adjusted_trans_pot_v`        |
| Rcpp export                        | `<name>_cpp`                    | `distance_neighbors_cpp()`               |
| Function bound as an R6 method     | takes `self` (and `private`, `super`) first | `set_full_trans_preds(self, overwrite)` |

Column and variable names:

- Identifiers are `id_<entity>`: `id_run`, `id_period`, `id_coord`. A reference to the same table is prefixed by its role: `parent_id_run`.
- Before/after pairs are suffixed by role: `id_lulc_anterior`, `id_lulc_posterior`. Use the suffix already established for that column; don't introduce new abbreviations (`_ant`, `_prev`).
- Booleans are `is_*` or `has_*`: `is_extrapolated`, `is_viable`.
- Counts are `n_*`: `n_observed`.
- Quantities with a fixed unit carry it as a suffix: `period_length_d`. Where the unit depends on the data (distances in the CRS's unit), the documentation says so.
- Prefer a name that explains over a comment that explains. `extrap_lengths` beats `x` plus a comment.

## Syntax

- Pipe with the native `|>`, never `%>%`. Use the `_` placeholder for non-first arguments: `grep(pattern = "_t$", x = _)`.
- Anonymous functions: `\(x)` for one-liners passed inline, `function(x) { ... }` when the body spans lines.
- Subset lists and data.frame columns with `[["name"]]`, not `$name`, which partially matches. `$` stays for R6 members (`self$connection`, `db$runs_t`).
- Write `TRUE`/`FALSE`, never `T`/`F`. Write integer literals as `1L` where the type matters (ids, counts, indices).
- Use `vapply()` with an explicit `FUN.VALUE`, not `sapply()`. `lapply()` and `for` loops are both fine; choose the clearer one.
- Use `seq_len()` and `seq_along()`, not `1:n`.
- Return early with `return()`; otherwise let the last expression be the value. Use `invisible()` for functions called for their side effect, and return the modified object so they can be piped.
- Clean up with `on.exit(..., add = TRUE)`, e.g. to restore `self$id_run` or remove a temporary directory.

## data.table

The package's tables are `data.table`s; work with them by reference.

- Coerce with `data.table::setDT()`, reorder with `setorder()`/`setcolorder()`, set attributes with `setattr()`. Copy (`data.table::copy()`) only when the caller's object must not change, and say so in a comment.
- Add or modify columns with `:=` or `data.table::set()`. Inside a loop over columns, `set()` is faster and avoids `[.data.table` overhead.
- Columns used in non-standard evaluation (`i`, `j`, `by`, `on`) must be declared in `utils::globalVariables()` in `R/init.R`, or R CMD check reports a NOTE. Regenerate the list with `tools:::.check_code_usage_in_package("evoland")`.
- Chain `x[...][...]` over several lines for readability, one bracket per line when it exceeds the line width.

## Errors, warnings, messages

- Let errors propagate. Add an early guard only when the default error would confuse the user (a cryptic DuckDB error, an index-out-of-bounds three calls down).
- Assertions use the named `stopifnot()` form; the name is the message the user sees:

  ```r
  stopifnot(
    "id_run must be scalar integerish or NULL" = length(y) == 1L && as.integer(y) == y,
    "periods must not overlap" = nrow(overlapping) == 0
  )
  ```

- Errors that need values interpolated use `stop(glue::glue(...), call. = FALSE)`. Say what was expected, what was found, and how to fix it if that is not obvious:

  ```r
  stop(glue::glue("Requested run (id_run = {id_run}) not found in runs_t"), call. = FALSE)
  ```

- Progress messages use `message()`, never `cat()` or `print()` (lintr's `print_linter` catches the latter), so that callers can silence them with `suppressMessages()`. Indent sub-steps by two spaces. Functions that emit progress take `quiet = FALSE`.
- `cat()` belongs only in `print.*` methods.
- Build strings with `glue::glue()`; use `paste()`/`paste0()` for simple concatenation and `toString()` for comma-separated lists of values.

## Table classes (`R/*_t.R`)

Every table follows the shape of `R/periods_t.R`:

1. `as_<entity>_t(x)`: if `x` is missing, build an empty table with the full column set and types; `setDT()`; cast each column with `cast_dt_col()`; compute derived columns; finish with `as_ducklake_db_t(x, class_name =, key_cols =, ...)`, which calls `validate()`.
2. Optionally `create_<entity>_t(...)` for building the table from a specification. It ends by calling `as_<entity>_t()`.
3. `validate.<entity>_t(x, ...)`: call `NextMethod()`, set the canonical column order with `setcolorder()`, then check invariants in one named `stopifnot()`. Return `x`.
4. `print.<entity>_t(x, nrow = 10, ...)`: `cat()` a short summary, then `NextMethod(nrow = nrow, ...)`, then `invisible(x)`.

Factor columns are stored as strings in DuckLake (no ENUM type); the `as_*_t()` constructor casts them back.

## R6 classes and database methods

- The class body in `R/evoland_db.R` (or `R/ducklake_db.R`) holds only the method signature, its roxygen documentation and a one-line delegation:

  ```r
  set_full_trans_preds = function(overwrite = FALSE) {
    create_method_binding(set_full_trans_preds)
  },
  ```

  The implementation is a plain function in the domain file, taking `self` (plus `private` or `super` if needed via `with_private = TRUE` / `with_super = TRUE`) as its first arguments. This keeps all R6 documentation in one file for roxygen, while the logic sits next to the table it works on.
- Never add methods with `$set()`.
- Group methods with section comments: `## Public Methods ----`, `### Setter methods ----`. These show up in RStudio's and Positron's outline.
- Parameterless views are active bindings; views with parameters are methods. Both end in `_v`.

## Schema names

Table names, column names, column types and key columns are read by external tools through the DuckLake catalog.
Treat them as a public API:

- Don't rename or retype a column, or change a table's keys, as a side effect of other work. If it is necessary, make it its own change and say so in the pull request.
- New columns follow the naming rules above. Check the existing `R/*_t.R` files for a column that already means the same thing.

## Documentation (roxygen)

- Markdown roxygen (`Roxygen: list(markdown = TRUE)`). Run `roxygen2::roxygenize()` after changing any `#'` block and commit the resulting `man/` and `NAMESPACE` changes.
- Exported functions document every parameter and the return value. For tables, `@return` lists every column with its meaning, in the canonical column order.
- Group a table's functions on one help page with `@name <entity>_t` on the first and `@describeIn <entity>_t <one line>` on the rest.
- Internal helpers that are documented at all get `@keywords internal`. Short internal helpers may instead have a plain `#` comment, or nothing if the name says it all.
- Link with `[fun()]` and `[pkg::fun()]`. See [prose.md](prose.md) for wording.

## Comments

- Comment why, not what. A comment restating the next line is noise; a comment naming the constraint that forced an odd choice is the most valuable kind.
- `# TODO` comments are allowed (lintr flags them so they stay visible); say what is missing, not just "fix this".
- Remove commented-out code rather than committing it, unless it documents a deliberate alternative; then say so.

## Tests (`inst/tinytest/`)

- tinytest, with files named `test_<thing>.R`. Where a test goes:
  - `test_<table>_t.R`: the `as_*_t()`/`create_*_t()` constructors and validation only; no database.
  - `test_db_*.R`: anything needing a database. Extend an instance those files already open (`make_test_db()` from `helper_testdb.R`) instead of creating another.
  - `test_integ_*.R`: longer sequences (fitting models, allocating periods, evaluating runs).
- Reach non-exported functions as `evoland:::fun`, because tests also run against the installed package.
- Be parsimonious: one expectation per behaviour, the smallest input that exercises it. Prefer `expect_equal()` against a literal table (`data.table::rowwiseDT()` keeps it readable) over many scalar checks.
- Test that errors fire with `expect_error(expr, "part of the message")`.
- Fixtures are generated by `data-raw/test_fixtures.R` into `R/sysdata.rda`. Add a test that regenerates a fixture and compares it, so the stored copy cannot go stale.
- Use `tempfile()`/`tempdir()` for anything written to disk.
