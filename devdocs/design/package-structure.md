# Package structure

The data model in [database.md](database.md) could in principle be written by any software that speaks DuckLake.
Statistical model objects (random forests, regressions) have no language-agnostic representation, though, so the logic that fits and applies them is tied to one environment: an R package.
It follows the conventions of [R Packages](https://r-pkgs.org/) by Hadley Wickham and Jennifer Bryan.
For where each kind of file lives, see the project layout in [AGENTS.md](../../AGENTS.md).

## Classes

### `ducklake_db`: generic storage

An [R6](https://r6.r-lib.org/) class (`R/ducklake_db.R`) that knows nothing about land use.
It opens a DuckLake catalog in an in-memory DuckDB connection and moves `data.table`s in and out of it: `$commit()` (append, upsert or overwrite, with an explicit uniqueness check), `$fetch()`, `$execute()` and `$get_query()` (with `glue_sql()` interpolation), `$transaction()`, and `$next_id()` for allocating ids.

Writes to the catalog can collide when several processes share a database.
DuckLake retries conflicting commits itself, but not contention on the catalog's own lock, so `ducklake_db` wraps every catalog operation in a retry with exponential backoff.
Which errors count as transient depends on the catalog backend; they are listed in `TRANSIENT_CATALOG_ERRORS`.

Keeping this layer generic means it can be tested on its own (`test_db_ducklake.R`) and reused outside `evoland-plus`.

### `evoland_db`: the domain database

`evoland_db` (`R/evoland_db.R`) inherits from `ducklake_db` and adds the land use domain:

- an active binding per table (`db$periods_t`), which reads the table on get and validates and commits on assignment; tables that define the model's frame (`coords_t`, `periods_t`) can be written only once, the others are upserted;
- views (`db$trans_v`, `db$pred_data_wide_v(...)`), see [database.md](database.md#views);
- the active run (`db$id_run`) and its lineage, through which every read of a run-dependent table is resolved: `$get_read_expr()` returns, for each slice of data, the closest ancestor run that has it;
- the workflow steps: ingesting predictors, fitting models, predicting transition potentials, allocating.

All methods are declared in the class body in `R/evoland_db.R`, because roxygen documents an R6 class only from a single file.
Their implementations live as ordinary functions next to the table they work on (`set_full_trans_preds()` in `R/trans_preds_t.R`) and are bound with `create_method_binding()`.
This keeps the class definition readable as an index of what the database can do, while the logic stays close to its data.

### Table classes

Each table has an S3 class inheriting from `ducklake_db_t`, which inherits from `data.table`.
The class is defined in `R/<table>_t.R`, which holds:

- `as_<table>_t()`, which coerces any list or data.frame, casts the columns and declares the table's keys, partitioning and map columns as attributes;
- optionally `create_<table>_t()`, which derives the table from a specification (`create_periods_t(period_length_str = "P10Y", ...)`);
- `validate.<table>_t()`, which checks invariants (non-overlapping periods, a base run), called by every constructor;
- `print.<table>_t()`, which prints a summary before the data.

Because `ducklake_db` reads keys and partitioning from these attributes, the schema is defined in R, one file per table, and `ducklake_db` itself needs no knowledge of it.

## Allocation backends

Allocation (placing the projected quantity of change on the map) has two interchangeable backends:

- [Dinamica EGO](https://dinamicaego.com/) (`R/alloc_dinamica.R`), run as an external process with models in `inst/dinamica_models/`. It is the established reference, but only runs on Linux x86.
- A C++ implementation of the [CLUMPY](https://github.com/mmyrte/clumpy) allocation methods (`R/alloc_clumpy.R`, `src/alloc_clumpy.cpp`), which runs everywhere R does.

Both read transition potentials and allocation parameters from the database and write the resulting land use map back to `lulc_data_t` under the active run.

## Dependencies

Dependencies are kept few, because every one is a maintenance and installation cost for users.

| Package               | Why                                                                     |
| --------------------- | ----------------------------------------------------------------------- |
| `data.table`          | In-memory tables; modified by reference, fast for millions of cells     |
| `DBI`, `duckdb`       | Database access; DuckDB provides the DuckLake extension                 |
| `R6`                  | Reference semantics for the database classes                            |
| `terra`               | Raster and vector I/O and processing                                    |
| `mlr3`, `mlr3filters`, `paradox` | Model fitting, tuning and feature selection behind one interface |
| `qs2`                 | Fast serialization of model objects into blob columns                   |
| `Rcpp`                | C++ for the neighbor search and allocation loops                        |
| `glue`                | String and SQL interpolation                                            |
| `curl`                | Downloading predictor sources                                           |

Learners (`ranger`, `rpart`), solvers (`lpSolve`), `processx` (for Dinamica) and the documentation and test tooling are in `Suggests` and checked for when needed.
See [../style/r.md](../style/r.md#dependencies) for the rules on adding dependencies.
