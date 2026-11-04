# C++ / Rcpp style

C++ in `src/` exists for hot loops that would be too slow in R (neighbor search, patch growth, patch statistics).
Keep it small: data preparation, validation and anything touching the database stay in R.

Formatting follows `.clang-format` (LLVM base). Most existing files predate it; format only the lines you change with `git clang-format`.
`.clangd` enables the `bugprone-*` clang-tidy checks; resolve their warnings rather than suppressing them.
The package declares no C++ standard, so code must compile with the default of the oldest R that `DESCRIPTION` allows.

## Interface with R

- Export with `// [[Rcpp::export]]` and suffix the name with `_cpp`. Exports stay internal: package code calls them from an R function that prepares and checks the inputs (`distance_neighbors_cpp()` from `set_neighbors()`), and tests may call them directly (`evoland.plus:::must_cpp()`).
- After changing an exported signature, run `Rcpp::compileAttributes()` and commit the regenerated `R/RcppExports.R` and `src/RcppExports.cpp`.
- Validate inputs in the R wrapper. C++ may assume what the wrapper guarantees; state the assumption in the doc comment.
- Return `List`s or `DataFrame`s whose column names match the schema (`id_coord`, `id_lulc`), so the R side needs no renaming.
- Errors via `Rcpp::stop()`, output via `Rcpp::Rcout` guarded by a `quiet` argument.
- In long loops, call `Rcpp::checkUserInterrupt()` periodically (every ~1000 iterations).
- Draw random numbers only through R's generator (`R::unif_rand()`, `R::rnorm()`, ...) so `set.seed()` makes runs reproducible; never `<random>` or `rand()`.
- Prefer Rcpp types at the boundary. Write Rcpp-free headers only for code that should also compile standalone (`clumpy_geometry.h`).

## Indices

- Keep index logic on one side of the boundary. Pass R-native data (vectors, lists, data frames keyed by `id_*` columns) to C++ and let C++ compute the positions it needs, rather than computing positions in R (e.g. with `which()` or `match()`) and passing them across for C++ to offset again. Code that does its own indexing in one language is easier to read than index arithmetic split between two.
- Where positions must cross the boundary anyway, remember R is 1-based and C++ 0-based: convert once, at the boundary, and name variables to say which is which (`cells_1based`).
- Cell and row indices are `int`, matching R's 32-bit integers, which caps rasters at ~2.1e9 cells. Where a computation could overflow `int` (products of dimensions, squared coordinates), use `long long` or `double` and comment why.

## Organization and naming

- Helpers not exported to R are `static` and live in the file that uses them. Shared helpers go in a header, in a namespace (`namespace clumpy`), with an `EVOLAND_<FILE>_H` include guard.
- Types are `PascalCase` (`GridKey`, `SparseColumn`); everything else is `snake_case`.

## Comments

- Each exported function gets a Doxygen-style `/** ... */` block: what it computes, each `@param` (with units and 0/1-basing for indices), and `@return` with column names.
- A file implementing an algorithm from the literature opens with a block comment naming the source and equation or section, and how the parameters map onto it (see `alloc_clumpy.cpp`, `clumpy_geometry.h`).
