# C++ / Rcpp style

C++ in `src/` exists for hot loops that would be too slow in R (neighbor search, patch growth, patch statistics).
Keep it small: data preparation, validation and anything touching the database stay in R.

Base: [LLVM style](https://llvm.org/docs/CodingStandards.html), as configured in `.clang-format` (2-space indent, 80 columns, short `if`/loop bodies may stay on one line).
`.clangd` enables the `bugprone-*` clang-tidy checks; resolve their warnings rather than suppressing them.
Most existing files predate `.clang-format`; format only the lines you change with `git clang-format` rather than reformatting whole files.
Target C++17, the default of current R toolchains; don't use newer features.

## Interface with R

- Export with `// [[Rcpp::export]]` and suffix the function name with `_cpp`. Exports stay internal: package code calls them from an R function that prepares and checks the inputs (`distance_neighbors_cpp()` from `set_neighbors()` in `R/neighbors_t.R`), and tests may call them directly as `evoland:::must_cpp()`.
- After changing an exported signature, run `Rcpp::compileAttributes()` (or `pkgbuild::compile_dll()`) and commit the regenerated `R/RcppExports.R` and `src/RcppExports.cpp`. Never edit them by hand.
- Prefer Rcpp types (`IntegerVector`, `NumericVector`, `DataFrame`, `List`) at the boundary. Inside, use `std::vector` where it is clearer or faster. Write Rcpp-free headers only for code that should also compile standalone (`clumpy_geometry.h`).
- Validate inputs in the R wrapper, where errors are cheap to write and read. C++ may assume what the wrapper guarantees; state the assumption in the doc comment.
- Return data.table-compatible `List`s or `DataFrame`s with column names that match the schema (`id_coord`, `id_lulc`, ...), so the R side needs no renaming.
- Signal errors with `Rcpp::stop("message")`; never `exit()`, `abort()` or uncaught C++ exceptions.
- Print with `Rcpp::Rcout`, never `std::cout`. Progress output is guarded by a `quiet` argument, like the R functions.
- In long loops, call `Rcpp::checkUserInterrupt()` periodically (every ~1000 iterations) so users can cancel.
- Draw random numbers only through R's generator (`R::unif_rand()`, `R::rnorm()`, ...) so `set.seed()` makes runs reproducible. Never use `<random>` or `rand()`.

## Indices and types

- R is 1-based, C++ 0-based. Convert once at the boundary, and name variables to say which is which (`cells_1based`) where both are in play.
- Cell and row indices are `int`, matching R's 32-bit integers. Where a computation could overflow `int` (products of dimensions, squared coordinates), use `long long` or `double` explicitly and comment why.
- Cast with `static_cast<>`, not C-style casts.
- Mark inputs `const` and pass non-trivial objects by `const &`.

## Code organization

- `using namespace Rcpp;` at file scope in `.cpp` files is acceptable; never in headers.
- Helpers not exported to R are `static` (or `inline` in a header) and live in the file that uses them, above their first use. Shared helpers go in a header in a namespace (`namespace clumpy { ... }`).
- Header guards are `#ifndef EVOLAND_<FILE>_H`.
- Include order: own header, `<Rcpp.h>`, then standard library headers alphabetically.

## Naming

- Functions and variables: `snake_case`.
- Types (`struct`, `class`): `PascalCase` (`GridKey`, `SparseColumn`).
- Constants: `snake_case` with `const`/`constexpr`; avoid `#define` for values.

## Comments

- Each exported function gets a Doxygen-style `/** ... */` block: what it computes, each `@param` (with units and 0/1-basing for indices), and `@return` with column names.
- A file implementing an algorithm from the literature opens with a block comment naming the source and equation or section (see `alloc_clumpy.cpp`, `clumpy_geometry.h`), and explaining how the parameters map onto it.
- Comment why, not what, as in R.
