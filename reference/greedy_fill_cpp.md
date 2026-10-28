# Greedy rank-and-fill walk

Walks candidate (cell, transition) pairs in the given order and accepts
a pair when its cell has not been claimed yet and its transition still
has quota left. The caller decides the order (by potential across all
transitions, or by transition priority, then potential), so this kernel
only enforces "one change per cell" and "no more than the demanded
count". See greedy_fill() for the R side.

## Usage

``` r
greedy_fill_cpp(cell, trans, quota, n_cells)
```

## Arguments

- cell:

  Integer vector, cell of each candidate as a positive integer id
  (`id_coord`), at most `n_cells`.

- trans:

  Integer vector, transition index of each candidate, 1-based into
  `quota`.

- quota:

  Integer vector, number of cells each transition may claim.

- n_cells:

  Integer, the largest cell id; sizes the array of claimed cells.

## Value

Logical vector, `TRUE` for the accepted candidates.
