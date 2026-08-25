# R6 Class for Sparse Linear Programs

Collects the constraints of a linear program in long form and hands them
to [`lpSolve::lp()`](https://rdrr.io/pkg/lpSolve/man/lp.html) in triplet
form, which keeps memory linear in the number of non-zero coefficients.
Constraints are tagged with the block they belong to, so a program can
be assembled once and solved over a subset of its blocks.

## Details

Two tables describe a program.

`variables` has one row per decision variable: an `id_var` giving its
column in the program, a `block` naming the group it belongs to, and
whatever further key columns the caller needs to identify it. Nothing
about the meaning of a variable is known here; the subclass supplies it.

`constraints` has one row per non-zero coefficient: `block`, `id_row`,
`id_var`, `coefficient`, plus the `dir` and `rhs` of the row it belongs
to, which are constant within an `id_row`. Coefficients are summed over
duplicated `(id_row, id_var)` pairs and exact zeroes are dropped, since
[`lpSolve::lp()`](https://rdrr.io/pkg/lpSolve/man/lp.html) matches
constraint rows to `dir`/`rhs` by the order of the row indices it is
given, and cannot represent an empty row.

All variables are non-negative:
[`lpSolve::lp()`](https://rdrr.io/pkg/lpSolve/man/lp.html) has no notion
of variable bounds, so a quantity that may take either sign has to be
split into two variables.

`lpSolve` is a suggested dependency, since most of the package does not
solve linear programs. It has to be installed before a program can be
constructed.

## See also

[trans_rate_lp](https://ethzplus.github.io/evoland-plus/reference/trans_rate_lp.md)

## Active bindings

- `variables`:

  The decision variables, one row each.

- `constraints`:

  The constraint coefficients, one row per non-zero entry.

- `block_summary`:

  Rows and coefficients per constraint block.

- `n_var`:

  Number of decision variables.

- `n_row`:

  Number of constraint rows.

- `status`:

  The [`lpSolve::lp()`](https://rdrr.io/pkg/lpSolve/man/lp.html) status
  of the last solve; 0 is success.

- `objective`:

  The objective value of the last solve.

- `values`:

  The solved value of every variable, joined to its keys.

## Methods

### Public methods

- [`lp_problem$new()`](#method-lp_problem-initialize)

- [`lp_problem$add_constraints()`](#method-lp_problem-add_constraints)

- [`lp_problem$solve()`](#method-lp_problem-solve)

- [`lp_problem$print()`](#method-lp_problem-print)

- [`lp_problem$clone()`](#method-lp_problem-clone)

------------------------------------------------------------------------

### `lp_problem$new()`

Initialize a program over a fixed set of decision variables. Fails if
the suggested `lpSolve` package is not installed, since nothing could be
solved.

#### Usage

    lp_problem$new(variables)

#### Arguments

- `variables`:

  A data.table with `id_var` (`1:n`, in order) and `block`, plus any key
  columns identifying each variable.

#### Returns

A new `lp_problem` object

------------------------------------------------------------------------

### `lp_problem$add_constraints()`

Add one block of constraint rows.

#### Usage

    lp_problem$add_constraints(block, constraints)

#### Arguments

- `block`:

  Name of the block, used to subset the program when solving.

- `constraints`:

  A data.table with `id_row`, `id_var`, `coefficient`, `dir` and `rhs`.
  `id_row` only has to be unique within this call; it is renumbered to
  stay unique across blocks. `dir` and `rhs` must be constant within an
  `id_row`.

#### Returns

The `lp_problem` object, invisibly

------------------------------------------------------------------------

### `lp_problem$solve()`

Solve the program and store the solution.

#### Usage

    lp_problem$solve(objective, direction = "min", blocks = NULL)

#### Arguments

- `objective`:

  A data.table with `id_var` and `coefficient`; variables it does not
  mention do not enter the objective.

- `direction`:

  `"min"` or `"max"`.

- `blocks`:

  Constraint blocks to include; all of them when `NULL`.

#### Returns

The `lp_problem` object, invisibly

------------------------------------------------------------------------

### `lp_problem$print()`

Print a summary of the program.

#### Usage

    lp_problem$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `lp_problem$clone()`

The objects of this class are cloneable with this method.

#### Usage

    lp_problem$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
