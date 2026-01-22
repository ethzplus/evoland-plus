# Evaluate Allocation Parameters with Fuzzy Similarity

Evaluates allocation parameters by running simulations over historical
periods and comparing results against observed data using fuzzy
similarity. For each perturbation:

1.  Runs
    [`alloc_dinamica()`](https://ethzplus.github.io/evoland-plus/reference/alloc_dinamica.md)
    using historical periods to produce simulated final period

2.  Compares initial vs final observed and initial vs final simulated
    using fuzzy similarity

3.  Returns `alloc_params_t` augmented with per-transition similarity
    metrics

## Arguments

- id_perturbations:

  Integer vector of perturbation IDs to evaluate. If NULL (default),
  evaluates all perturbations in `alloc_params_t`.

- work_dir:

  Character path for Dinamica working directory. Default
  "dinamica_rundir".

- keep_intermediate:

  Logical, keep intermediate Dinamica files? Default FALSE.

## Details

The evaluation uses fuzzy similarity with spatial tolerance (11x11
window, exponential decay with divisor=2). For each transition, compares
the spatial pattern of changes from initial to final period between
observed and simulated.

Returns the `alloc_params_t` table augmented with:

- `similarity`: Fuzzy similarity per transition (0-1, NA if no observed
  transitions)

## Examples

``` r
if (FALSE) { # \dontrun{
db <- evoland_db$new("path/to/db")
# Evaluate all perturbations
evaluated_params <- db$eval_alloc_params_t()

# Evaluate specific perturbations
evaluated_params <- db$eval_alloc_params_t(id_perturbations = 1:3)
} # }
```
