# Initialize Allocation Parameters Table

Computes allocation parameters for all viable transitions, aggregated
across observed periods and then randomly perturbed N times. This is a
method on `evoland_db` that analyzes patch dynamics to determine
expansion vs. patcher behavior for the Dinamica allocation procedure.

The method first computes parameters for all viable transitions across
observed periods (where `id_period > 1` and `is_extrapolated == FALSE`),
then aggregates (mean) these parameters by transition. Finally, it
creates randomly perturbed versions of these aggregated parameters and
returns them together with the original estimate, i.e. the result set
size is (n viable transitions) \* (m perturbations + 1)

## Arguments

- n_perturbations:

  Integer number of perturbed parameter sets to generate per transition
  (default: 5)

- sd:

  Standard deviation for random perturbation of frac_expander as a
  fraction (default: 0.05)

## Value

Invisibly returns the computed `alloc_params_t` table.

## Details

The workflow is:

1.  For each transition and period pair:

    - Create rasters for the anterior and posterior periods

    - Identify transition cells (cells that changed from anterior to
      posterior class)

    - Use focal operations to determine if transition cells are adjacent
      to existing patches (expansion) or form new patches (patcher
      behavior)

    - Compute patch statistics using internal C++ implementation

2.  Aggregate parameters across periods (mean) for each transition

3.  For each transition, create N randomly perturbed versions:

    - Add random noise to frac_expander (normal distribution, mean=0,
      sd=sd)

    - Clamp frac_expander to \[0, 1\]

    - Recalculate frac_patcher as 1 - frac_expander

4.  Store all perturbed versions in the `alloc_params_t` table

## Requirements

- `coords_t` must have `resolution` and `epsg` metadata

- `trans_meta_t` must have at least one viable transition

- `periods_t` must have at least one observed period with
  `id_period > 1`

## Examples

``` r
if (FALSE) { # \dontrun{
db <- evoland_db$new("path/to/db")
# ... populate with coords, periods, lulc_data, trans_meta ...
db$create_alloc_params_t()
} # }
```
