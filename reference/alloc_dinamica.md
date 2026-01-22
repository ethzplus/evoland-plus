# Dinamica EGO Allocation Methods

Methods for running Dinamica EGO allocation simulations and evaluating
allocation parameters. These methods are added to the `evoland_db`
class.

Runs a path-dependent Monte Carlo simulation using Dinamica EGO for land
use allocation. Iterates through a sequence of contiguous periods, using
the simulated output from one period as the input to the next.

## Arguments

- id_periods:

  Integer vector of contiguous period IDs to simulate. Must be in
  sequential order. The first period is used as the origin state.

- id_perturbation:

  Integer, perturbation ID for selecting allocation parameters from
  `alloc_params_t`

- work_dir:

  Character, base directory for Dinamica runs. A subdirectory will be
  created for this simulation. Default: "dinamica_rundir"

- keep_intermediate:

  Logical, keep intermediate files after successful completion? Default:
  FALSE

## Value

Invisibly returns the table name where results were written (e.g.,
"lulc_data_t_perturbation_1")

## Requirements

- `trans_models_t` must have full models fitted

- `alloc_params_t` must exist with specified `id_perturbation`

- `periods_t` must contain all specified `id_periods`

- Dinamica EGO must be installed and `DinamicaConsole` must be on PATH

## Examples

``` r
if (FALSE) { # \dontrun{
db <- evoland_db$new("path/to/db")
# Simulate historical periods with perturbation 1
db$alloc_dinamica(
  id_periods = 1:3,
  id_perturbation = 1L
)
} # }
```
