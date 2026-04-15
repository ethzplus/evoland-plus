# Dinamica EGO Allocation Methods

Methods for running Dinamica EGO allocation simulations and evaluating
allocation parameters. These methods are added to the `evoland_db`
class.

## Usage

``` r
alloc_dinamica_setup_inputs(
  self,
  id_period_ant,
  id_period_post,
  anterior_rast,
  temp_dir,
  gof_criterion,
  gof_maximize
)

alloc_dinamica_one_period(
  self,
  id_period_ant,
  id_period_post,
  anterior_rast,
  iteration_dir,
  gof_criterion,
  gof_maximize
)

alloc_dinamica(
  self,
  id_periods,
  gof_criterion,
  gof_maximize,
  work_dir = "dinamica_rundir",
  keep_intermediate = FALSE
)

eval_alloc_params_t(
  self,
  gof_criterion,
  gof_maximize,
  work_dir = "dinamica_rundir",
  keep_intermediate = FALSE
)
```

## Arguments

- self:

  The evoland_db instance

- id_period_ant:

  Integer, anterior period ID

- id_period_post:

  Integer, posterior period ID

- anterior_rast:

  SpatRast with anterior LULC state

- temp_dir:

  Character, path to temporary directory

- iteration_dir:

  Character, path to iteration directory

- id_periods:

  Integer vector of posterior period IDs to simulate (must be
  contiguous; e.g. if simulating period 4, data from period 3 will be
  used as anterior data)

- work_dir:

  Character, path to working directory for simulations

- keep_intermediate:

  Logical, whether to keep intermediate files from simulations

## Value

List with paths to created files

lulc_data_t table with simulated results

## Functions

- `alloc_dinamica_setup_inputs()`: Private helper: Set up input files
  for a single Dinamica allocation iteration

- `alloc_dinamica_one_period()`: Private helper: Run a single Dinamica
  allocation iteration

- `alloc_dinamica()`: Run Dinamica EGO allocation over multiple periods

- `eval_alloc_params_t()`: Evaluate allocation parameters using fuzzy
  similarity over different runs.
