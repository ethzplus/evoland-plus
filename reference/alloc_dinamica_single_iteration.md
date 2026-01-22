# Private helper: Run a single Dinamica allocation iteration

Private helper: Run a single Dinamica allocation iteration

## Usage

``` r
alloc_dinamica_single_iteration(
  self,
  id_period_ant,
  id_period_post,
  id_perturbation,
  anterior_rast,
  iteration_dir
)
```

## Arguments

- self:

  The evoland_db instance

- id_period_ant:

  Integer, anterior period ID

- id_period_post:

  Integer, posterior period ID

- id_perturbation:

  Integer, perturbation ID

- anterior_rast:

  SpatRast with anterior LULC state

- iteration_dir:

  Character, path to iteration directory

## Value

lulc_data_t table with simulated results
