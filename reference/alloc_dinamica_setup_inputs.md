# Private helper: Set up input files for a single Dinamica allocation iteration

Private helper: Set up input files for a single Dinamica allocation
iteration

## Usage

``` r
alloc_dinamica_setup_inputs(
  self,
  id_period_ant,
  id_period_post,
  id_perturbation,
  anterior_rast,
  temp_dir
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

  Integer, perturbation ID for selecting allocation parameters

- anterior_rast:

  SpatRast with anterior LULC state

- temp_dir:

  Character, path to temporary directory

## Value

List with paths to created files
