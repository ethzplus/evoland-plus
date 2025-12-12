# Dinamica Utility Functions

Interact with Dinamica from R, see **Functions** section below.

## Usage

``` r
exec_dinamica(
  model_path,
  disable_parallel = TRUE,
  log_level = NULL,
  additional_args = NULL,
  write_logfile = TRUE,
  echo = FALSE
)

run_evoland_dinamica_sim(
  run_modelprechecks = TRUE,
  config = get_config(),
  work_dir = format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"),
  calibration = FALSE,
  ...
)

process_dinamica_script(infile, outfile, mode = "encode", check = TRUE)
```

## Arguments

- model_path:

  Path to the .ego model file to run. Any submodels must be included in
  a directory of the exact form `basename(modelpath)_ego_Submodels`,
  [see
  wiki](https://csr.ufmg.br/dinamica/dokuwiki/doku.php?id=submodels)

- disable_parallel:

  Whether to disable parallel steps (default TRUE)

- log_level:

  Logging level (1-7, default NULL)

- additional_args:

  Additional arguments to pass to DinamicaConsole, see
  `DinamicaConsole -help`

- write_logfile:

  bool, write stdout&stderr to a file?

- echo:

  bool, direct echo to console?

- run_modelprechecks:

  bool, Validate that everything's in place for a model run. Will never
  be run if calibration.

- config:

  List of config params

- work_dir:

  Working dir, where to place ego files and control table

- calibration:

  bool, Is this a calibration run?

- ...:

  passed to `exec_dinamica()`

- infile:

  Input file path. Treated as input if passed AsIs using
  [`base::I()`](https://rdrr.io/r/base/AsIs.html)

- outfile:

  Output file path (optional)

- mode:

  Character, either "encode" or "decode"

- check:

  Default TRUE, simple check to ensure that you're handling what you're
  expecting

## Functions

- `exec_dinamica()`: Execute a Dinamica .ego file using
  `DinamicaConsole`

- `run_evoland_dinamica_sim()`: Set up evoland-specific Dinamica EGO
  files; execute using `exec_dinamica()`

- `process_dinamica_script()`: Encode or decode raw R and Python code
  chunks in .ego files and their submodels to/from base64
