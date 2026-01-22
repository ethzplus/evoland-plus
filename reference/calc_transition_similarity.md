# Compute fuzzy similarity of differences for transition validation

Implements the "similarity of differences" approach from Dinamica EGO.
Compares the spatial pattern of a specific transition in observed vs
simulated maps, allowing for spatial tolerance.

## Usage

``` r
calc_transition_similarity(
  initial_map,
  observed_map,
  simulated_map,
  from_class,
  to_class,
  window_size = 11L,
  use_exp_decay = TRUE,
  decay_divisor = 2
)
```

## Arguments

- initial_map:

  SpatRaster, initial LULC state

- observed_map:

  SpatRaster, observed final LULC state

- simulated_map:

  SpatRaster, simulated final LULC state

- from_class:

  Integer, initial class of transition

- to_class:

  Integer, final class of transition

- window_size:

  Integer, size of moving window (must be odd). Default 11.

- use_exp_decay:

  Logical, use exponential decay? Default TRUE.

- decay_divisor:

  Numeric, attenuation factor. Default 2.

## Value

List with:

- observed_change: SpatRaster of observed changes

- simulated_change: SpatRaster of simulated changes

- similarity: Minimum fuzzy similarity between change maps

- sim_obs_to_sim: Mean similarity from observed to simulated

- sim_sim_to_obs: Mean similarity from simulated to observed

- n_observed: Number of cells with observed transition

- n_simulated: Number of cells with simulated transition
