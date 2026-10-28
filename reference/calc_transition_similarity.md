# Compute fuzzy similarity of differences for transition validation

Implements the "similarity of differences" approach from Dinamica EGO
(Soares-Filho et al. 2009), after the fuzzy set comparison of Hagen
(2003). Compares the spatial pattern of a specific transition in
observed vs simulated maps, allowing for spatial tolerance.

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

- observed_change: Logical SpatRaster, `TRUE` where the transition was
  observed

- simulated_change: Logical SpatRaster, `TRUE` where the transition was
  simulated

- similarity: Minimum of the two directional similarities; `NA` if the
  transition happened in neither map

- similarity_observed_to_simulated: Mean membership of observed changes
  in the simulated change

- similarity_simulated_to_observed: Mean membership of simulated changes
  in the observed change

- similarity_map: SpatRaster of the observed-to-simulated membership at
  each observed change (`NA` elsewhere)

- n_observed: Number of cells with observed transition

- n_simulated: Number of cells with simulated transition

## Details

Both maps are reduced to where the transition happened. For each cell
that changed in one map, its membership in the other map's change is the
distance decay to the nearest cell that changed there, within the
window: 1 for a change in the same cell, `exp(-d / decay_divisor)` for
one `d` cells away (or 1 anywhere in the window without decay), 0 if
there is none. The directional similarity is the mean of these
memberships over the changed cells of the first map only; cells that
changed in neither map do not enter it. The overall similarity is the
minimum of the two directions, so a simulation cannot score well by
placing much more (or much less) change than was observed.

## References

Hagen, A. (2003). Fuzzy set approach to assessing similarity of
categorical maps. International Journal of Geographical Information
Science, 17(3), 235-249. https://doi.org/10.1080/13658810210157822

Soares-Filho, B. S., Rodrigues, H. O., & Costa, W. L. (2009). Modeling
Environmental Dynamics with Dinamica EGO. Centro de Sensoriamento
Remoto, UFMG.
