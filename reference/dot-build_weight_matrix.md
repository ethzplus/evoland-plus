# Build distance weight matrix for the fuzzy similarity window

Build distance weight matrix for the fuzzy similarity window

## Usage

``` r
.build_weight_matrix(window_size, use_exp_decay, decay_divisor)
```

## Arguments

- window_size:

  Integer, size of window (must be odd)

- use_exp_decay:

  Logical, use exponential decay?

- decay_divisor:

  Numeric, attenuation factor

## Value

Matrix of weights: `exp(-d / decay_divisor)` for Euclidean distance `d`
from the centre cell, or 1 throughout without decay
