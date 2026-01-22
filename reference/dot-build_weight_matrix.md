# Build distance weight matrix for fuzzy similarity

Build distance weight matrix for fuzzy similarity

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

Matrix of weights
