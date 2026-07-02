# Normal patch-area sampler (C++)

Normal patch-area sampler (C++)

## Usage

``` r
sample_normal_area_cpp(area_mean, area_var)
```

## Arguments

- area_mean:

  Mean patch area (cells); NA / \<= 0 returns 1.

- area_var:

  Patch-area variance (cells^2); sd = sqrt(area_var); the draw is
  clamped to \>= 1.

## Value

Integer \>= 1.
