# Log-normal patch-area sampler (C++)

Log-normal patch-area sampler (C++)

## Usage

``` r
sample_lognorm_area_cpp(area_mean, area_var)
```

## Arguments

- area_mean:

  Mean patch area (cells); NA / \<= 0 returns 1.

- area_var:

  Patch-area variance (cells^2); NA / \<= 0 treated as 1.

## Value

Integer \>= 1.
