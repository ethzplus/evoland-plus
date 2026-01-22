# Compute similarity map from fuzzy neighborhoods

Compute similarity map from fuzzy neighborhoods

## Usage

``` r
.compute_similarity_map(fuzzy_a, fuzzy_b, crisp_a, ignore_na)
```

## Arguments

- fuzzy_a:

  List of fuzzy neighborhood rasters for map A

- fuzzy_b:

  List of fuzzy neighborhood rasters for map B

- crisp_a:

  SpatRaster, original categorical map A

- ignore_na:

  Logical, ignore NA values?

## Value

SpatRaster of similarity values (0 to 1)
