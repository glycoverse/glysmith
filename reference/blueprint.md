# Create a Blueprint

A blueprint is a list of steps that are executed in order.

## Usage

``` r
blueprint(...)
```

## Arguments

- ...:

  One or more step objects.

## Value

A blueprint object.

## Examples

``` r
blueprint(
  step_preprocess(),
  step_pca(),
  step_dea(),  # this comma is ok
)
#> 
#> ── Blueprint (3 steps) ──
#> 
#> • preprocess
#> • pca
#> • dea
```
