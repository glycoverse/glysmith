# Convert zero abundances to missing values

Apply
[`glyclean::zero_to_na()`](https://glycoverse.github.io/glyclean/reference/zero_to_na.html)
to the active experiment before filtering or imputing measurements
encoded as zero.

## Usage

``` r
step_zero_to_na()
```

## Value

A `glysmith_step` object.

## Examples

``` r
blueprint(step_zero_to_na(), step_remove("rare"), step_impute("min_prob"))
#> 
#> ── Blueprint (3 steps) ──
#> 
#> • step_zero_to_na()
#> • step_remove("rare")
#> • step_impute("min_prob")
```
