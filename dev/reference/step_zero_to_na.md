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
blueprint(step_zero_to_na() %=>% step_remove("rare") %=>% step_impute("min_prob"))
#> Blueprint (root accepts exp)
#> └─ node_1  step_zero_to_na() (exp)
#>   └─ node_2  step_remove("rare") (exp)
#>     └─ node_3  step_impute("min_prob") (exp)
```
