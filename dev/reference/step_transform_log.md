# Log-transform abundances

Apply
[`glyclean::transform_log()`](https://glycoverse.github.io/glyclean/reference/transform_log.html)
to the active experiment. Glyclean converts from the stored abundance
scale and records the new scale. For log-ratio coordinates use
[`step_coda()`](https://glycoverse.github.io/glysmith/dev/reference/step_coda.md).

## Usage

``` r
step_transform_log(base = 2)
```

## Arguments

- base:

  Logarithm base: `2` (default), `exp(1)` for natural logarithms, or
  `10`. These correspond to `"log2"`, `"log"`, and `"log10"` metadata.

## Value

A `glysmith_step` object.

## Examples

``` r
step_transform_log(base = 10)
#> <step "step_transform_log(base = 10)"> Log-transform abundances
```
