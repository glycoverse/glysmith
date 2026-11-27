# Impute missing abundances

Use
[`glyclean::auto_impute()`](https://glycoverse.github.io/glyclean/reference/auto_impute.html)
or select an `impute_*` function by its suffix. The imputed experiment
becomes the input to the next step.

## Usage

``` r
step_impute(
  method = c("auto", "zero", "sample_min", "half_sample_min", "sw_knn", "fw_knn", "bpca",
    "ppca", "svd", "min_prob", "miss_forest", "dreamai"),
  ...,
  dreamai_methods = NULL
)
```

## Arguments

- method:

  Imputation method. `"auto"` selects by experiment type and sample
  count. Explicit methods are `"zero"`, `"sample_min"`,
  `"half_sample_min"`, `"sw_knn"`, `"fw_knn"`, `"bpca"`, `"ppca"`,
  `"svd"`, `"min_prob"`, `"miss_forest"` and `"dreamai"`. DreamAI
  requires a glyclean installation that exports `impute_dreamai()`.

- ...:

  Arguments for the selected glyclean function, such as `k` for
  nearest-neighbor imputation, or `by`, `q`, `tune.sigma` and `seed` for
  [`glyclean::impute_min_prob()`](https://glycoverse.github.io/glyclean/reference/impute_min_prob.html).

- dreamai_methods:

  Components passed to the DreamAI ensemble's `method` argument. The
  default uses glyclean's choice of components.

## Value

A `glysmith_step` object.

## Examples

``` r
step_impute("half_sample_min")
#> <step "step_impute(\"half_sample_min\")"> Impute missing abundances
#> (half_sample_min)
step_impute("min_prob", by = "group", q = 0.02, seed = 42)
#> <step "step_impute(\"min_prob\", by = \"group\", q = 0.02, seed = 42)"> Impute
#> missing abundances (min_prob)
```
