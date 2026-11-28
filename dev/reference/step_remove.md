# Filter features by abundance or missingness

Use
[`glyclean::auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.html)
or select a `remove_*` function by its suffix. Chain filters in the
order in which their criteria should apply.

## Usage

``` r
step_remove(
  method = c("auto", "rare", "constant", "low_var", "low_cv", "low_expr"),
  ...
)
```

## Arguments

- method:

  Filtering method. Use `"auto"` for a preset, `"rare"` for missingness,
  `"constant"` for invariant features, or `"low_var"`, `"low_cv"` and
  `"low_expr"` for variance, coefficient of variation and abundance
  thresholds.

- ...:

  Arguments for the selected filter. For example,
  [`glyclean::remove_rare()`](https://glycoverse.github.io/glyclean/reference/remove_rare.html)
  takes `prop`, `n`, `min_n`, `by` and `strict`.
  [`glyclean::auto_remove()`](https://glycoverse.github.io/glyclean/reference/auto_remove.html)
  takes `preset` and `group_col`.

## Value

A `glysmith_step` object.

## Examples

``` r
blueprint(step_remove("rare", prop = 0.4) %=>% step_remove("constant"))
#> Blueprint (root accepts exp)
#> └─ node_1  step_remove("rare", prop = 0.4) (exp)
#>   └─ node_2  step_remove("constant") (exp)
```
