# Normalize sample abundances

Use
[`glyclean::auto_normalize()`](https://glycoverse.github.io/glyclean/reference/auto_normalize.html)
or select a `normalize_*` function by its suffix. The step updates the
active experiment (`exp`). Each cleaning step saves its input with its
node ID and the suffix `__raw_exp`.

## Usage

``` r
step_normalize(
  method = c("auto", "median", "median_abs", "median_quotient", "total_area", "quantile",
    "loessf", "loesscyc", "rlr", "rlrma", "rlrmacyc", "vsn"),
  ...
)
```

## Arguments

- method:

  Use `"auto"` to choose by experiment type, or select `"median"`,
  `"median_abs"`, `"median_quotient"`, `"total_area"`, `"quantile"`,
  `"loessf"`, `"loesscyc"`, `"rlr"`, `"rlrma"`, `"rlrmacyc"` or `"vsn"`.

- ...:

  Arguments for the selected glyclean function. For example,
  [`glyclean::normalize_median_quotient()`](https://glycoverse.github.io/glyclean/reference/normalize_median_quotient.html)
  accepts `by` to normalize within groups, and
  [`glyclean::normalize_rlrmacyc()`](https://glycoverse.github.io/glyclean/reference/normalize_rlrmacyc.html)
  accepts `n_iter`.

## Value

A `glysmith_step` object.

## Examples

``` r
blueprint(step_normalize("median_quotient", by = "group") %=>% step_pca())
#> Blueprint (root accepts exp)
#> └─ node_1  step_normalize("median_quotient", by = "group") (exp)
#>   └─ node_2  step_pca() (terminal)
```
