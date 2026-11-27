# Correct batch effects

Apply
[`glyclean::auto_correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/auto_correct_batch_effect.html)
to correct batches when the proportion of affected features exceeds its
threshold. Select ComBat or limma to apply
[`glyclean::correct_batch_effect()`](https://glycoverse.github.io/glyclean/reference/correct_batch_effect.html)
directly.

## Usage

``` r
step_correct_batch_effect(method = c("auto", "combat", "limma"), ...)
```

## Arguments

- method:

  Batch correction method, one of `"auto"`, `"combat"` or `"limma"`.

- ...:

  Arguments for the selected method. Automatic correction uses
  `batch_col`, `group_col` and `prop_threshold`. ComBat and limma use
  `batch` and `group`. Both functions accept `check_confounding` and
  `confounding_threshold`.

## Value

A `glysmith_step` object.

## Examples

``` r
step_correct_batch_effect("auto", batch_col = "plate", prop_threshold = 0.2)
#> <step "step_correct_batch_effect(\"auto\", batch_col = \"plate\",
#> prop_threshold = 0.2)"> Correct batch effects (auto)
step_correct_batch_effect("limma", batch = "plate", group = "group")
#> <step "step_correct_batch_effect(\"limma\", batch = \"plate\", group =
#> \"group\")"> Correct batch effects (limma)
```
