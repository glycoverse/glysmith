# Test for batch effects in each feature

Run
[`glyclean::detect_batch_effect()`](https://glycoverse.github.io/glyclean/reference/detect_batch_effect.html)
on the active experiment and save the feature identifiers (`.variable`)
and ANOVA p-values (`p_value`) in the `batch_effect` result table.

## Usage

``` r
step_detect_batch_effect(batch = "batch", group = NULL)
```

## Arguments

- batch:

  Either a factor/character vector specifying batch assignments for each
  sample, or a string specifying the column name in `sample_info`.
  Defaults to "batch".

- group:

  Either a factor/character vector specifying group assignments for each
  sample, or a string specifying the column name in `sample_info`. If
  provided, it will be used as a covariate in the ANOVA model. This is
  useful when you have an unbalanced design. Default to NULL.

## Value

A `glysmith_step` object.

## Examples

``` r
step_detect_batch_effect(batch = "plate", group = "group")
#> <step "step_detect_batch_effect(batch = \"plate\", group = \"group\")"> Detect
#> batch effects
```
