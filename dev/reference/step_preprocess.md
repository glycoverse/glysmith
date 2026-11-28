# Step: Preprocessing

Preprocess the experiment using
[`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html).
This step can be omitted if the experiment is already preprocessed.

This step requires `exp` (experiment data).

## Usage

``` r
step_preprocess(
  batch_col = "batch",
  normalize_to_try = NULL,
  impute_to_try = NULL,
  remove_preset = "discovery",
  batch_prop_threshold = 0.3,
  check_batch_confounding = TRUE,
  batch_confounding_threshold = 0.4,
  rep_col = NULL,
  group_col = "group",
  standardize_variable = TRUE,
  seed = 123
)
```

## Arguments

- batch_col:

  Column name for batch information (for QC plots and batch effect
  handling).

- normalize_to_try:

  Legacy argument for versions of `auto_clean()` that accept candidate
  methods. Use
  [`step_normalize()`](https://glycoverse.github.io/glysmith/dev/reference/step_normalize.md)
  to select a method.

- impute_to_try:

  Legacy argument for versions of `auto_clean()` that accept candidate
  methods. Use
  [`step_impute()`](https://glycoverse.github.io/glysmith/dev/reference/step_impute.md)
  to select a method.

- remove_preset:

  Preset for data removal: "discovery", "biomarker", or NULL.

- batch_prop_threshold:

  Threshold for batch proportion filtering.

- check_batch_confounding:

  Whether to check for batch confounding.

- batch_confounding_threshold:

  Threshold for batch confounding detection.

- rep_col:

  Column name for replicate information (for QC plots).

- group_col:

  The column name in sample_info for groups. Default is "group". Can be
  NULL when no group information is available.

- standardize_variable:

  Whether to call
  [`glyexp::standardize_variable()`](https://glycoverse.github.io/glyexp/reference/standardize_variable.html)
  after aggregation. Set to `FALSE` to skip network calls for faster
  testing. Default is `TRUE`.

- seed:

  Non-negative integer seed for random number generation. Default is
  `123`. The caller's random state is restored, including on errors.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to preprocess

Data generated:

- `raw_exp`: The input to this preprocessing step

Subsequent steps read the processed experiment from `exp`.

## See also

[`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html),
[`step_remove()`](https://glycoverse.github.io/glysmith/dev/reference/step_remove.md),
[`step_normalize()`](https://glycoverse.github.io/glysmith/dev/reference/step_normalize.md),
[`step_impute()`](https://glycoverse.github.io/glysmith/dev/reference/step_impute.md),
[`step_aggregate()`](https://glycoverse.github.io/glysmith/dev/reference/step_aggregate.md),
[`step_correct_batch_effect()`](https://glycoverse.github.io/glysmith/dev/reference/step_correct_batch_effect.md)

## Examples

``` r
step_preprocess()
#> <step "step_preprocess()"> Preprocessing
step_preprocess(remove_preset = "discovery")
#> <step "step_preprocess(remove_preset = \"discovery\")"> Preprocessing
```
