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
  rep_col = NULL
)
```

## Arguments

- batch_col:

  Column name for batch information (for QC plots and batch effect
  handling).

- normalize_to_try:

  Normalization methods to try during auto_clean.

- impute_to_try:

  Imputation methods to try during auto_clean.

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

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to preprocess

Data generated:

- `raw_exp`: The raw experiment (previous `exp`, saved for reference)

This step is special in that it silently overwrites the `exp` data with
the preprocessed experiment. This ensures that no matter if
preprocessing is performed or not, the "active" experiment is always
under the key `exp`. The previous `exp` is saved as `raw_exp` for
reference.

## See also

[`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html)

## Examples

``` r
step_preprocess()
#> <step "step_preprocess()"> Preprocessing
step_preprocess(remove_preset = "discovery")
#> <step "step_preprocess(remove_preset = \"discovery\")"> Preprocessing
```
