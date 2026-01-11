# Step: Preprocessing

Preprocess the experiment using
[`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html).
This step can be omitted if the experiment is already preprocessed.

## Usage

``` r
step_preprocess(
  batch_col = "batch",
  qc_name = "QC",
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

- qc_name:

  Name of QC sample group (used for QC sample detection in
  preprocessing).

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

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- Always include this step by default unless the user explicitly
  excludes it or tell you she/he has already performed preprocessing.

- Ask for the column name for batch information if not provided.

- Ask for QC samples in the experiment if not provided. If so, ask the
  group name of the QC samples. Explain to the user that if it is "QC"
  for example, the samples with "QC" in the `group_col` column will be
  considered as QC samples. And these QC samples will be used for
  choosing the best normalization and imputation methods. Also mention
  that QC samples will be excluded after preprocessing.

- If the user intents to perform biomarker related analysis, set
  `remove_preset` to "biomarker".

- Use default values for other arguments unless the user explicitly
  specifies otherwise.

## See also

[`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html)

## Examples

``` r
step_preprocess()
#> <step "step_preprocess()"> Preprocessing
step_preprocess(remove_preset = "discovery")
#> <step "step_preprocess(remove_preset = \"discovery\")"> Preprocessing
```
