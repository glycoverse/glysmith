# Step: Preprocessing

Preprocess the experiment using
[`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html).
This step can be omitted if the experiment is already preprocessed.

## Usage

``` r
step_preprocess(...)
```

## Arguments

- ...:

  Step-specific arguments passed to underlying functions. Use the format
  `pkg.func.arg`. For example,
  `step_preprocess(glyclean.auto_clean.remove_preset = "discovery")`.

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

## Dynamic Arguments

This step supports the following dynamic arguments:

- `glyclean.auto_clean.batch_col`: Column name for batch information.

- `glyclean.auto_clean.qc_name`: Name of QC samples (default: "QC").

- `glyclean.auto_clean.normalize_to_try`: List of normalization
  functions to try.

- `glyclean.auto_clean.impute_to_try`: List of imputation functions to
  try.

- `glyclean.auto_clean.remove_preset`: Preset for removing variables
  ("simple", "discovery", "biomarker").

- `glyclean.auto_clean.batch_prop_threshold`: Proportion threshold for
  batch correction (default: 0.3).

- `glyclean.auto_clean.check_batch_confounding`: Whether to check batch
  confounding (default: TRUE).

- `glyclean.auto_clean.batch_confounding_threshold`: Threshold for batch
  confounding (default: 0.4).

## See also

[`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html)

## Examples

``` r
step_preprocess()
#> <step "step_preprocess()"> Preprocessing
```
