# Step: Preprocessing

Preprocess the experiment using
[`glyclean::auto_clean()`](https://glycoverse.github.io/glyclean/reference/auto_clean.html).
Optionally run QC plots before and/or after preprocessing. This step can
be omitted if the experiment is already preprocessed.

## Usage

``` r
step_preprocess(
  pre_qc = FALSE,
  post_qc = TRUE,
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

- pre_qc:

  Whether to run QC plots before preprocessing.

- post_qc:

  Whether to run QC plots after preprocessing.

- batch_col:

  The column name in sample_info for batches. Default is "batch". Can be
  NULL when no batch information is available.

- qc_name:

  The name of QC samples in the `group_col` column. Default is "QC".
  Only used when `group_col` is not NULL.

- normalize_to_try:

  Normalization functions to try. A list. Default includes:

  - [`normalize_median()`](https://glycoverse.github.io/glyclean/reference/normalize_median.html):
    median normalization

  - [`normalize_median_abs()`](https://glycoverse.github.io/glyclean/reference/normalize_median_abs.html):
    absolute median normalization

  - [`normalize_total_area()`](https://glycoverse.github.io/glyclean/reference/normalize_total_area.html):
    total area mormalization

  - [`normalize_quantile()`](https://glycoverse.github.io/glyclean/reference/normalize_quantile.html):
    quantile normalization

  - [`normalize_loessf()`](https://glycoverse.github.io/glyclean/reference/normalize_loessf.html):
    LoessF normalization

  - [`normalize_loesscyc()`](https://glycoverse.github.io/glyclean/reference/normalize_loesscyc.html):
    LoessCyc normalization

  - [`normalize_rlr()`](https://glycoverse.github.io/glyclean/reference/normalize_rlr.html):
    RLR normalization

  - [`normalize_rlrma()`](https://glycoverse.github.io/glyclean/reference/normalize_rlrma.html):
    RLRMA normalization

  - [`normalize_rlrmacyc()`](https://glycoverse.github.io/glyclean/reference/normalize_rlrmacyc.html):
    RLRMAcyc normalization

- impute_to_try:

  Imputation functions to try. A list. Default includes:

  - [`impute_zero()`](https://glycoverse.github.io/glyclean/reference/impute_zero.html):
    zero imputation

  - [`impute_sample_min()`](https://glycoverse.github.io/glyclean/reference/impute_sample_min.html):
    sample-wise minimum imputation

  - [`impute_half_sample_min()`](https://glycoverse.github.io/glyclean/reference/impute_half_sample_min.html):
    half sample-wise minimum imputation

  - [`impute_sw_knn()`](https://glycoverse.github.io/glyclean/reference/impute_sw_knn.html):
    sample-wise KNN imputation

  - [`impute_fw_knn()`](https://glycoverse.github.io/glyclean/reference/impute_fw_knn.html):
    feature-wise KNN imputation

  - [`impute_bpca()`](https://glycoverse.github.io/glyclean/reference/impute_bpca.html):
    BPCA imputation

  - [`impute_ppca()`](https://glycoverse.github.io/glyclean/reference/impute_ppca.html):
    PPCA imputation

  - [`impute_svd()`](https://glycoverse.github.io/glyclean/reference/impute_svd.html):
    SVD imputation

  - [`impute_min_prob()`](https://glycoverse.github.io/glyclean/reference/impute_min_prob.html):
    minimum probability imputation

  - [`impute_miss_forest()`](https://glycoverse.github.io/glyclean/reference/impute_miss_forest.html):
    MissForest imputation

- remove_preset:

  The preset for removing variables. Default is "discovery". Available
  presets:

  - "simple": remove variables with more than 50% missing values.

  - "discovery": more lenient, remove variables with more than 80%
    missing values, but ensure less than 50% of missing values in at
    least one group.

  - "biomarker": more strict, remove variables with more than 40%
    missing values, and ensure less than 60% of missing values in all
    groups.

- batch_prop_threshold:

  The proportion of variables that must show significant batch effects
  to perform batch correction. Default is 0.3 (30%).

- check_batch_confounding:

  Whether to check for confounding between batch and group variables.
  Default to TRUE.

- batch_confounding_threshold:

  The threshold for Cramer's V to consider batch and group variables
  highly confounded. Only used when `check_batch_confounding` is TRUE.
  Default to 0.4.

- rep_col:

  Column name for replicate information (for
  [`glyclean::plot_rep_scatter()`](https://glycoverse.github.io/glyclean/reference/plot_rep_scatter.html)).

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to preprocess

Data generated:

- `raw_exp`: The raw experiment (previous `exp`, saved for reference)

Plots generated when `post_qc = TRUE`:

- `qc_missing_heatmap`: Missing value heatmap

- `qc_missing_samples_bar`: Missing value bar plot on samples

- `qc_missing_variables_bar`: Missing value bar plot on variables

- `qc_tic_bar`: Total intensity count bar plot

- `qc_rank_abundance`: Rank abundance plot

- `qc_int_boxplot`: Intensity boxplot

- `qc_rle`: RLE plot

- `qc_cv_dent`: CV density plot

- `qc_batch_pca`: PCA score plot colored by batch (if `batch_col`
  provided)

- `qc_rep_scatter`: Replicate scatter plots (if `rep_col` provided)

When `pre_qc = TRUE`, the same plots are generated with the `qc_pre_`
prefix.

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
