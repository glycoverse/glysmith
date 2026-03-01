# Step: Plot QC

Generate quality control plots for the experiment using `glyclean`
plotting functions. This step can be used before AND after
[`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md)
to generate QC plots at different stages.

This step requires `exp` (experiment data).

## Usage

``` r
step_plot_qc(
  when = "post",
  batch_col = "batch",
  rep_col = NULL,
  plot_width = 7,
  plot_height = 5
)
```

## Arguments

- when:

  Character string indicating when this QC step is run. Use `"pre"` for
  pre-preprocessing QC, `"post"` for post-preprocessing QC, or any other
  value for generic QC. This is appended to the step ID. Default is
  `"post"`.

- batch_col:

  Column name for batch information (for
  [`glyclean::plot_batch_pca()`](https://glycoverse.github.io/glyclean/reference/plot_batch_pca.html)).

- rep_col:

  Column name for replicate information (for
  [`glyclean::plot_rep_scatter()`](https://glycoverse.github.io/glyclean/reference/plot_rep_scatter.html)).

- plot_width:

  Width of plots in inches. Default is 7.

- plot_height:

  Height of plots in inches. Default is 5.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to plot QC for

Plots generated:

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

When `when = "pre"`, plots are prefixed with `qc_pre_` to distinguish
from post-QC plots. When `when = "post"` or `NULL`, plots use the
standard `qc_` prefix.

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- By default, include this step ONLY after
  [`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md).

- You MUST provide the when parameter to specify when the QC is being
  run.

## See also

[`glyclean::plot_missing_heatmap()`](https://glycoverse.github.io/glyclean/reference/plot_missing_heatmap.html),
[`glyclean::plot_tic_bar()`](https://glycoverse.github.io/glyclean/reference/plot_tic_bar.html),
and other glyclean plotting functions.

## Examples

``` r
step_plot_qc(when = "pre")
#> <step "step_plot_qc(when = \"pre\")"> QC (pre-preprocessing)
step_plot_qc(when = "post")
#> <step "step_plot_qc(when = \"post\")"> QC (post-preprocessing)
```
