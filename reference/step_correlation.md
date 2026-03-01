# Step: Correlation Analysis

Perform pairwise correlation analysis using
[`glystats::gly_cor()`](https://glycoverse.github.io/glystats/reference/gly_cor.html)
and visualize the correlation matrix using
[`glyvis::plot_corrplot()`](https://glycoverse.github.io/glyvis/reference/plot_corrplot.html).
This step calculates correlation coefficients and p-values for all pairs
of variables or samples.

This step depends on the `on` parameter (default: `exp`).

- When `on = "exp"`, requires `exp` (usually after
  [`step_preprocess()`](https://glycoverse.github.io/glysmith/reference/step_preprocess.md)).

- When `on = "sig_exp"`, requires `sig_exp` from one of
  [`step_dea_limma()`](https://glycoverse.github.io/glysmith/reference/step_dea_limma.md),
  [`step_dea_ttest()`](https://glycoverse.github.io/glysmith/reference/step_dea_ttest.md),
  [`step_dea_wilcox()`](https://glycoverse.github.io/glysmith/reference/step_dea_wilcox.md),
  [`step_dea_anova()`](https://glycoverse.github.io/glysmith/reference/step_dea_anova.md),
  or
  [`step_dea_kruskal()`](https://glycoverse.github.io/glysmith/reference/step_dea_kruskal.md).

- When `on = "trait_exp"`, requires `trait_exp` from
  [`step_derive_traits()`](https://glycoverse.github.io/glysmith/reference/step_derive_traits.md).

- When `on = "sig_trait_exp"`, requires `sig_trait_exp` from DEA on
  traits.

- When `on = "dynamic_motif_exp"`, requires `dynamic_motif_exp` from
  [`step_quantify_dynamic_motifs()`](https://glycoverse.github.io/glysmith/reference/step_quantify_dynamic_motifs.md).

- When `on = "sig_dynamic_motif_exp"`, requires `sig_dynamic_motif_exp`
  from DEA on motifs.

- When `on = "branch_motif_exp"`, requires `branch_motif_exp` from
  [`step_quantify_branch_motifs()`](https://glycoverse.github.io/glysmith/reference/step_quantify_branch_motifs.md).

- When `on = "sig_branch_motif_exp"`, requires `sig_branch_motif_exp`
  from DEA on motifs.

## Usage

``` r
step_correlation(
  on = "exp",
  on_cor = c("variable", "sample"),
  method = c("pearson", "spearman"),
  p_adj_method = "BH",
  plot_width = 7,
  plot_height = 7,
  ...
)
```

## Arguments

- on:

  Name of the experiment to run correlation analysis on. Can be "exp",
  "sig_exp", "trait_exp", "sig_trait_exp", "dynamic_motif_exp",
  "sig_dynamic_motif_exp", "branch_motif_exp", "sig_branch_motif_exp".

- on_cor:

  A character string specifying what to correlate. Either "variable"
  (default) to correlate variables/features, or "sample" to correlate
  samples.

- method:

  A character string indicating which correlation coefficient is to be
  computed. One of "pearson" (default) or "spearman".

- p_adj_method:

  A character string specifying the method to adjust p-values. See
  `p.adjust.methods` for available methods. Default is "BH". If NULL, no
  adjustment is performed.

- plot_width:

  Width of the plot in inches. Default is 7.

- plot_height:

  Height of the plot in inches. Default is 7.

- ...:

  Additional arguments passed to
  [`glystats::gly_cor()`](https://glycoverse.github.io/glystats/reference/gly_cor.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp` (if `on = "exp"`): The experiment to run correlation analysis on

- `trait_exp` (if `on = "trait_exp"`): The trait experiment to run
  correlation analysis on

- `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif
  experiment to run correlation analysis on

- `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif
  experiment to run correlation analysis on

Tables generated (with suffixes):

- `correlation`: A table containing pairwise correlation results with
  columns:

  - `variable1`, `variable2` (or `sample1`, `sample2` if
    `on = "sample"`)

  - `cor`: Correlation coefficient

  - `p_val`: P-value from correlation test

  - `p_adj`: Adjusted p-value (if p_adj_method is not NULL)

Plots generated (with suffixes):

- `correlation`: A correlation matrix heatmap

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- Include this step to explore relationships between variables or
  samples.

- Be careful to use when sample size or variable number is large (\>
  50). Before using this step for large data, ask the user if they want
  to proceed.

## See also

[`glystats::gly_cor()`](https://glycoverse.github.io/glystats/reference/gly_cor.html),
[`glyvis::plot_corrplot()`](https://glycoverse.github.io/glyvis/reference/plot_corrplot.html)

## Examples

``` r
step_correlation()
#> <step "step_correlation()"> Correlation analysis
step_correlation(on = "sig_exp")
#> <step "step_correlation(on = \"sig_exp\")"> Correlation analysis of significant
#> variables
step_correlation(on_cor = "sample", method = "spearman")
#> <step "step_correlation(on_cor = \"sample\", method = \"spearman\")">
#> Correlation analysis
```
