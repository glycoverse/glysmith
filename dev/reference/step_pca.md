# Step: Principal Component Analysis (PCA)

Run PCA using
[`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html)
and plot it with
[`glyvis::plot_pca()`](https://glycoverse.github.io/glyvis/reference/plot_pca.html).
Loading plot for glycoproteomics data can be crowded with too many
variables. Ignore the resulting plot if it is not informative.

This step depends on the `on` parameter (default: `exp`).

- When `on = "exp"`, requires `exp` (usually after
  [`step_preprocess()`](https://glycoverse.github.io/glysmith/dev/reference/step_preprocess.md)).

- When `on = "sig_exp"`, requires `sig_exp` from one of
  [`step_dea_limma()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_limma.md),
  [`step_dea_ttest()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_ttest.md),
  [`step_dea_wilcox()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_wilcox.md),
  [`step_dea_anova()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_anova.md),
  or
  [`step_dea_kruskal()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_kruskal.md).

- When `on = "trait_exp"`, requires `trait_exp` from
  [`step_derive_traits()`](https://glycoverse.github.io/glysmith/dev/reference/step_derive_traits.md).

- When `on = "sig_trait_exp"`, requires `sig_trait_exp` from DEA on
  traits.

- When `on = "dynamic_motif_exp"`, requires `dynamic_motif_exp` from
  [`step_quantify_dynamic_motifs()`](https://glycoverse.github.io/glysmith/dev/reference/step_quantify_dynamic_motifs.md).

- When `on = "sig_dynamic_motif_exp"`, requires `sig_dynamic_motif_exp`
  from DEA on motifs.

- When `on = "branch_motif_exp"`, requires `branch_motif_exp` from
  [`step_quantify_branch_motifs()`](https://glycoverse.github.io/glysmith/dev/reference/step_quantify_branch_motifs.md).

- When `on = "sig_branch_motif_exp"`, requires `sig_branch_motif_exp`
  from DEA on motifs.

## Usage

``` r
step_pca(
  on = "exp",
  center = TRUE,
  scale = TRUE,
  loadings = FALSE,
  screeplot = TRUE,
  plot_width = 5,
  plot_height = 5,
  ...
)
```

## Arguments

- on:

  Name of the experiment to run PCA on. Can be "exp", "sig_exp",
  "trait_exp", "sig_trait_exp", "dynamic_motif_exp",
  "sig_dynamic_motif_exp", "branch_motif_exp", "sig_branch_motif_exp".

- center:

  A logical indicating whether to center the data. Default is TRUE.

- scale:

  A logical indicating whether to scale the data. Default is TRUE.

- loadings:

  Logical indicating whether to generate the loading plot. Default is
  `FALSE` since loading plots for glycoproteomics data can be crowded.

- screeplot:

  Logical indicating whether to generate the screeplot. Default is
  `TRUE`.

- plot_width:

  Width of plots in inches. Default is 5.

- plot_height:

  Height of plots in inches. Default is 5.

- ...:

  Additional arguments passed to
  [`prcomp()`](https://rdrr.io/r/stats/prcomp.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp` (if `on = "exp"`): The experiment to run PCA on

- `trait_exp` (if `on = "trait_exp"`): The trait experiment to run PCA
  on

- `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif
  experiment to run PCA on

- `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif
  experiment to run PCA on

Tables generated (with suffixes):

- `pca_samples`: A table containing the PCA scores for each sample

- `pca_variables`: A table containing the PCA loadings for each variable

- `pca_eigenvalues`: A table containing the PCA eigenvalues

Plots generated (with suffixes):

- `pca_scores`: A PCA score plot colored by group (always generated)

- `pca_loadings`: A PCA loading plot (if `loadings = TRUE`)

- `pca_screeplot`: A PCA screeplot (if `screeplot = TRUE`)

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md)
only.*

- Include this step if needed.

## See also

[`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html),
[`glyvis::plot_pca()`](https://glycoverse.github.io/glyvis/reference/plot_pca.html)

## Examples

``` r
step_pca()
#> <step "step_pca()"> Principal component analysis
```
