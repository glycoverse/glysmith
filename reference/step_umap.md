# Step: UMAP

Perform UMAP analysis using
[`glystats::gly_umap()`](https://glycoverse.github.io/glystats/reference/gly_umap.html)
and plot a UMAP plot using
[`glyvis::plot_umap()`](https://glycoverse.github.io/glyvis/reference/plot_umap.html).
Note that the result of UMAP largely depends on the `n_neighbors`
parameter. Usually it's a trial-and-error process to find the best value
iteratively. If you are not satisfied with the result, manually call
[`glyvis::plot_umap()`](https://glycoverse.github.io/glyvis/reference/plot_umap.html)
with different `n_neighbors` values to find the best one.

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
step_umap(
  on = "exp",
  n_neighbors = 15,
  n_components = 2,
  plot_width = 5,
  plot_height = 5,
  ...
)
```

## Arguments

- on:

  Name of the experiment to run UMAP on. Can be "exp", "sig_exp",
  "trait_exp", "sig_trait_exp", "dynamic_motif_exp",
  "sig_dynamic_motif_exp", "branch_motif_exp", "sig_branch_motif_exp".

- n_neighbors:

  Number of neighbors to consider for each point. Default is 15.

- n_components:

  Number of output dimensions. Default is 2.

- plot_width:

  Width of the plot in inches. Default is 5.

- plot_height:

  Height of the plot in inches. Default is 5.

- ...:

  Additional arguments passed to
  [`uwot::umap()`](https://jlmelville.github.io/uwot/reference/umap.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp` (if `on = "exp"`): The experiment to perform UMAP on

- `trait_exp` (if `on = "trait_exp"`): The trait experiment to perform
  UMAP on

- `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif
  experiment to perform UMAP on

- `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif
  experiment to perform UMAP on

Data generated (with suffixes):

- `umap`: The UMAP result

Plots generated (with suffixes):

- `umap`: The UMAP plot

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- Include this step only when the user explicitly asks for UMAP.

## See also

[`glystats::gly_umap()`](https://glycoverse.github.io/glystats/reference/gly_umap.html),
[`glyvis::plot_umap()`](https://glycoverse.github.io/glyvis/reference/plot_umap.html)

## Examples

``` r
step_umap()
#> <step "step_umap()"> UMAP
step_umap(n_neighbors = 15)
#> <step "step_umap(n_neighbors = 15)"> UMAP
```
