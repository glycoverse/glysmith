# Step: Heatmap

Create a heatmap plot using
[`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html).
The heatmap visualizes expression values across samples.

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
step_heatmap(on = "exp", plot_width = 7, plot_height = 7, ...)
```

## Arguments

- on:

  Name of the experiment data in `ctx$data` to plot. One of "exp",
  "sig_exp", "trait_exp", "sig_trait_exp", "dynamic_motif_exp",
  "sig_dynamic_motif_exp", "branch_motif_exp", "sig_branch_motif_exp".
  Default is "exp".

- plot_width:

  Width of the plot in inches. Default is 7.

- plot_height:

  Height of the plot in inches. Default is 7.

- ...:

  Additional arguments passed to
  [`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- Depends on `on` parameter (default: `exp`)

Plots generated:

- `heatmap`: A heatmap plot (if `on = "exp"`)

- `sig_heatmap`: A heatmap plot (if `on = "sig_exp"`)

- `trait_heatmap`: A heatmap plot (if `on = "trait_exp"`)

- `sig_trait_heatmap`: A heatmap plot (if `on = "sig_trait_exp"`)

- `dynamic_motif_heatmap`: A heatmap plot (if
  `on = "dynamic_motif_exp"`)

- `sig_dynamic_motif_heatmap`: A heatmap plot (if
  `on = "sig_dynamic_motif_exp"`)

- `branch_motif_heatmap`: A heatmap plot (if `on = "branch_motif_exp"`)

- `sig_branch_motif_heatmap`: A heatmap plot (if
  `on = "sig_branch_motif_exp"`)

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- Include this step if needed.

- It is recommended to use this step on significant results (e.g.
  `on = "sig_exp"`) if available.

## See also

[`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html)

## Examples

``` r
step_heatmap()
#> <step "step_heatmap()"> Heatmap
step_heatmap(on = "sig_exp")
#> <step "step_heatmap(on = \"sig_exp\")"> Heatmap of significant variables
step_heatmap(on = "trait_exp")
#> <step "step_heatmap(on = \"trait_exp\")"> Heatmap of traits
```
