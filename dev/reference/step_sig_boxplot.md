# Step: Significant Variables Boxplot

Create boxplots for the most significant variables from DEA analysis
using
[`glyvis::plot_boxplot()`](https://glycoverse.github.io/glyvis/reference/plot_boxplot.html).
The function selects the top `n_top` variables with the lowest adjusted
p-values from the DEA results and plots their expression values grouped
by sample groups.

This step depends on the `on` parameter (default: `sig_exp`).

- When `on = "sig_exp"`, requires `sig_exp` from one of
  [`step_dea_limma()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_limma.md),
  [`step_dea_ttest()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_ttest.md),
  [`step_dea_wilcox()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_wilcox.md),
  [`step_dea_anova()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_anova.md),
  or
  [`step_dea_kruskal()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_kruskal.md).

- When `on = "sig_trait_exp"`, requires `sig_trait_exp` from DEA on
  traits.

- When `on = "sig_dynamic_motif_exp"`, requires `sig_dynamic_motif_exp`
  from DEA on motifs.

- When `on = "sig_branch_motif_exp"`, requires `sig_branch_motif_exp`
  from DEA on motifs.

The number of variables is limited to a maximum of 25, as enforced by
[`glyvis::plot_boxplot()`](https://glycoverse.github.io/glyvis/reference/plot_boxplot.html).

## Usage

``` r
step_sig_boxplot(
  on = "sig_exp",
  n_top = 25,
  panel_width = 1.5,
  panel_height = 1.2,
  min_width = 5,
  min_height = 3,
  max_width = 14,
  max_height = 12,
  ...
)
```

## Arguments

- on:

  Name of the experiment data in `ctx$data` to plot. One of "sig_exp",
  "sig_trait_exp", "sig_dynamic_motif_exp", "sig_branch_motif_exp".
  Default is "sig_exp".

- n_top:

  Number of top significant variables to plot. Must be between 1 and 25
  (inclusive). Default is 25.

- panel_width:

  Width of each panel in inches. Default is 1.5.

- panel_height:

  Height of each panel in inches. Default is 1.2.

- min_width:

  Minimum plot width in inches. Default is 5.

- min_height:

  Minimum plot height in inches. Default is 3.

- max_width:

  Maximum plot width in inches. Default is 14.

- max_height:

  Maximum plot height in inches. Default is 12.

- ...:

  Additional arguments passed to
  [`glyvis::plot_boxplot()`](https://glycoverse.github.io/glyvis/reference/plot_boxplot.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- Depends on `on` parameter:

  - `sig_exp` (default): Significant experiment from DEA

  - `sig_trait_exp`: Significant trait experiment from DTA

  - `sig_dynamic_motif_exp`: Significant dynamic motif experiment from
    DMA

  - `sig_branch_motif_exp`: Significant branch motif experiment from DMA

Plots generated:

- `sig_boxplot`: A boxplot of significant variables (if
  `on = "sig_exp"`)

- `sig_trait_boxplot`: A boxplot of significant traits (if
  `on = "sig_trait_exp"`)

- `sig_dynamic_motif_boxplot`: A boxplot of significant dynamic motifs
  (if `on = "sig_dynamic_motif_exp"`)

- `sig_branch_motif_boxplot`: A boxplot of significant branch motifs (if
  `on = "sig_branch_motif_exp"`)

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md)
only.*

- Include this step after DEA steps to visualize the significant
  variables.

- This step is particularly useful for understanding the expression
  patterns of the most differentially expressed features across groups.

## See also

[`glyvis::plot_boxplot()`](https://glycoverse.github.io/glyvis/reference/plot_boxplot.html)

## Examples

``` r
step_sig_boxplot()
#> <step "step_sig_boxplot()"> Significant variables boxplot of significant
#> variables
step_sig_boxplot(n_top = 12)
#> <step "step_sig_boxplot(n_top = 12)"> Significant variables boxplot of
#> significant variables
step_sig_boxplot(on = "sig_trait_exp")
#> <step "step_sig_boxplot(on = \"sig_trait_exp\")"> Significant variables boxplot
#> of significant traits
```
