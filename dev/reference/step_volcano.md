# Step: Volcano Plot

Create a volcano plot from DEA results using
[`glyvis::plot_volcano()`](https://glycoverse.github.io/glyvis/reference/plot_volcano.html).

This step requires one of the DEA steps to be run:

- [`step_dea_limma()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_limma.md)
  (multi-group comparison is also supported)

- [`step_dea_ttest()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_ttest.md)

- [`step_dea_wilcox()`](https://glycoverse.github.io/glysmith/dev/reference/step_dea_wilcox.md)

## Usage

``` r
step_volcano(
  log2fc_cutoff = 1,
  p_cutoff = 0.05,
  p_col = "p_adj",
  plot_width = 5,
  plot_height = 6,
  ...
)
```

## Arguments

- log2fc_cutoff:

  The log2 fold change cutoff. Defaults to 1.

- p_cutoff:

  The p-value cutoff. Defaults to 0.05.

- p_col:

  The column name for p-value. Defaults to "p_adj". Can also be "p_val"
  (raw p-values without multiple testing correction).

- plot_width:

  Width of the plot in inches. Default is 5.

- plot_height:

  Height of the plot in inches. Default is 6.

- ...:

  Other arguments passed to
  [`EnhancedVolcano::EnhancedVolcano()`](https://rdrr.io/pkg/EnhancedVolcano/man/EnhancedVolcano.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- `dea_res`: The DEA results from
  [`glystats::gly_limma()`](https://glycoverse.github.io/glystats/reference/gly_limma.html)

Plots generated:

- `volcano`: A volcano plot

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md)
only.*

- Always include this step by default if DEA is performed, and the DEA
  method is not ANOVA or Kruskal-Wallis.

## See also

[`glyvis::plot_volcano()`](https://glycoverse.github.io/glyvis/reference/plot_volcano.html)

## Examples

``` r
step_volcano()
#> <step "step_volcano()"> Volcano plot
step_volcano(log2fc_cutoff = 2)
#> <step "step_volcano(log2fc_cutoff = 2)"> Volcano plot
```
