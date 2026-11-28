# Step: Significant Variables Boxplot

Create boxplots for the most significant variables from DEA analysis
using
[`glyvis::plot_boxplot()`](https://glycoverse.github.io/glyvis/reference/plot_boxplot.html).
The function selects the top `n_top` variables with the lowest adjusted
p-values from the DEA results and plots their expression values grouped
by sample groups. The upstream step supplies the experiment.

## Usage

``` r
step_sig_boxplot(
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

## See also

[`glyvis::plot_boxplot()`](https://glycoverse.github.io/glyvis/reference/plot_boxplot.html)

## Examples

``` r
step_sig_boxplot()
#> <step "step_sig_boxplot()"> Significant variables boxplot
step_sig_boxplot(n_top = 12)
#> <step "step_sig_boxplot(n_top = 12)"> Significant variables boxplot
```
