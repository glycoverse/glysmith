# Step: Correlation Analysis

Perform pairwise correlation analysis using
[`glystats::gly_cor()`](https://glycoverse.github.io/glystats/reference/gly_cor.html)
and visualize the correlation matrix using
[`glyvis::plot_corrplot()`](https://glycoverse.github.io/glyvis/reference/plot_corrplot.html).
This step calculates correlation coefficients and p-values for all pairs
of variables or samples. The upstream step supplies the experiment.

## Usage

``` r
step_correlation(
  on_cor = c("variable", "sample"),
  method = c("pearson", "spearman"),
  p_adj_method = "BH",
  plot_width = 7,
  plot_height = 7,
  ...
)
```

## Arguments

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

## See also

[`glystats::gly_cor()`](https://glycoverse.github.io/glystats/reference/gly_cor.html),
[`glyvis::plot_corrplot()`](https://glycoverse.github.io/glyvis/reference/plot_corrplot.html)

## Examples

``` r
step_correlation()
#> <step "step_correlation()"> Correlation analysis
step_correlation(on_cor = "sample", method = "spearman")
#> <step "step_correlation(on_cor = \"sample\", method = \"spearman\")">
#> Correlation analysis
```
