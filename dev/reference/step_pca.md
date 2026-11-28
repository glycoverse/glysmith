# Step: Principal Component Analysis (PCA)

Run PCA using
[`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html)
and plot it with
[`glyvis::plot_pca()`](https://glycoverse.github.io/glyvis/reference/plot_pca.html).
Loading plot for glycoproteomics data can be crowded with too many
variables. Ignore the resulting plot if it is not informative. The
upstream step supplies the experiment.

## Usage

``` r
step_pca(
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

## See also

[`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html),
[`glyvis::plot_pca()`](https://glycoverse.github.io/glyvis/reference/plot_pca.html)

## Examples

``` r
step_pca()
#> <step "step_pca()"> Principal component analysis
```
