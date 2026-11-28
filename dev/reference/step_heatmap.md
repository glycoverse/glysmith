# Step: Heatmap

Create a heatmap plot using
[`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html).
The heatmap visualizes expression values across samples. The upstream
step supplies the experiment.

## Usage

``` r
step_heatmap(plot_width = 7, plot_height = 7, ...)
```

## Arguments

- plot_width:

  Width of the plot in inches. Default is 7.

- plot_height:

  Height of the plot in inches. Default is 7.

- ...:

  Additional arguments passed to
  [`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html).

## Value

A `glysmith_step` object.

## See also

[`glyvis::plot_heatmap()`](https://glycoverse.github.io/glyvis/reference/plot_heatmap.html)

## Examples

``` r
step_heatmap()
#> <step "step_heatmap()"> Heatmap
```
