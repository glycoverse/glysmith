# Step: UMAP

Perform UMAP analysis using
[`glystats::gly_umap()`](https://glycoverse.github.io/glystats/reference/gly_umap.html)
and plot a UMAP plot using
[`glyvis::plot_umap()`](https://glycoverse.github.io/glyvis/reference/plot_umap.html).
Note that the result of UMAP largely depends on the `n_neighbors`
parameter. Usually it's a trial-and-error process to find the best value
iteratively. If you are not satisfied with the result, manually call
[`glyvis::plot_umap()`](https://glycoverse.github.io/glyvis/reference/plot_umap.html)
with different `n_neighbors` values to find the best one. The upstream
step supplies the experiment.

## Usage

``` r
step_umap(
  n_neighbors = 15,
  n_components = 2,
  plot_width = 5,
  plot_height = 5,
  ...
)
```

## Arguments

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
