# Step: t-SNE

Perform t-SNE analysis using
[`glystats::gly_tsne()`](https://glycoverse.github.io/glystats/reference/gly_tsne.html)
and plot a t-SNE plot using
[`glyvis::plot_tsne()`](https://glycoverse.github.io/glyvis/reference/plot_tsne.html).
Note that the result of t-SNE largely depends on the `perplexity`
parameter. Usually it's a trial-and-error process to find the best value
iteratively. If you are not satisfied with the result, manually call
[`glyvis::plot_tsne()`](https://glycoverse.github.io/glyvis/reference/plot_tsne.html)
with different `perplexity` values to find the best one. The upstream
step supplies the experiment.

## Usage

``` r
step_tsne(dims = 2, perplexity = 30, plot_width = 5, plot_height = 5, ...)
```

## Arguments

- dims:

  Number of output dimensions. Default is 2.

- perplexity:

  Perplexity parameter for t-SNE. Default is 30.

- plot_width:

  Width of the plot in inches. Default is 5.

- plot_height:

  Height of the plot in inches. Default is 5.

- ...:

  Additional arguments passed to
  [`Rtsne::Rtsne()`](https://rdrr.io/pkg/Rtsne/man/Rtsne.html).

## Value

A `glysmith_step` object.

## See also

[`glystats::gly_tsne()`](https://glycoverse.github.io/glystats/reference/gly_tsne.html),
[`glyvis::plot_tsne()`](https://glycoverse.github.io/glyvis/reference/plot_tsne.html)

## Examples

``` r
step_tsne()
#> <step "step_tsne()"> t-SNE
step_tsne(perplexity = 30)
#> <step "step_tsne(perplexity = 30)"> t-SNE
```
