# Step: t-SNE

Perform t-SNE analysis using
[`glystats::gly_tsne()`](https://glycoverse.github.io/glystats/reference/gly_tsne.html)
and plot a t-SNE plot using
[`glyvis::plot_tsne()`](https://glycoverse.github.io/glyvis/reference/plot_tsne.html).
Note that the result of t-SNE largely depends on the `perplexity`
parameter. Usually it's a trial-and-error process to find the best value
iteratively. If you are not satisfied with the result, manually call
[`glyvis::plot_tsne()`](https://glycoverse.github.io/glyvis/reference/plot_tsne.html)
with different `perplexity` values to find the best one.

## Usage

``` r
step_tsne(on = "exp", dims = 2, perplexity = 30, ...)
```

## Arguments

- on:

  Name of the experiment to run t-SNE on. Can be "exp", "sig_exp",
  "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".

- dims:

  Number of output dimensions. Default is 2.

- perplexity:

  Perplexity parameter for t-SNE. Default is 30.

- ...:

  Additional arguments passed to
  [`Rtsne::Rtsne()`](https://rdrr.io/pkg/Rtsne/man/Rtsne.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp` (if `on = "exp"`): The experiment to perform t-SNE on

- `trait_exp` (if `on = "trait_exp"`): The trait experiment to perform
  t-SNE on

- `motif_exp` (if `on = "motif_exp"`): The motif experiment to perform
  t-SNE on

Data generated (with suffixes):

- `tsne`: The t-SNE result

Plots generated (with suffixes):

- `tsne`: The t-SNE plot

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
