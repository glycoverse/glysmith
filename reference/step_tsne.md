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
step_tsne(on = "exp", ...)
```

## Arguments

- on:

  Name of the experiment to run t-SNE on. Can be "exp", "sig_exp",
  "trait_exp", or "sig_trait_exp".

- ...:

  Step-specific arguments passed to
  [`glystats::gly_tsne()`](https://glycoverse.github.io/glystats/reference/gly_tsne.html)
  and
  [`glyvis::plot_tsne()`](https://glycoverse.github.io/glyvis/reference/plot_tsne.html).
  Use the format `pkg.func.arg`. For example,
  `step_tsne(glystats.gly_tsne.perplexity = 30)`.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to perform t-SNE on

Data generated:

- `tsne`: The t-SNE result

Plots generated:

- `tsne`: The t-SNE plot

## Dynamic Arguments

This step supports the following dynamic arguments:

- `glystats.gly_tsne.perplexity`: The perplexity parameter for t-SNE.

- `glystats.gly_tsne.dims`: The number of dimensions for t-SNE.

- `glystats.gly_tsne.xxx`: xxx are other parameters of `Rtsne::Rtsne()`.

## See also

[`glystats::gly_tsne()`](https://glycoverse.github.io/glystats/reference/gly_tsne.html),
[`glyvis::plot_tsne()`](https://glycoverse.github.io/glyvis/reference/plot_tsne.html)

## Examples

``` r
step_tsne()
#> <step "step_tsne()"> t-SNE
step_tsne(glystats.gly_tsne.perplexity = 30)
#> <step "step_tsne(glystats.gly_tsne.perplexity = 30)"> t-SNE
```
