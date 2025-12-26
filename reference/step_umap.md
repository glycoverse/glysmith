# Step: UMAP

Perform UMAP analysis using
[`glystats::gly_umap()`](https://glycoverse.github.io/glystats/reference/gly_umap.html)
and plot a UMAP plot using
[`glyvis::plot_umap()`](https://glycoverse.github.io/glyvis/reference/plot_umap.html).
Note that the result of UMAP largely depends on the `n_neighbors`
parameter. Usually it's a trial-and-error process to find the best value
iteratively. If you are not satisfied with the result, manually call
[`glyvis::plot_umap()`](https://glycoverse.github.io/glyvis/reference/plot_umap.html)
with different `n_neighbors` values to find the best one.

## Usage

``` r
step_umap(on = "exp", ...)
```

## Arguments

- on:

  Name of the experiment to run UMAP on. Can be "exp", "sig_exp",
  "trait_exp", or "sig_trait_exp".

- ...:

  Step-specific arguments passed to
  [`glystats::gly_umap()`](https://glycoverse.github.io/glystats/reference/gly_umap.html)
  and
  [`glyvis::plot_umap()`](https://glycoverse.github.io/glyvis/reference/plot_umap.html).
  Use the format `pkg.func.arg`. For example,
  `step_umap(glystats.gly_umap.n_neighbors = 15)`.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to perform UMAP on

Data generated:

- `umap`: The UMAP result

Plots generated:

- `umap`: The UMAP plot

## Dynamic Arguments

This step supports the following dynamic arguments:

- `glystats.gly_umap.n_neighbors`: The number of neighbors.

- `glystats.gly_umap.n_components`: The number of dimensions.

- `glystats.gly_umap.xxx`: xxx are other parameters of `uwot::umap()`.

## See also

[`glystats::gly_umap()`](https://glycoverse.github.io/glystats/reference/gly_umap.html),
[`glyvis::plot_umap()`](https://glycoverse.github.io/glyvis/reference/plot_umap.html)

## Examples

``` r
step_umap()
#> <step "step_umap()"> UMAP
step_umap(glystats.gly_umap.n_neighbors = 15)
#> <step "step_umap(glystats.gly_umap.n_neighbors = 15)"> UMAP
```
