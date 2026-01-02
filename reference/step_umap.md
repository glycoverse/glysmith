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
step_umap(on = "exp", n_neighbors = 15, n_components = 2, ...)
```

## Arguments

- on:

  Name of the experiment to run UMAP on. Can be "exp", "sig_exp",
  "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".

- n_neighbors:

  Number of neighbors to consider for each point. Default is 15.

- n_components:

  Number of output dimensions. Default is 2.

- ...:

  Additional arguments passed to
  [`uwot::umap()`](https://jlmelville.github.io/uwot/reference/umap.html).

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp` (if `on = "exp"`): The experiment to perform UMAP on

- `trait_exp` (if `on = "trait_exp"`): The trait experiment to perform
  UMAP on

- `motif_exp` (if `on = "motif_exp"`): The motif experiment to perform
  UMAP on

Data generated (with suffixes):

- `umap`: The UMAP result

Plots generated (with suffixes):

- `umap`: The UMAP plot

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
