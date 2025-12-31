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
  "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".

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

- `exp` (if `on = "exp"`): The experiment to perform UMAP on

- `trait_exp` (if `on = "trait_exp"`): The trait experiment to perform
  UMAP on

- `motif_exp` (if `on = "motif_exp"`): The motif experiment to perform
  UMAP on

Data generated (with suffixes):

- `umap`: The UMAP result

Plots generated (with suffixes):

- `umap`: The UMAP plot

## Dynamic Arguments

This step supports the following dynamic arguments:

- `glystats.gly_umap.n_neighbors`: The number of neighbors.

- `glystats.gly_umap.n_components`: The number of dimensions.

- `glystats.gly_umap.xxx`: xxx are other parameters of
  [`uwot::umap()`](https://jlmelville.github.io/uwot/reference/umap.html).

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
