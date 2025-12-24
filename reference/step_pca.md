# Step: Principal Component Analysis (PCA)

Run PCA using
[`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html)
and plot it with
[`glyvis::plot_pca()`](https://glycoverse.github.io/glyvis/reference/plot_pca.html).

## Usage

``` r
step_pca(...)
```

## Arguments

- ...:

  Step-specific arguments passed to underlying functions. Use the format
  `pkg.func.arg`. For example,
  `step_pca(glystats.gly_pca.center = FALSE)`.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to run PCA on

Tables generated:

- `pca_samples`: A table containing the PCA scores for each sample

- `pca_variables`: A table containing the PCA loadings for each variable

- `pca_eigenvalues`: A table containing the PCA eigenvalues

Plots generated:

- `pca`: A PCA plot colored by group

## Dynamic Arguments

This step supports the following dynamic arguments:

- `glystats.gly_pca.center`: Whether to center the data (default: TRUE).

- `glystats.gly_pca.scale`: Whether to scale the data (default: TRUE).

- `glyvis.plot_pca.type`: Plot type ("screeplot", "individual",
  "variables", "biplot").

- `glyvis.plot_pca.groups`: Group membership for coloring.

## See also

[`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html),
[`glyvis::plot_pca()`](https://glycoverse.github.io/glyvis/reference/plot_pca.html)

## Examples

``` r
step_pca()
#> <step "step_pca()"> Principal component analysis
```
