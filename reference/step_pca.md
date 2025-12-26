# Step: Principal Component Analysis (PCA)

Run PCA using
[`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html)
and plot it with
[`glyvis::plot_pca()`](https://glycoverse.github.io/glyvis/reference/plot_pca.html).
Loading plot for glycoproteomics data can be crowded with too many
variables. Ignore the resulting plot if it is not informative.

## Usage

``` r
step_pca(on = "exp", ...)
```

## Arguments

- on:

  Name of the experiment to run PCA on. Can be "exp", "sig_exp",
  "trait_exp", or "sig_trait_exp".

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

- `pca_scores`: A PCA score plot colored by group

- `pca_loadings`: A PCA loading plot

- `pca_screeplot`: A PCA screeplot

## Dynamic Arguments

This step supports the following dynamic arguments:

- `glystats.gly_pca.center`: Whether to center the data (default: TRUE).

- `glystats.gly_pca.scale`: Whether to scale the data (default: TRUE).

## See also

[`glystats::gly_pca()`](https://glycoverse.github.io/glystats/reference/gly_pca.html),
[`glyvis::plot_pca()`](https://glycoverse.github.io/glyvis/reference/plot_pca.html)

## Examples

``` r
step_pca()
#> <step "step_pca()"> Principal component analysis
```
