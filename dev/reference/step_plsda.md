# Step: Partial Least Squares Discriminant Analysis (PLS-DA)

Perform PLS-DA using
[`glystats::gly_plsda()`](https://glycoverse.github.io/glystats/reference/gly_plsda.html)
and plot it with
[`glyvis::plot_plsda()`](https://glycoverse.github.io/glyvis/reference/plot_plsda.html).
PLS-DA is a supervised method that finds components maximizing
covariance between predictors and the response variable (group
membership). The upstream step supplies the experiment.

## Usage

``` r
step_plsda(ncomp = 2, scale = TRUE, plot_width = 5, plot_height = 5, ...)
```

## Arguments

- ncomp:

  Number of components to include. Default is 2.

- scale:

  Logical indicating whether to scale the data. Default is TRUE.

- plot_width:

  Width of plots in inches. Default is 5.

- plot_height:

  Height of plots in inches. Default is 5.

- ...:

  Additional arguments passed to
  [`glystats::gly_plsda()`](https://glycoverse.github.io/glystats/reference/gly_plsda.html).

## Value

A `glysmith_step` object.

## See also

[`glystats::gly_plsda()`](https://glycoverse.github.io/glystats/reference/gly_plsda.html),
[`glyvis::plot_plsda()`](https://glycoverse.github.io/glyvis/reference/plot_plsda.html)

## Examples

``` r
step_plsda()
#> <step "step_plsda()"> Partial least squares discriminant analysis
step_plsda(ncomp = 3)
#> <step "step_plsda(ncomp = 3)"> Partial least squares discriminant analysis
```
