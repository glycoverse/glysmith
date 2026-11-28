# Step: Orthogonal Partial Least Squares Discriminant Analysis (OPLS-DA)

Perform OPLS-DA using
[`glystats::gly_oplsda()`](https://glycoverse.github.io/glystats/reference/gly_oplsda.html)
and plot it with
[`glyvis::plot_oplsda()`](https://glycoverse.github.io/glyvis/reference/plot_oplsda.html).
OPLS-DA separates variation into predictive (related to group) and
orthogonal (unrelated) components. This step only works with binary
classification (exactly 2 groups). The upstream step supplies the
experiment.

## Usage

``` r
step_oplsda(
  pred_i = 1,
  ortho_i = NA,
  scale = TRUE,
  plot_width = 5,
  plot_height = 5,
  ...
)
```

## Arguments

- pred_i:

  Number of predictive components to include. Default is 1.

- ortho_i:

  Number of orthogonal components to include. Default is NA (automatic).

- scale:

  Logical indicating whether to scale the data. Default is TRUE.

- plot_width:

  Width of plots in inches. Default is 5.

- plot_height:

  Height of plots in inches. Default is 5.

- ...:

  Additional arguments passed to
  [`glystats::gly_oplsda()`](https://glycoverse.github.io/glystats/reference/gly_oplsda.html).

## Value

A `glysmith_step` object.

## See also

[`glystats::gly_oplsda()`](https://glycoverse.github.io/glystats/reference/gly_oplsda.html),
[`glyvis::plot_oplsda()`](https://glycoverse.github.io/glyvis/reference/plot_oplsda.html)

## Examples

``` r
step_oplsda()
#> <step "step_oplsda()"> Orthogonal partial least squares discriminant analysis
step_oplsda(pred_i = 1, ortho_i = 1)
#> <step "step_oplsda(pred_i = 1, ortho_i = 1)"> Orthogonal partial least squares
#> discriminant analysis
```
