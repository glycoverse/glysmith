# Step: ROC Analysis

Perform ROC analysis using
[`glystats::gly_roc()`](https://glycoverse.github.io/glystats/reference/gly_roc.html),
extract top 10 variables with highest AUC, and plot ROC curves for these
variables using
[`glyvis::plot_roc()`](https://glycoverse.github.io/glyvis/reference/plot_roc.html).

## Usage

``` r
step_roc(...)
```

## Arguments

- ...:

  Step-specific arguments passed to
  [`glystats::gly_roc()`](https://glycoverse.github.io/glystats/reference/gly_roc.html)
  and
  [`glyvis::plot_roc()`](https://glycoverse.github.io/glyvis/reference/plot_roc.html).
  Use the format `pkg.func.arg`. For example,
  `step_roc(glystats.gly_roc.pos_class = "positive_class")`.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to perform ROC analysis on

Tables generated:

- `roc_auc`: A table containing the ROC AUC values for all variables

Plots generated:

- `roc_curves`: ROC curves for the top 10 variables

## Dynamic Arguments

This step supports the following dynamic arguments:

- `glystats.gly_roc.pos_class`: The positive class.

## See also

[`glystats::gly_roc()`](https://glycoverse.github.io/glystats/reference/gly_roc.html),
[`glyvis::plot_roc()`](https://glycoverse.github.io/glyvis/reference/plot_roc.html)

## Examples

``` r
step_roc()
#> <step "step_roc()"> ROC analysis
```
