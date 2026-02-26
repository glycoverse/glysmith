# Step: Partial Least Squares Discriminant Analysis (PLS-DA)

Perform PLS-DA using
[`glystats::gly_plsda()`](https://glycoverse.github.io/glystats/reference/gly_plsda.html)
and plot it with
[`glyvis::plot_plsda()`](https://glycoverse.github.io/glyvis/reference/plot_plsda.html).
PLS-DA is a supervised method that finds components maximizing
covariance between predictors and the response variable (group
membership).

## Usage

``` r
step_plsda(
  on = "exp",
  ncomp = 2,
  scale = TRUE,
  plot_width = 5,
  plot_height = 5,
  ...
)
```

## Arguments

- on:

  Name of the experiment to run PLS-DA on. Can be "exp", "sig_exp",
  "trait_exp", "sig_trait_exp", "dynamic_motif_exp",
  "sig_dynamic_motif_exp", "branch_motif_exp", "sig_branch_motif_exp".

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

## Details

Data required:

- `exp` (if `on = "exp"`): The experiment to run PLS-DA on

- `trait_exp` (if `on = "trait_exp"`): The trait experiment to run
  PLS-DA on

- `dynamic_motif_exp` (if `on = "dynamic_motif_exp"`): The dynamic motif
  experiment to run PLS-DA on

- `branch_motif_exp` (if `on = "branch_motif_exp"`): The branch motif
  experiment to run PLS-DA on

Tables generated (with suffixes):

- `plsda_samples`: A table containing the PLS-DA scores for each sample

- `plsda_variables`: A table containing the PLS-DA loadings for each
  variable

- `plsda_variance`: A table containing the explained variance for each
  component

- `plsda_vip`: A table containing the Variable Importance in Projection
  (VIP) scores

- `plsda_perm_test`: A table containing permutation test results

Plots generated (with suffixes):

- `plsda_scores`: A PLS-DA score plot colored by group

- `plsda_loadings`: A PLS-DA loading plot

- `plsda_variance`: A PLS-DA variance (scree) plot

- `plsda_vip`: A PLS-DA VIP score plot

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- Include this step when users explicitly asks for PLS-DA.

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
