# Step: Orthogonal Partial Least Squares Discriminant Analysis (OPLS-DA)

Perform OPLS-DA using
[`glystats::gly_oplsda()`](https://glycoverse.github.io/glystats/reference/gly_oplsda.html)
and plot it with
[`glyvis::plot_oplsda()`](https://glycoverse.github.io/glyvis/reference/plot_oplsda.html).
OPLS-DA separates variation into predictive (related to group) and
orthogonal (unrelated) components. This step only works with binary
classification (exactly 2 groups).

## Usage

``` r
step_oplsda(
  on = "exp",
  pred_i = 1,
  ortho_i = NA,
  scale = TRUE,
  plot_width = 5,
  plot_height = 5,
  ...
)
```

## Arguments

- on:

  Name of the experiment to run OPLS-DA on. Can be "exp", "sig_exp",
  "trait_exp", "sig_trait_exp", "motif_exp", "sig_motif_exp".

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

## Details

Data required:

- `exp` (if `on = "exp"`): The experiment to run OPLS-DA on

- `trait_exp` (if `on = "trait_exp"`): The trait experiment to run
  OPLS-DA on

- `motif_exp` (if `on = "motif_exp"`): The motif experiment to run
  OPLS-DA on

Tables generated (with suffixes):

- `oplsda_samples`: A table containing the OPLS-DA scores for each
  sample

- `oplsda_variables`: A table containing the OPLS-DA loadings for each
  variable

- `oplsda_variance`: A table containing the explained variance for each
  component

- `oplsda_vip`: A table containing the Variable Importance in Projection
  (VIP) scores

- `oplsda_perm_test`: A table containing permutation test results

Plots generated (with suffixes):

- `oplsda_scores`: An OPLS-DA score plot colored by group

- `oplsda_loadings`: An OPLS-DA loading plot

- `oplsda_variance`: An OPLS-DA variance (scree) plot

- `oplsda_vip`: An OPLS-DA VIP score plot

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/reference/inquire_blueprint.md)
only.*

- Include this step when users explicitly asks for OPLS-DA.

- This step only works with binary classification (exactly 2 groups). If
  multiple groups are found, ask if
  [`step_subset_groups()`](https://glycoverse.github.io/glysmith/reference/step_subset_groups.md)
  should be run first.

## See also

[`glystats::gly_oplsda()`](https://glycoverse.github.io/glystats/reference/gly_oplsda.html),
[`glyvis::plot_oplsda()`](https://glycoverse.github.io/glyvis/reference/plot_oplsda.html)

## Examples

``` r
step_oplsda()
#> Error in step_oplsda(): The package "ropls" is required.
step_oplsda(pred_i = 1, ortho_i = 1)
#> Error in step_oplsda(pred_i = 1, ortho_i = 1): The package "ropls" is required.
```
