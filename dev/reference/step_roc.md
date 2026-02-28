# Step: ROC Analysis

Perform ROC analysis using
[`glystats::gly_roc()`](https://glycoverse.github.io/glystats/reference/gly_roc.html),
extract top 10 variables with highest AUC, and plot ROC curves for these
variables using
[`glyvis::plot_roc()`](https://glycoverse.github.io/glyvis/reference/plot_roc.html).

This step requires `exp` (experiment data).

## Usage

``` r
step_roc(pos_class = NULL, plot_width = 5, plot_height = 5)
```

## Arguments

- pos_class:

  A character string specifying which group level should be treated as
  the positive class. If `NULL` (default), the second level
  (alphabetically) will be used as the positive class.

- plot_width:

  Width of the plot in inches. Default is 5.

- plot_height:

  Height of the plot in inches. Default is 5.

## Value

A `glysmith_step` object.

## Details

Data required:

- `exp`: The experiment to perform ROC analysis on

Tables generated:

- `roc_auc`: A table containing the ROC AUC values for all variables

Plots generated:

- `roc_curves`: ROC curves for the top 10 variables

## AI Prompt

*This section is for AI in
[`inquire_blueprint()`](https://glycoverse.github.io/glysmith/dev/reference/inquire_blueprint.md)
only.*

- Include this step if the user explicitly asks for ROC analysis, or if
  he/she mentions "biomarker(s)" in the prompt.

- If the experiment has more than 2 groups but the user wants a specific
  two-group comparison, ask which two groups to compare and include
  `step_subset_groups(groups = c("A", "B"))` before this step.

## See also

[`glystats::gly_roc()`](https://glycoverse.github.io/glystats/reference/gly_roc.html),
[`glyvis::plot_roc()`](https://glycoverse.github.io/glyvis/reference/plot_roc.html)

## Examples

``` r
step_roc()
#> <step "step_roc()"> ROC analysis
```
