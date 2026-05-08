# Perform the Whole Analysis Pipeline

This function performs a comprehensive analysis for group comparison.

## Usage

``` r
forge_analysis(exp, blueprint = blueprint_default(), group_col = "group")
```

## Arguments

- exp:

  A
  [`glyexp::experiment()`](https://glycoverse.github.io/glyexp/reference/experiment.html)
  object.

- blueprint:

  A `glysmith_blueprint` object. Default is
  [`blueprint_default()`](https://glycoverse.github.io/glysmith/reference/blueprint_default.md).

- group_col:

  Column name of group information in the sample information. Used for
  various analyses. Default is "group".

## Value

A `glysmith_result` object, with the following components:

- `exp`: the experiment after preprocessing.

- `plots`: a named list of ggplot objects.

- `tables`: a named list of tibbles.

- `meta`: a named list of metadata, containing:

  - `explanation`: a named character vector or list of explanations for
    each plot and table, with keys like `tables$summary` and
    `plots$pca`.

  - `steps`: a character vector of the steps of the analysis.

  - `log`: the messages and outputs from each step.

- `blueprint`: the blueprint used for the analysis.

## Examples

``` r
if (FALSE) { # \dontrun{
exp <- glyexp::real_experiment2
result <- forge_analysis(exp)
print(result)
} # }
```
