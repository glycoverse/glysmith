# Perform the Whole Analysis Pipeline

This function performs a comprehensive analysis for group comparison.

## Usage

``` r
forge_analysis(exp, blueprint = blueprint_default(), group_col = "group")
```

## Arguments

- exp:

  A
  [`glyexp::GlycomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycomicSE.html)
  or
  [`glyexp::GlycoproteomicSE()`](https://glycoverse.github.io/glyexp/reference/GlycoproteomicSE.html)
  object.

- blueprint:

  A `glysmith_blueprint` object. Default is
  [`blueprint_default()`](https://glycoverse.github.io/glysmith/dev/reference/blueprint_default.md).

- group_col:

  Column name of group information in the sample information. Used for
  various analyses. Default is "group".

## Value

A `glysmith_result` object, with the following components:

- `exp`: the input experiment for multiple entry routes. A single route
  returns its last main-route experiment, or NULL if its producer did
  not succeed.

- `data`: main outputs and auxiliary objects. Node IDs identify main
  outputs and prefix auxiliary object, plot and table names.

- `plots`: a named list of ggplot objects.

- `tables`: a named list of tibbles.

- `meta`: a named list of metadata, containing:

  - `explanation`: a named character vector or list of explanations for
    each plot and table, with keys like `tables$node_1__summary`.

  - `steps`: a character vector of the steps of the analysis.

  - `nodes`: each step's producer, route, status and reason.

  - `logs`: the messages and outputs from each step.

- `blueprint`: the blueprint used for the analysis.

## Examples

``` r
if (FALSE) { # \dontrun{
exp <- glyexp::real_experiment2
result <- forge_analysis(exp)
print(result)
} # }
```
